### EGAJI v0.01 ###

# Librerías ----

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet.minicharts)
library(leaflet)
library(sf)

# Lectura ----

## Datos licencias ----
licencias <- readRDS("data/licencias_100.rds")

## Mapa ----

### Mapa todo México ----

mapa <- st_read("data/maps/Muni_2012gw.shp")

### Mapa Mun Ags ----
mun_ags <- mapa %>% 
  filter(CVE_ENT == "01" & CVE_MUN == "001")

# UI ----

ui <- navbarPage(title = "EGAJI", 
                 id = "nav",
                 footer = includeHTML("footer.html"),
                 fluid = TRUE,
                 collapsible = TRUE,
                 
                ## Tab panel - Inicio ----
                 
                 tabPanel("Inicio",
                          includeHTML("home.html")
                          )
                 ,
                 
                 ## Tab panel - Mapa ----
                 
                 tabPanel("Mapa interactivo",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                          ### Panel dinámico ----
                              
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          
                                          h2("Información de los comercios"),
                                          
                                          #### Slider para seleccionar pagos -----
                                          
                                          sliderInput(
                                            "numero_pagos_slide",
                                            "Número de pagos",
                                            min(licencias$numero_pagos, na.rm = TRUE),
                                            max(licencias$numero_pagos, na.rm = TRUE),
                                            value = range(licencias$numero_pagos, na.rm = FALSE),
                                            step = 1
                                          ),
                                          
                                          #### Seleccionador tipo licencia ----

                                          pickerInput(
                                            "Tipo",
                                            "Tipo de licencia",
                                            choices = unique(licencias$tipo_licencia),
                                            selected = c("Normal"),
                                            multiple = T,
                                            options = list(`actions-box` = TRUE)
                                          ),
                                          
                                          #### Seleccionador giros ----
                                          pickerInput(
                                            "Giro",
                                            "Giro",
                                            choices = sort(unique(licencias$nombre_de_giro)),
                                            selected = c("Abarrotes al por menor"),
                                            multiple = T,
                                            options = list(`actions-box` = TRUE)
                                          ),
                                          
                                          #### Histograma giros ----
                                          plotOutput("histComercios", height = 200)
                                        )
                                      )
                                    ),
                # Tab panel - Trámite ----------------------------------------
                tabPanel("Tramita tu licencia",
                         includeHTML("tramite.html"), # Hasta aqui todo funcionaba xd
                         
                         titlePanel("Trámite/Renovación de licencia"),
                         
                         ## Sidebar layout with input and output definitions ----
                         sidebarLayout(
                           
                           ### Sidebar panel for inputs ----
                           sidebarPanel(
                             
                             #### Text input ----,
                             textInput("caption", "Captura tu número de licencia", "Ej. 131027"),
                             verbatimTextOutput("value"),
                             
                             
                             #### Horizontal line ----
                             tags$hr(),
                             
                             #### File input ----
                             fileInput("file1", "Carga tus documentos",
                                       multiple = FALSE,
                                       buttonLabel = "Seleccionar...",
                                       placeholder = "Ningún documento seleccionado",
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
                      ),
                      mainPanel(
                        
                        #### Output: Data file ----
                        tableOutput("contents")
                        
                      )
                    )
                  )
)

### Server ----
server <- function(input, output, session) {
  
  #### Filtrado de los inputs checkboxes y deslizador ----

  filter_type <- reactive({licencias %>%
      filter(tipo_licencia %in% input$Tipo & nombre_de_giro %in% input$Giro)})  
  
  observeEvent(input$Tipo, {
    updateSliderInput(
      session = session,
      inputId = "numero_pagos_slide", # range para que sirva
      min = min(filter_type()$numero_pagos),
      max = max(filter_type()$numero_pagos),
      value = range(filter_type()$numero_pagos, na.rm = FALSE)
    )
  })
  
  filter_range <- reactive({
    filter_type() %>% 
      filter(numero_pagos >= input$numero_pagos_slide[1]) %>% 
      filter(numero_pagos <= input$numero_pagos_slide[2])
  })
  
  #### Histograma de giros ----
  
  # Comercios enfocados en el mapa
  comercios_enfocados <- reactive({ #zipsInBounds
    if (is.null(input$mymap_bounds))
      return(licencias[FALSE, ])
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(licencias,
           latitud >= latRng[1] & latitud <= latRng[2] &
             longitud >= lngRng[1] & longitud <= lngRng[2] &
             nombre_de_giro %in% input$Giro)
  })
  
  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histComercios <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(comercios_enfocados()) == 0)
      return(NULL)
    
    # hist(as.factor(comercios_enfocados()$nombre_de_giro),
    #      breaks = 20,
    #      main = "SuperZIP score (visible zips)",
    #      xlab = "Percentile",
    #      col = '#00DD00',
    #      border = 'white')
    print(ggplot(data = comercios_enfocados(), aes(x = nombre_de_giro, fill = nombre_de_giro)) +
      geom_histogram(stat = "count", color = "red", alpha = 0.5) +
      scale_fill_discrete(name = "Giro",
                          labels = function(x) str_wrap(x, width = 10)) +
      theme_minimal() +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        labs(title = "Número de comercios en la zona actual",
             x = "",
             y = "Cantidad"))
  })

  
  #### Renderizado de mapa ----

  output$mymap <- renderLeaflet({
    leaflet(mun_ags) %>%
      addTiles() %>%
      addPolygons() %>%
      addMarkers(data = filter_range(), 
                 lat = ~latitud, 
                 lng = ~longitud,  
                 clusterOptions = markerClusterOptions(),
                 popup = ~popup_info,
                 label = ~nombre_de_giro)
  })
  
}

# Shinyapp ----
shinyApp(ui = ui, server = server)
