library(shiny)
library(bslib)
library(leaflet)



# Define UI for application that draws a histogram
ui <- page_sidebar(
    #Funcionamiento 
    
    #titulo
    title="Dashboard",
    
    #Sidebar 
    
    sidebar=sidebar(
        "El cambio climático tiene efectos directos en el territorio nacional. 
        Las variaciones en la precipitación son relevantes para la seguridad de
        la población",
        

        #Selector por estado de la republica 
        selectInput("select_estado",
                    label="Selector de Estados",
                    choices = estados_mx, 
                    multiple = TRUE, 
                    selected = NULL ), 
        
        #Selector por tipo
        selectInput("select_tipo",
                    label="Selector de tipo/zona",
                    choices = zona_hex)

        
         ),
     
        leafletOutput("mapa")
    
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Paleta de color continua para precipitación
    pal <- colorNumeric(
        palette = "viridis",   # o "Blues", "viridis", etc.
        domain = malla_precip_mm_anual$precipitacion_mm_anio  # tu columna numérica
    )
    
    #Añadimos el mapa 
    output$mapa<-renderLeaflet({
        leaflet()|>
            addTiles()|>
            setView(lng = -102.5528, lat = 23.6345, zoom = 5)|>#Mantenemos zoom en mexico 
            addPolygons(data=malla_precip_mm_anual[malla_precip_mm_anual$estado %in% input$select_estado, ],
                        fillColor = ~pal(precipitacion_mm_anio),
                        fillOpacity = 0.8,
                        stroke = FALSE,
                        weight = 0.1,
                        color= "white"
                        )|>
            addLegend(position = "bottomright",
                      pal = pal,
                      values = malla_precip_mm_anual$precipitacion_mm_anio,
                      title = "Precipitación (mm/año)")|>
            addPopups(popup = malla_precip_mm_anual[malla_precip_mm_anual$estado %in% input$select_estado, ],)
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

