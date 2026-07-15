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
                    choices = estados_mx), 
        
        #Selector por tipo
        selectInput("select_tipo",
                    label="Selector de tipo/zona",
                    choices = zona_hex)
        
        
        
         ),
     
        leafletOutput("mapa")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Añadimos el mapa 
    output$mapa<-renderLeaflet({
        leaflet()|>
            addTiles()|>
            setView(lng = -102.5528, lat = 23.6345, zoom = 5)|>#Mantenemos zoom en mexico 
            addPolygons(data=malla_precip_mm_anual[malla_precip_mm_anual$estado==input$select_estado, ])
        
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

