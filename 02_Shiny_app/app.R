library(shiny)
library(bslib)
library(leaflet)


# Define UI for application that draws a histogram
ui <- page_navbar(
    
    #titulo
    title="Dashboards geoespacial ",
    
    #Sidebar compartido 
    sidebar=sidebar(
        "Selecciona variables de interés",
        
        #Selector por estado de la republica 
        selectInput("select_estado",
                    label="Selector de Estados",
                    choices = estados_mx, 
                    multiple = TRUE, 
                    selected = NULL ), 
    
        hr(),
        
        #Espacio de descargas 
        downloadButton("downloadData", "Descarga datos")
        ),
     
    
    #Ventana del mapa 
    nav_panel(
        title = "Mapa ",
        leafletOutput("mapa", height = "calc(100vh - 120px)")
    ),
    
    
    #Ventana de información complementaria 
    nav_panel(
        title = "Datos e información complemetaria",
        card(plotOutput("boxplot", height = "200px"))),
    
    
    footer = div(
        style = "background-color:#000000; color:white; text-align:center; 
                 padding:8px; font-size:12px; width:100%;",
        "Fuente: Proyección de precipitación SSP1-2.6. "),

    nav_item(input_dark_mode())
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
   
    #Paleta de color continua para precipitación
    pal <- colorNumeric(
        palette = "cividis",  
        domain = malla_precip_mm_anual$precipitacion_mm_anio 
    )
    
    
    #HTML para el POPUP 
    
    #Añadimos el mapa 
    output$mapa<-renderLeaflet({
        
        #Preparamos el mapa para representar en base a lo filtrado 
        malla_representacion <- malla_precip_mm_anual |>
            filter(
                if (is.null(input$select_estado)) TRUE else estado %in% input$select_estado,
                if (is.null(input$select_tipo))   TRUE else tipo   %in% input$select_tipo
            )
        
        
        leaflet()|>
            addTiles()|>
            setView(lng = -102.5528, lat = 23.6345, zoom = 5)|>#Mantenemos zoom en mexico 
            addPolygons(data=malla_representacion[malla_representacion$estado %in% input$select_estado, ],
                        fillColor = ~pal(precipitacion_mm_anio),
                        fillOpacity = 0.78,
                        stroke = FALSE,
                        #color = "black",
                        #weight = 0.1,
                        popup = paste0("<b>HEX_ID:</b> ", malla_representacion$hex_id, "<br>",
                                       "<b>ESTADO:</b> ", malla_representacion$estado, "<br>",
                                       "<b>Tipo:</b> ", malla_representacion$tipo, "<br>",
                                       "<b>Valor:</b> ", round(malla_representacion$precipitacion_mm_anio,digits = 2), " mm"
                        ),
                        
                        )|>
            addLegend(position = "bottomright",
                      pal = pal,
                      values = malla_representacion$precipitacion_mm_anio,
                      title = "Precipitación (mm/año)")

    })
    
    
    output$boxplot <- renderPlot({
        
        datos_plot <- malla_precip_mm_anual |>
            mutate(seleccionado = estado %in% input$select_estado)
        
        ggplot(data = datos_plot) +
            geom_boxplot(aes(x = reorder(estado, precipitacion_mm_anio, FUN = median), 
                             y = precipitacion_mm_anio,
                             fill = seleccionado)) +
            scale_fill_manual(values = c("TRUE" = "#F9A825", "FALSE" = "grey80"),
                              guide = "none") +  # oculta la leyenda TRUE/FALSE
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            labs(x = "Estado", y = "Precipitación (mm/año)")
        
    })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)

