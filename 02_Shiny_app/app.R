library(shiny)
library(bslib)
library(leaflet)
library(DT)


# Define UI for application that draws a histogram
ui <- page_navbar(
    
    #titulo
    title= "Dashboards geoespacial",
    
    #Sidebar compartido 
    sidebar=sidebar(
        paste0(meta$descripcion),
        
        #Selector por estado de la republica 
        selectInput("select_estado",
                    label="Selector de Estados",
                    choices = estados_mx, 
                    multiple = TRUE, 
                    selected = NULL ), 
    
        hr(),
        
        card(
            
            card_header("Acerca de estos datos"),
            card_body(
                style = "font-size: 10px;",
                tags$dl(
                    tags$dt("Descripción"),
                    tags$dd(meta$descripcion),
                    tags$dt("Fuente"),
                    tags$dd(meta$fuente),
                    tags$dt("Unidades"),
                    tags$dd(meta$unidades)
                )
            )
        ),
        
        #Espacio de descargas 
        downloadButton("downloadData", "Descarga datos")),
     
    
    #Ventana del mapa 
    nav_panel(
        title = "Mapa ",
        leafletOutput("mapa", height = "calc(100vh - 120px)")
    ),
    
    
    #Ventana de información complementaria 
    
    nav_panel(
      title = "Datos e información complementaria",
      
      style = "overflow-y: auto; height: 100vh;",
      
      layout_columns(
        card(
          card_header("Boxplot por estado"),
          card_body(plotOutput("boxplot", height = "300px"))
        ),
        card(
          card_header("Datos por estado"),
          card_body(tableOutput("mean_precip_table"))
        )
      ) 
      
      
      #Se puede añadir un histograma u otra grafica de analisis 
      
      # card(
      #   card_header("Histograma datos")
      #   card_body(plotOutput("histograma"))
      # )
      # 
      
      
    ),
    
    #Ventana del mapa 
    nav_panel(
      title = "Datos proyecto ",
      card(
        card_header("Datos de proyectos"),
        card_body(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi ut leo in ipsum finibus tincidunt. Duis sit amet dolor at lectus suscipit scelerisque. Maecenas quis eros id augue efficitur pulvinar. Morbi id odio tortor. Fusce volutpat id sem a gravida. Ut vehicula purus vitae tellus imperdiet pulvinar. Mauris eget rhoncus lorem. Praesent sollicitudin laoreet ipsum, et posuere enim feugiat et. Quisque lacus arcu, accumsan eu rhoncus id, vestibulum vitae ante. Pellentesque facilisis diam ut porta aliquam. In ut justo ligula. Duis nec odio in elit hendrerit consequat in in dolor. In in risus accumsan, dignissim elit a, auctor erat. Proin orci ex, sodales ac sapien eu, porttitor ultrices enim.
                  "),p("Nulla sit amet interdum augue. Proin ac efficitur metus. Pellentesque sollicitudin dolor et nisl posuere dictum. Phasellus vel mi tristique, ultrices libero et, dignissim neque. Nulla consequat nulla a lorem ultricies condimentum sollicitudin a ante. Aliquam vel lacus a arcu hendrerit sollicitudin et at urna. Etiam porta sapien enim, in consectetur nisi rhoncus ac. Fusce gravida mi a sem lacinia volutpat. Curabitur commodo euismod tortor sed condimentum. Phasellus id posuere nulla. Etiam blandit odio sit amet nisi mollis, eget molestie enim interdum. Praesent accumsan ex vitae augue tincidunt, ac pretium eros blandit. Phasellus tortor nibh, ornare eget sem non, accumsan fringilla lacus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Fusce vel congue lectus, quis dapibus lacus. "))
    )
    ),
    
    
    footer = div(
        style = "background-color:#000000; color:white; text-align:center; 
                 padding:8px; font-size:12px; width:100%;",
        "Responsable academido: xxxx@unam.mx  ||   Proyecto: XXX-XXX-SEHICTY  ||  Mantenimiento: lalejandroavc@gmail.com "),

    nav_item(input_dark_mode())
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #mapa ---------------------
    #Paleta de color continua para precipitación
    pal <- colorNumeric(
        palette = "cividis",  
        domain = malla_precip_mm_anual$precipitacion_mm_anio 
    )
    
    
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
                      
                        stroke = TRUE,
                        color = ~pal(precipitacion_mm_anio),
                        weight = 1,
                        smoothFactor = 0,                        
                        
                      
                        popup = paste0("<b>HEX_ID:</b> ", malla_representacion$hex_id, "<br>",
                                       "<b>ESTADO:</b> ", malla_representacion$estado, "<br>",
                                       "<b>Tipo:</b> ", malla_representacion$tipo, "<br>",
                                       "<b>Valor:</b> ", round(malla_representacion$precipitacion_mm_anio,digits = 2), " mm/año"
                        ),
                        
                        )|>
            addLegend(position = "bottomright",
                      pal = pal,
                      values = malla_representacion$precipitacion_mm_anio,
                      title = "Precipitación (mm/año)")

    })
    
    #boxplot-----------------------------------
    
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
    
    
    #histograma----------------------------
    
    output$histograma <- renderPlot({
        
        datos_plot <- malla_precip_mm_anual |>
            mutate(seleccionado = estado %in% input$select_estado)
        
        ggplot(data = datos_plot) +
            geom_histogram(aes(x = precipitacion_mm_anio, fill = seleccionado),
                           bins = 30,
                           position = "identity",
                           alpha = 0.7) +
            scale_fill_manual(values = c("TRUE" = "#F9A825", "FALSE" = "grey80"),
                              guide = "none") +
            theme_bw() +
            labs(x = "Precipitación (mm/año)", y = "Frecuencia (# de hexágonos)")
        
    })

    
    #tabla de datos--------------------------
    
    output$mean_precip_table <- renderTable({
      mean_precip_mm_anual
    })
  
    
    #boton de descarga----------------------
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(paste(input$select_estado, collapse = "_"), "_precipitacion.gpkg")
        },
        content = function(file) {
            df_exportar <- malla_precip_mm_anual
            
            sf::st_write(df_exportar, file, driver = "GPKG", delete_dsn = TRUE)
        }
    )
    
    
    
    
    
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)

