function(input, output, session) {

hosptab <- reactiveValues()
hosptab$df <- hosptable
  
  df_r <- reactiveValues(new_data = datf)
  clicked_markers <- reactiveValues(clickedMarker = NULL) 
  
  observeEvent(input$map_click, {
    
    if (input$rb == "Add a marker") {
      click <- input$map_click 
      click_lat <- click$lat
      click_long <- click$lng
      
      clicked_markers$clickedMarker <- c(clicked_markers$clickedMarker, 1) 
      id <- length(clicked_markers$clickedMarker)      
      
      hosptab$df <- rbind(rep(NA,ncol(datf)),hosptab$df)
      
        hosptab$df$lat[1] <- click_lat
        hosptab$df$lon[1] <-  click_long
        hosptab$df$exist[1] <- as.character(input$exist)
      
      write.csv(x=hosptab$df,file="data/newsattractors.csv")
    }
  })
  
  removemark<- observe({
    
    if (input$rb == "Remove a marker") {
      showModal(modalDialog(
       title = "Important message",
        "You want to remove the attractor",
        easyClose = TRUE
      ))
      
      click <- input$map_click 
      click_lat <- click$lat
      click_long <- click$lng
      
      isolate(temp<- hosptab$df[-which(
        
        hosptab$df$lat == click_lat &
        hosptab$df$lon ==  click_long &
        hosptab$df$exist == input$exist
        
      ),])
      print(temp)
      write.csv(x=hosptab$df,file="data/newsattractors.csv") 
      ifelse(test = nrow(temp) == 0, 
             yes = print("no encontré algo para eliminar"), 
             no = hosptab$df <- temp 
      )
    }
  })
   
  updateDistances <- observe ({
      
    if (input$upd){ 
      leafletProxy("map", session) %>%
      addCircleMarkers(data = newsvalues,
                      radius = 2,
                      color =  ~pal(distance))
    }
  })
  
  savechanges <- observe ({
    newsattractors <- read.csv("data/newsattractors.csv")
    newsvalues <- distance_new_table(generators =g,
                                    attractorsn = newsattractors)
    if (input$saved){
      
      showNotification("Archivo guardado.")    
      write.csv(x=newsvalues,file="data/distanciasnuevas.csv")
      #savedchang <- read.csv("data/distanciasnuevas.csv")
      #View(savedchang)
      
    }
   
  })
  
  datasetInput <- eventReactive(input$actua, {
    resultsdistances <- read.csv("data/distanciasnuevas.csv")
    switch(input$dataset,
           "generators" = g,
           "attractors" = hosptab$df,
           "distances" = resultsdistances)
  }, ignoreNULL = FALSE)  

  # TABLA DE DATOS
output$tabledata <- renderDataTable({
  datasetInput()
}) #,rownames= TRUE

output$downloadData <- downloadHandler(
  
  filename = function() {
    paste(input$dataset, input$filetype, sep = ".")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(datasetInput(), file, sep = sep,
                row.names = FALSE)
  }
)

# MAPA
  output$map <- renderLeaflet({
    leaflet(data=hosptab$df)%>%
      addTiles(layerId = "tiles")%>%
      addMarkers(icon = ~isolate(iconify(exist)))%>%   
  
   addCircleMarkers(data = values,
                      radius = 2,
                       color =  ~pal(distance))%>%
      #addMarkers(data=destiny,lng=~longitude, lat=~latitude, label = ~name,icon=~markerhosp, 
       #          popup=~paste("<h6 style='color:red'>#",code,"</h6>","<strong>Hospital:</strong>",name))%>%
      # Map view 
      setView(lng = 115.25942542764824, lat =-8.645581443331796, zoom = 11) #
  })
  
  observeEvent(input$providerName, {
    leafletProxy("map", session) %>%
      addProviderTiles(input$providerName, layerId = "tiles")
  })
  # Icon marker hospital
  #markerhosp <- makeIcon(
   # iconUrl = "http://cdn2.iconfinder.com/data/icons/location-map-simplicity/512/hospital-512.png",
    #iconWidth = 1, iconHeight = 1
  #)
  pal <- colorNumeric(palette = rev(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58")),
                      domain = values$distance)
  
  # Events: Add and Clear markers hospitals 
  v <- reactiveValues(msg = "")
  observeEvent(input$map_zoom, {
    v$msg <- paste("Zoom changed to", input$map_zoom)
  })
  observeEvent(input$map_bounds, {
    v$msg <- paste("Bounds changed to", paste(input$map_bounds, collapse = ", "))
  })
  output$message <- renderText(v$msg)
  # End: add and clear markers
}

