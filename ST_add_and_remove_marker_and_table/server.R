
function(input, output, session) {
  
  ## chooser
  output$selection <- renderPrint(
    input$mychooser
  )
  
##------------------  UPLOADING FILE ------------------------------
  
  output$contents <- renderTable({
    
  req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  #####------------------ DOwnloading
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Datos de superzip en EUA" = cars)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
# Create the map
  output$map <- renderLeaflet({
    
    input$update
    isolate(leaflet(data=allzips) %>%
              addProviderTiles(input$bmap))%>%
  
      # Color scale indicating proximity
      addCircleMarkers(data = values,
                       radius = 2,
                       color =  ~pal(distance))%>%

          # Markers hospitals with popups
addMarkers(lng=~longitude, lat=~latitude, label = ~name,icon=~markerhosp, 
           popup=~paste("<h6 style='color:red'>#",code,"</h6>","<strong>Hospital:</strong>",name),
           layerId = paste0("marker", 1:59))%>%
    
      # Map view 
      setView(lng = 115.25942542764824, lat =-8.645581443331796, zoom = 11)
  })
 
# Icon marker hospital
  markerhosp <- makeIcon(
    iconUrl = "http://cdn2.iconfinder.com/data/icons/location-map-simplicity/512/hospital-512.png",
    iconWidth = 30, iconHeight = 37
  )
  
# Create a color palette that indicates proximity (near distances)
  pal <- colorNumeric(palette = rev(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58")),
                     domain = values$distance)
  
# Events: Add and Clear markers hospitals 
  v <- reactiveValues(msg = "")
  
  df_r <- reactiveValues(new_data = datf)
  clicked_markers <- reactiveValues(clickedMarker = NULL) 
  observeEvent(input$map_click, { 
    
    click<- input$map_click 
    click_lat <- click$lat
    click_long <- click$lng
    
    clicked_markers$clickedMarker <- c(clicked_markers$clickedMarker, 1) 
    id <- length(clicked_markers$clickedMarker) 
    
    v$msg <- paste("Add marker in: ",input$map_click$lat, ",", input$map_click$lng)
    
    if (input$addMarker) {
      leafletProxy("map") %>%
        addMarkers(lng = click$lng, lat = click$lat, layerId = id) #layerid
      
        df_r$new_data <- rbind(rep(NA,ncol(datf)), df_r$new_data)
        df_r$new_data$longitude[1] <- click_long
        df_r$new_data$latitude[1] <- click_lat
        df_r$new_data$existing[1] <- click_long
      
        #if (input$removeMark) {
          #leafletProxy("map", session) %>%
        #         removeMarker(input$map_click)
          
         # }
        
        }
  })
  
  output$tabledata <- renderDataTable({
    df_r$new_data
    
    
   # allzips <- read.csv("data/hospitals_denpasar_con_nombres.csv")
    })
  
  observeEvent(input$map_zoom, {
    v$msg <- paste("Zoom changed to", input$map_zoom)
  })
  observeEvent(input$map_bounds, {
    v$msg <- paste("Bounds changed to", paste(input$map_bounds, collapse = ", "))
  })
  observeEvent(input$clearMarkers, {
    
    leafletProxy("map") %>% clearMarkers()
  })
  
 # observeEvent(input$removeMark, {
  #  if (input$removeMark) {
   #   leafletProxy("map") %>%
    #    removeMarker(lng = click$lng, lat = click$lat,id)
      
    #}
  #})
  observeEvent(input$map_marker_click, {
  if (input$removeMark) {
  leafletProxy("map", session) %>%
        removeMarker(input$map_marker_click$id)
   
    }
  })
  
  output$message <- renderText(v$msg)

# End: add and clear markers
}