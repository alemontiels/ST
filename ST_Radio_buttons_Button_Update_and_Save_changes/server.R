
function(input, output, session) {
  hosptab <- reactiveValues()
  hosptab$df <- hosptable
  
  ##------------------  UPLOADING FILE ------------------------------
  
 # output$contents <- renderTable({
    
  #  req(input$file1)
    
   # df <- read.csv(input$file1$datapath,
    #               header = input$header,
     #              sep = input$sep,
      #             quote = input$quote)
    
    #if(input$disp == "head") {
     # return(head(df))
    #}
    #else {
     # return(df)
    #}
    
  #})
  #####------------------ DOwnloading
  
  # Reactive value for selected dataset ----
  #datasetInput <- reactive({
   # switch(input$dataset,
    #       "Datos de superzip en EUA" = cars)
  #})
  
  # Table of selected dataset ----
  #output$table <- renderTable({
   # datasetInput()
  #})
  
  # Downloadable csv of selected dataset ----
  #output$downloadData <- downloadHandler(
   # filename = function() {
    #  paste(input$dataset, ".csv", sep = "")
    #},
    #content = function(file) {
     # write.csv(datasetInput(), file, row.names = FALSE)
    #}
  #)
  
  
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
      
      #     hosptab$df$id[1] <- as.numeric(input$id)
      hosptab$df$lat[1] <- click_lat
      hosptab$df$lon[1] <-  click_long
      hosptab$df$exist[1] <- as.character(input$exist)
     write.csv(x=hosptab$df,file="data/vernewatractores.csv")
      #newatracttors<- read.csv("vernewatractores.csv")
     
    }
    
    
  })
  
  removemark<- observe({
    # clicked_markers$clickedMarker <- c(clicked_markers$clickedMarker, 1) 
    #id <- length(clicked_markers$clickedMarker)      
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
        
        #hosptab$df$id == input$id &
        hosptab$df$lat == click_lat &
          hosptab$df$lon ==  click_long &
          hosptab$df$exist == input$exist
        
      ),])
      print(temp)
      write.csv(x=hosptab$df,file="data/vernewatractores.csv") #1
      ifelse(test = nrow(temp) == 0, 
             yes = print("no encontré algo para eliminar"), 
             no = hosptab$df <- temp 
      )
      
      
    }
  })
  
  savechanges <- observe ({
    
    if (input$saved){
      savedchang<- read.csv("data/vernewatractores.csv")
      View(savedchang)
    
  showNotification("Cambios guardados.")    
      
    }
  })
## update
  updateDistances <- observe ({
    
    source("functionsnew.R")
  if (input$upd){
    leafletProxy("map", session) %>%
    addCircleMarkers(data = valuesnew,
                      radius = 2,
                      color =  ~pal(distance))
  }
    write.csv(x=valuesnew, file="data/distanciasnuevas.csv")
  })
  output$tabledata <- renderDataTable({hosptab$df}) #,rownames= TRUE
 

  output$map <- renderLeaflet({
    leaflet(data=hosptab$df)%>%
      addTiles(layerId = "tiles")%>%
      addMarkers(icon = ~isolate(iconify(exist)), layer=~id)%>%   
  
   addCircleMarkers(data = values,
                      radius = 2,
                       color =  ~pal(distance))%>%
      
    #  addMarkers(icon = ~isolate(iconify(exist))) %>%
      
      # Markers hospitals with popups
      
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
  
  # Create a color palette that indicates proximity (near distances)
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