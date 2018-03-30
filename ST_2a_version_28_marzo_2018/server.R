function(input, output, session) {

  df_r <- reactiveValues(new_data = datf)
  clicked_markers <- reactiveValues(clickedMarker = NULL) 
  values <- reactive({
    v <- read.csv(paste0("_data/",
                         input$select_data,
                         "/values.csv"))
    v <- 
      dcast(data = v, 
            formula = x + y ~ variable_name, 
            value.var = "variable_value")
    
    v <- v[c("x", "y", c(input$select_layers$right))]
    return(v)
  })
  
  filters <- reactive({
    f <- read.csv(paste0("_data/",
                         input$select_data,
                         "/filters.csv"))
    
    f <- 
      dcast(data = f, 
            formula = x + y ~ filter_name,
            value.var = "filter_value")
    
    f <- f[c("x", "y", c(input$select_filters$right))]
    
  })
  
  config <- reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = merge(
        x = data.frame(variable = c(input$select_layers$right)),
        y = read.csv(paste0("_data/",
                            input$select_data,
                            "/config.csv")
        )
      )    
    }
  })
  
  results_table <- reactive({
    subset(index_table(values_table = values(),
                       filters_table = filters(),
                       config_table = config(), 
                       selected_operation = input$filter_method), 
           index >= input$subset_score[1] & index <= input$subset_score[2])
  })
  
  
  output$map <- renderLeaflet({
    
    d <- results_table()
    if(nrow(d)<1){
      d <- data.frame(x=1, y = 1, index = 1)
    }
    print(head(d[c("x", "y", "index")]))
    
    pal <- colorNumeric(palette = rev(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58")), 
                        # domain = d$index)
                        domain = 0:100)
    d$text <- paste("Index:", round(d$index, 2), "points")
  
    leaflet(data = d) %>% 
      addProviderTiles(input$providerName, layerId = "tiles")%>%
      addCircleMarkers(lng = ~x, lat = ~y, 
                       radius = input$point_size,
                       color = ~pal(index), 
                       stroke = FALSE, 
                       fillOpacity = 0.9, 
                       popup = ~text) %>%
      fitBounds(lng1 = max(d$x), lat1 = max(d$y),
                lng2 = min(d$x), lat2 = min(d$y)) %>%
      addLegend("bottomright", pal = pal, values = 0:100,
                title = "Index",
                opacity = 1)
  })
  
  
  output$dynamic_chooser_1 <- renderUI({
    chooserInput(inputId = "select_layers",
                 leftLabel = "Available_layers",
                 rightLabel = "Selected_layers",
                 leftChoices = c(get_default_layers(dataset = read.csv(paste0("_data/", 
                                                                              input$select_data, 
                                                                              "/config.csv")), 
                                                    default = "off")),
                 rightChoices = c(get_default_layers(dataset = read.csv(paste0("_data/", 
                                                                               input$select_data, 
                                                                               "/config.csv")), 
                                                     default = "on")),
                 size = 10, multiple = TRUE)
    
  })
  output$dynamic_chooser_2 <- renderUI({
    chooserInput(inputId = "select_filters",
                 leftLabel = "Available_filters",
                 rightLabel = "Selected_filters",
                 rightChoices = c(),
                 leftChoices = c(get_filters(
                   read.csv(paste0("_data/", input$select_data, "/filters.csv"))
                 )),
                 size = 10, multiple = TRUE)
  })
  output$hot = renderRHandsontable({
    
    DF = config()
    if (!is.null(DF))
      rhandsontable(DF) %>%
      hot_col("variable", readOnly = TRUE)
    
  })
  
  v <- reactiveValues(msg = "")
  observeEvent(input$map_zoom, {
    v$msg <- paste("Zoom changed to: ", input$map_zoom)
  })
  observeEvent(input$map_bounds, {
    v$msg <- paste("Bounds changed to: ", paste(input$map_bounds, collapse = ", "))
  })
  output$message <- renderText(v$msg)
  
  observeEvent(input$map_click, {
    
    if (input$rb == "Add a marker") {
      # Menu
      toggleModal(session, "modalnew", toggle = "close")  
      
      click <- input$map_click 
      click_lat <- click$lat
      click_long <- click$lng
      
      clicked_markers$clickedMarker <- c(clicked_markers$clickedMarker, 1) 
      id <- length(clicked_markers$clickedMarker)   
      
      if(input$buttontrue){
        
    #    datasetInput$df <- rbind(rep(NA,ncol(datf)),datasetInput$df)
        
     #   datasetInput$df$lat[1] <- click_lat
      #  datasetInput$df$lon[1] <-  click_long
       # datasetInput$df$exist[1] <- as.character(input$buttontrue)
        
      }
    }
  })
  
  observeEvent(input$buttonfalse, {
    toggleModal(session, "modalnew", toggle = "close")
  })
  
  removemark<- observe({
    
    if (input$rb == "Remove a marker") {
#      showModal(modalDialog(
#        title = "Important message",
#        "You want to remove the attractor",
#        easyClose = TRUE
#      )
#    )
      
#      click <- input$map_click 
#      click_lat <- click$lat
#      click_long <- click$lng
      
#      isolate(temp<- datasetInput$df[-which(
        
#          datasetInput$df$lat == click_lat &
#          datasetInput$df$lon ==  click_long &
#          datasetInput$df$exist == input$buttontrue &
#          datasetInput$df$exist == input$buttonfalse 
        
#      ),])
#      print(temp)
#      ifelse(test = nrow(temp) == 0, 
#             yes = print("no encontré algo para eliminar"), 
#             no = datasetInput$df <- temp 
#      )
    }
  })
  
  updateDistances <- observe ({
    
    if (input$upd){ 
      #leafletProxy("map", session) %>%
      # addCircleMarkers(data = newsvalues,
      #                 radius = 2,
      #                color =  ~pal(distance))
    }
  })
  
  savechanges <- observe ({
    
    if (input$saved){
      
      showNotification("Archivo guardado.")    
    #  write.csv(x=newsvalues,file="data/distanciasnuevas.csv")
      
    }
    
  })
  
  datasetInput <- eventReactive(input$actua, {
   
        switch(input$dataset,
           #"generators" = g,
           "attractors" = datasetInput$df
           #"distances" = resultsdistances
           )
  }, ignoreNULL = FALSE)  
  
  # TABLA DE DATOS
  output$tabledata <- renderDataTable({
  #  datasetInput()
  }) 
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$dataset,'.csv', sep = '')
    },
    
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
}

