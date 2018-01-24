
# 2. ST (GitHub) muestra un mapa con *circle markers* con una escala de color que indica proximidad.
#3. ST (GitHub) permite agregar o borrar un *marker*

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
#library(dplyr)
library(rsconnect)
library (sp)


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
    leaflet() %>%
      
      addTiles(
     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
      #  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      # Escala de color
      addCircleMarkers(data = values,
                       radius = 2,
                       color =  ~pal(distance))%>%
    
      setView(lng = 115.25942542764824, lat =-8.645581443331796, zoom = 11)
   # map_type <- c("Hydda.Full", "Stamen.TonerLite", "OpenStreetMap.Mapnik", "Esri.WorldImagery")[as.numeric(input$map_type)]   
})
 
  
  # icon marker
  markerhosp <- makeIcon(
    iconUrl = "http://cdn2.iconfinder.com/data/icons/location-map-simplicity/512/hospital-512.png",
    iconWidth = 30, iconHeight = 37
  )
 # Create a color palette
  pal <- colorNumeric(palette = rev(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58")),
                     domain = values$distance)
  #####----
  
  leafletProxy("map", data = allzips) %>%
     clearShapes() %>%
   addMarkers(lng=~longitude, lat=~latitude,icon=~markerhosp, popup=~paste("<h6 style='color:red'>#",code,"</h6>","<strong>Hospital:</strong>",name))
  
  #map_type <- c("Hydda.Full", "Stamen.TonerLite", "OpenStreetMap.Mapnik", "Esri.WorldImagery")[as.numeric(input$map_type)]
  ## Data Explorer  ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

