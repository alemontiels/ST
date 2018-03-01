#library(leaflet)
library(shiny)

navbarPage(inverse=TRUE,title=div(img(src="capsus.png", height=30, width=80,align="center")), id="nav",
           
           tabPanel(strong("Map",style = "color:Orange"),
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="150%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = "auto", left = 75, right = "auto", bottom =80,
                                      width = 350, height = 400,
                                      
                                      HTML ('<h5><strong> <p align="center">HEALTH SECTOR</p></strong></h5>'),
                                      
                                      selectInput("providerName","Type of map:", c(
                                        "Hydda.Full",
                                        "Stamen.Toner",
                                        "CartoDB.Positron",
                                        "Esri.WorldImagery",
                                        "Stamen.Watercolor")
                                      ),
                                      radioButtons(inputId ="rb", label =""
                                                   , choices= list("Add a marker","Remove a marker"), inline = TRUE
                                      ),
                                      HTML ('<h5><strong><p align="center">When adding a marker, you must first choose "FALSE" or "TRUE".</p></strong></h5>'),
                                      selectInput("exist", "", choices = c(FALSE, TRUE)),
                                      # EFECTS BUTTONS
                                      tags$head(
                                        tags$style(HTML('#upd{background-color:cyan} #upd{font-family:Berlin Sans FB} #upd{color:black}'))
                                      ), 
                                      actionButton("upd", "Update",width = '45%'), 
                                      tags$head(
                                        tags$style(HTML('#saved{background-color:orange} #saved{font-family:Berlin Sans FB} #saved{color:black}'))
                                      ),
                                      actionButton("saved", "Save Changes", width='45%'),
                                      HTML ('</br>'),      
                                      HTML ('</br>'), 
                                      
                                      strong(textOutput("message"))
                                      
                        ),
                        
                        
                        tags$div(id="cite",
                                 '    Data compiled for ', tags$em('Denpasar, Indonesia, 2018')
                        )
                    )
           ),
  tabPanel(strong("Data table"), 
    sidebarPanel(
           selectInput("dataset", "Choose a dataset:",
                       choices = c("generators", "attractors", "distances")),
           actionButton("actua", "Update View"),
           h4("To download:"),
           radioButtons("filetype", "File type:",
                        choices = c("csv","tsv")
                        ),
           downloadButton('downloadData', 'Download')
    ),
    mainPanel(
           h4("Observations"),
          # tableOutput("view"),
           
           dataTableOutput("tabledata"))
)
)