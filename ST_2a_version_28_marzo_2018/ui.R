library(shiny)

navbarPage(theme=shinytheme("cyborg"),inverse=TRUE,title=div(img(src="capsus.png", height=30, width=103,align="center")), id="nav",
           
           tabPanel(strong("Map"),
           
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               leafletOutput("map", width="150%", height="100%"),
          
                             # Styles Selectinput
                             tags$style(type='text/css', ".selectize-input { font-family:Yu Gothic UI Semibold; font-size: 13px;line-height:5px;} .selectize-dropdown { font-size: 14px; font-family:Futura Bk BT; line-height: 10px; }"),                            
                             tags$style(type='text/css',"label.control-label, 
                                        .selectize-control.single { 
                                        display: table-cell; 
                                        } 
                                        label.control-label {
                                        padding-right: 10px;
                                        }
                                        .selectize-control.single div.item {
                                        padding-right: 10px;
                                        }
                                        "),
  
                               
               #),
               
               absolutePanel(id = "controls", class = "modal-content", fixed = TRUE,
                             draggable = TRUE, cursor = "inherit" ,top = 150, left = "auto", right = 10, bottom = 150,
                             width = 210, height = 450,
                             HTML ('</br>'), 
                             HTML ('<font size="3" color="red" face="Bahnschrift">City:</font>'),
                             tags$div(title="Selecciona una ciudad.",          
                                      
                                      selectInput("select_data", label = "",choices = get_list_of_files())
                             ),
                             HTML ('<h5><strong> <p align="center"><font size="3" color="teal" face="Bahnschrift">Visualizations options:</font></p></strong></h5>'),                
                             tags$div(style="color:black;" ,title="Selecciona un tipo de mapa.",         
                                      selectInput("providerName","Map type:",c(
                                        "OpenStreetMap.Mapnik",
                                        "Hydda.Full",
                                        "Stamen.Toner",
                                        "CartoDB.Positron",
                                        "Esri.WorldImagery",
                                        "Esri.NatGeoWorldMap",
                                        "Esri.DeLorme")
                                        ) 
                                      ),
                            
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange}")),
                            
                            tags$div(style="color:black; font-family:Bahnschrift" ,title="Tamano de puntos de proximidad.",         
                                     sliderInput("point_size", "Point size",
                                                min = 1, max = 4, value = 2)
                            ), 
                          
                            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: teal}")),
                            
                            tags$div(style="color:black; font-family:Bahnschrift" ,title="Subconjunto por puntaje.",         
                            sliderInput("subset_score", "Subset by score",
                                        min = 0, max = 100, value = c(0,100))
                            ),
                            
                            tags$div(style="color:black; font-family:Arial;font-weight:bold; font-size:60%;",         
                                      textOutput("message")
                            ),
                            
                            HTML('</br>')
               ), 
               
               absolutePanel(id = "controls", class = "modal-content", fixed = TRUE,
                             draggable = TRUE, top = 65, left = 50, right = "auto", bottom =100,
                             width = "auto", height = 590,
                                
                             HTML ('<h5><strong> <p align="center"><font size="2" color="red" face="Bahnschrift">LAYERS:</font></p></strong></h5>'),

                             tags$div(style="color:black; font-family:Bahnschrift" ,title="Selecciona uno o mas atractores.",       
                                      uiOutput("dynamic_chooser_1")
                             ),
                             HTML ('</br>'), 
                             tags$div(style="color:black; font-family:Bahnschrift" ,title="Selecciona o borra un marcador en el mapa.",         
                                      radioButtons(inputId ="rb", label ="", choices= list("Add a marker","Remove a marker"), inline = TRUE
                                      )
                             ),
                             #selectInput("exist", "", choices = c(FALSE, TRUE)),
                             
                             # Styles Buttons
                             tags$head(
                               tags$style(HTML('#upd{background-color:cyan} #upd{font-family:Berlin Sans FB} #upd{color:black}'))
                             ), 
                             actionButton("upd", "Update",width = '45%',title="Actualiza los calculos de distancias"), 
                             
                             tags$head(
                               tags$style(HTML('#saved{background-color:orange} #saved{font-family:Berlin Sans FB} #saved{color:black}'))
                              ),
                             actionButton("saved", "Save Changes", width='45%', title="Guarda cambios de los calculos actualizados."),
                            
                             HTML ('</br>'), 
                             HTML ('</br>'), 
                             HTML ('<strong><font size="2" color="teal" face="Bahnschrift">FILTER METHOD:</font></strong>'),
                             HTML ('</br>'), 
                             HTML ('</br>'), 
                             HTML ('<strong> <p align="center"><font size="2" color="teal" face="Bahnschrift">FILTERS:</font></p></strong>'),
                            
                              tags$div(style="color:black; font-family:Bahnschrift" ,title="Selecciona uno o mas filtros.",        
                                       uiOutput("dynamic_chooser_2")
                                      )
               ),
               absolutePanel(id = "controls", class = "modal-content", fixed = TRUE,
                             draggable = TRUE, top = 380, left = 188, right = "auto", bottom =400,
                             width = 150, height = 40, # mas de bottom y left
                             tags$div(style="color:black;" ,title="Selecciona un metodo del filtro",         
                                      selectInput("filter_method", label ="", width='38%',
                                                  choices = list("Instersection","Union")
                                      )
                             )
               ),
               
               
               tags$div(id="cite",'    Data compiled for ', tags$em('Denpasar, Indonesia, 2018')),
               
               tags$div(style="color:white; font-family:Bahnschrift",         
                        bsModal("modalnew", "Choose a marker", "rb", size = "small",
                                HTML("CHOOSE:"),
                                br(),
                                br(),
                                img (src="hosp_true.png",width=42, height=42),
                                tags$head(
                                  tags$style(HTML('#buttontrue{background-color:green} #buttontrue{font-family:Humanst521 BT} #buttontrue{color:white}'))
                                ), 
                                actionButton("buttontrue", "TRUE", title="Es para agregar un marcador verdadero es cuando..."),
                                
                                img (src="hospital_false.png",width=40, height=42),
                                tags$head(
                                  tags$style(HTML('#buttonfalse{background-color:crimson} #buttonfalse{font-family:Humanst521 BT} #buttonfalse{color:white}'))
                                ),
                                actionButton("buttonfalse", "Hipothetic", title="Agrega un marcador hipotetico CAUSAS: abandonado, temblor, huracan, clausurado")
                        )
               )
               ) # fin del div
           ),
           
           tabPanel(strong("Data table"), 
                    HTML ('<h5><strong><p><font size="3" color="teal" face="GeoSlab703 Md BT">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;OBSERVATIONS</font></p></strong></h5>'),
                    sidebarPanel(
                      selectInput("dataset", "Choose a dataset:",
                                  choices = c("generators", "attractors", "distances")),
                      actionButton("actua", "Update View", width='48%'),
                      downloadButton("downloadData", "Download CSV", width='48%')
                      
                    ),
                    
                    mainPanel(
                      dataTableOutput("tabledata")
                    )
           ),
           
           tabPanel(strong("Settings"), 
                    
                    h2('Settings:'),
                    rHandsontableOutput("hot", height = 400,width="auto")
           ),
           
           tabPanel(strong("About"), 
                    fluidRow(column(12, 
                                    HTML(paste(tags$span(style="color: #16b3eb", 
                                                         tags$h2(tags$b("Suitability"), 
                                                                 tags$sup(tags$span(style="font-size: 65%", "beta 0.2")), 
                                                                 sep = "")))), 
                                    br(),
                                    tags$div(style="color:teal;font-weight:bold; font-family:Candara; text-align:justify; font-size:110%",         
                                             p("The Suitability tool identifies optimal locations for a specific activity within a city. The tool is mainly based in geographic information systems and multicriteria analysis 
                                               methods. It simplifies the assessing the availability of infrastructure, urban equipment 
                                               and the distance to services and amenities, between others."),
                                             p("The tool is flexible and adaptable to different resolutions, including neighbourhood level and urban-block level. Additionally, 
                                               the tool is enabled to filter out results by category, such as land use or locations prone 
                                               natural disasters and to choose the assessment criteria for each layer. Possible applications
                                               for the tool include mapping infrastructure-gap hotspots, identifying optimal locations for 
                                               social housing and strategic investment-funds allocation, between others.")))),
                    tags$div(style="text-align:center",         
                             img (src="mapbl.jpg",width=1100, height=400)),
                    img (src="ficha_capsus.png",width=1348, height=154)
                  )
           
          )
