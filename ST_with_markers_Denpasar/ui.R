library(leaflet)
# titlePanel(windowTitle = PAGE_TITLE,
#title =
 # div(
  #  img(
   #   src = "my_logo.png",
    #  height = 100,
     # width = 100,
      #style = "margin:10px 10px"
    #),
    #PAGE_TITLE
  #)
#),
# Choices for drop-downs
source("chooser.R")
vars <- c(
  "Is SuperZIP?" = "superzip",
  #"Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


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

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = 20, right = "auto", bottom ="auto",
        width = 400, height = "auto",

        h2("Explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('The State of White America, 1960–2010')
      )
    )
  ),

  tabPanel(strong("Data"),
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All ids"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),
  
  tabPanel(strong("Selections"),
           
           chooserInput("mychooser", "Available frobs", "Selected frobs",
                        row.names(USArrests), c(), size = 10, multiple = TRUE
           ),
           verbatimTextOutput("selection")
  ),
  
  navbarMenu(strong("More"), icon = icon("save"),
             #tabPanel("Text", icon = icon("save")),
             tabPanel("Uploading files",
                      titlePanel("Uploading Files"),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Select a file ----
                          fileInput("file1", "Choose CSV File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          # Input: Select quotes ----
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Data file ----
                          tableOutput("contents")
                          
                        )
                        
                      )
             
             
                      
                      
                      ),
             
             tabPanel("Downloading data",
                      titlePanel("Downloading Data"),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Choose dataset ----
                          selectInput("dataset", "Choose a dataset:",
                                      choices = c("Datos de superzip en EUA")),
                          
                          # Button
                          downloadButton("downloadData", "Download")
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          tableOutput("table")
                          
                        )
                        
                      )
                      
                      
                      )
             ),
  
  
  conditionalPanel("false", icon("crosshair"))
)