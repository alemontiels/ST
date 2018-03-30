
check_values_file <- function(directory){

  print("Checking values.csv file...")

  values_exists <- file.exists(paste0(directory, "values.csv"))
  
  if(values_exists){

    load_tables <- TRUE
    # Load the table for preliminary tests
    values_temp <- read.csv(file = paste0(directory, "values.csv"))
    
    # Does the table have x and y variables in it's first columns?
    x_col_ok <- colnames(values_temp)[1] == "x"
    if(x_col_ok){
      print("First column is named 'x', thanks.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      print("ERROR: First column is not named 'x', please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
    # y
    y_col_ok <- colnames(values_temp)[2] == "y"
    if(y_col_ok){
      print("Second column is named 'y', thanks.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      print("ERROR: Second column is not named 'y', please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
    # Are x and y numeric or integer?
    # x
    if(is.numeric(values_temp$x) | is.integer(values_temp$x)){
      print("'x' is numeric, thanks.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      print("ERROR: It seams that 'x' is not numeric, please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
    # y
    if(is.numeric(values_temp$y) | is.integer(values_temp$y)){
      print("'y' is numeric, thanks.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      print("ERROR: It seams that 'y' is not numeric. Please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
    # Does values.csv contain at least one variable?
    if(ncol(values_temp) > 2){
      # Talk to your user
      print("The values.csv table has the following variables:")
      print(colnames(values_temp))
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      # Talk to your user
      print("ERROR: It seams that the values.csv table has no variables at all. Please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
    # Are variables in values.csv numeric or integer?
    if(all(lapply(values_temp[c(-1,-2)], class) == "numeric" | 
           lapply(values_temp[c(-1,-2)], class) == "integer")){
      # Talk to your user
      print("All variables in table values.csv are either numeric or integer, thanks.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- TRUE & load_tables
    } else{
      # Talk to your user
      print("ERROR: Not all variables in table values.csv are either numeric or integer. Please select a valid file.")
      # At the end of the program, shall I recommed to load the tables?
      load_tables <- FALSE & load_tables
    }
    
  } else{
    print("ERROR: Couldn't find a values.csv file in the selected directory. There is no much I can do without a valid values.csv file. Quiting importing. Please try it again with a valid directory.")
  }
  print("Can I load values.csv?")
  print(load_tables)
  return(load_tables)
}


# rowProds
rowProds <- function(X){ apply(X,1,FUN="prod") }

## normalization methods 

### normalize a vector
normalize <- function(values_vector, minmax = NULL){
  if(is.null(minmax)){
    min_value <- min(values_vector, na.rm = T)
    max_value <- max(values_vector, na.rm = T)
  } else {
    min_value <- minmax[1]
    max_value <- minmax[2]
  }
  # let the smallest value be zero
  values_vector <- values_vector - min_value
  values_vector[which(values_vector < 0)] <- 0 
  # let the largest value be 100
  values_vector <- 100*values_vector/(max_value-min_value)
  values_vector[which(values_vector > 100)] <- 100 
  return(values_vector)
}

### standarize data
### get a vector and scale it
standarize <- function(values_vector, center = TRUE, scale= TRUE){
  values_vector   <- scale(x = values_vector, center = center, scale = scale)
  return(normalize(values_vector))
}

### solve_smaller_better_cases
solve_smaller_better_cases <- function(normalized_table, config_table){ 
  
  for(n in get_layers(normalized_table)){
    if(as.character(config_table[which(config_table$variable == n), "normalization.type"]) == "smaller_better"){
      normalized_table[n] <- 100 - normalized_table[n]
    }
  }
  return(
    normalized_table
  )
}

# grind
grind <- function(filters_table, operation){
  loc <- filters_table[c(+1, +2)]
  val   <- filters_table[c(-1, -2)]
  if(ncol(val) > 0){
    if(operation == "Union"){
      filter_index <- rowSums(val)  
    } else {
      filter_index <- rowProds(val) 
    }
    
    grinded_df <- cbind(filter_index, loc)
    grinded_df <- as.data.frame(grinded_df[which(grinded_df$filter_index>0), ])
    grinded_df <- grinded_df[-1]
    return(grinded_df)
  } else{
    return(filters_table[1:2])
  }
}

# index_table
index_table <- function(values_table, filters_table, config_table, selected_operation){
  
  # filter
  values_table <- merge(grind(filters_table, selected_operation), values_table)
  
  # normalize
  for(n in get_layers(values_table)){
    
    selected_method <- subset(config_table, variable == n)$normalization.method
    
    # standarize
    if(selected_method == "standarize"){
      values_table[n] <- standarize(values_table[n])
    }
    # reference
    if(selected_method == "reference"){
      lowest_value <- as.numeric(config_table[which(config_table$variable == n), "lowest.value"])
      highest_value <- as.numeric(config_table[which(config_table$variable == n), "highest.value"])
      values_table[n] <- normalize(values_vector = values_table[,n],
                                   minmax = c(lowest_value, highest_value))
    }
    # observe
    if(selected_method == "observe"){
      values_table[n] <- normalize(values_vector = values_table[,n])
    }
  }
  
  # solve_smaller_better_cases
  values_table <- solve_smaller_better_cases(values_table, config_table)
  
  # weight
  index <- rep(0, nrow(values_table))
  for(n in get_layers(values_table)){
    index <- index + values_table[,n] * as.numeric(config_table[which(config_table$variable == n), "weight"])
  }
  
  values_table$index <- index / sum(config_table$weight)
  
  # return
  return(values_table)
}

# chooser
chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 20, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

get_layers <- function(dataset){
  colnames(dataset[c(-1, -2)])
}

get_default_layers <- function(dataset, default){
  as.vector(dataset[which(dataset$default == default), "variable"])
}

get_filters <- function(dataset){
  as.vector(levels(as.factor(dataset$filter_name)))
}

# get_list_of_files
get_list_of_files <- function(){
  return(list.files(path = "_data"))
}
