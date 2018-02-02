
# Functions in urban.context v 0.03

# Estimate the distance from point 1 to point 2
estimate_distance <- function(x1, y1, x2, y2){
  return(
    t(spDists(
      SpatialPoints(coords = data.frame(x = x1, y = y1), proj4string=CRS("+proj=longlat +datum=WGS84")),
      SpatialPoints(coords = data.frame(x = x2, y = y2), proj4string=CRS("+proj=longlat +datum=WGS84")),
      longlat=T))
  )
}

# Subset data with a "square" cut
cut_data <- function(x, y, data, df.x = "x", df.y = "y", side.length){
  return(
    data[data[,df.x] <= x + side.length &
           data[,df.x] > x - side.length &
           data[,df.y] <= y + side.length &
           data[,df.y] > y - side.length  , ])
}

# distance_to_nearest_service
distance_to_nearest_service <-  function(x, y, data, df.x = "x", df.y = "y", radius = 20, empty.value = 20){
  # subset data by cutting the data.frame 
  # consider that 1 km ~ 1 / 50 degree 
  # please note that this is a "square" cut
  
  data <- 
    cut_data(x = x, y = y, 
             data = data, 
             df.x = df.x, df.y = df.y, 
             side.length = radius / 50)
  
  # is the subset empty?
  if (nrow(data) > 0) {
    # if it's not, then estimate distances to point (x,y)
    data$distance <- estimate_distance(x1 = x, y1 = y,
                                       x2 = data[,df.x],
                                       y2 = data[,df.y])
    return(min(data[,"distance"]))
    # if the subset is empty, return the empty.value
  } else{
    return(empty.value)}
}


distance_table <- function(attractors, generators){
  
  for(r in 1:nrow(generators)){
    generators$distance[r] <- distance_to_nearest_service(x = generators$lon[r], 
                                                          y = generators$lat[r], 
                                                          data = attractors, 
                                                          df.x = "lon", 
                                                          df.y = "lat")#df data frame
 #   print(format_percentage(x = r, nrow(generators)))
    
  }
  
  return(generators)
  
}



