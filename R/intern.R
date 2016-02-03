
testSp <- function(x){
  if (class(x) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")){
    if (is.na(sp::proj4string(x))){
      stop(
        paste(
          "Your input (", quote(x),
          ") does not have a valid coordinate reference system.", sep=""),
        call. = F)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

spToDf <- function(x){
  # transform to WGS84
  x <- sp::spTransform(x = x, CRSobj = "+init=epsg:4326")
  # this function takes a SpatialDataFrame and transforms it into a dataframe
  x <-  data.frame(id = row.names(x), 
                   lat = sp::coordinates(x)[,2], 
                   lon = sp::coordinates(x)[,1], 
                   stringsAsFactors = FALSE)
  return(list(loc = x, id = "id", lat = "lat", lon = "lon"))
}