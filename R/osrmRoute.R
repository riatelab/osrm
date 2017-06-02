#' @name osrmRoute
#' @title Get the Shortest Path Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry between two points.
#' This function interfaces the \emph{route} OSRM service. 
#' @param src a numeric vector of identifier, longitude and latitude (WGS84), a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine 
#' point.
#' @param dst a numeric vector of identifier, longitude and latitude (WGS84), a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the destination 
#' point.
#' @param overview "full", "simplified" or FALSE. Add geometry either full (detailed), simplified 
#' according to highest zoom level it could be display on, or not at all. 
#' @param sp if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @return If sp is FALSE, a data frame is returned. It contains the longitudes and latitudes of 
#' the travel path between the two points.\cr
#' If sp is TRUE a SpatialLinesDataFrame is returned. It contains 4 fields : 
#' identifiers of origine and destination, travel time in minutes and travel distance in 
#' kilometers.\cr
#' If overview is FALSE, a named numeric vector is returned. It contains travel time (in minutes) 
#' and travel distance (in kilometers).
#' @examples
#' \dontrun{
#' 
#' # Load data
#' data("com")
#' # Travel path between points
#' route <- osrmRoute(src = com[1, c("comm_id", "lon","lat")],
#'                    dst = com[15, c("comm_id", "lon","lat")])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(route[,1:2], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' route2 <- osrmRoute(src=c("Bethune", 2.64781, 50.5199),
#'                     dst = c("Cassel", 2.486388, 50.80016),
#'                     sp = TRUE)
#' # Display the path
#' if(require(sp)){
#'   plot(com[c(1,16),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#'   plot(route2, lty = 2, add=TRUE)
#'   text(com[c(1,16),3:4], labels = com[c(1,16),2], pos = 2)
#' }
#' # Input is SpatialPointsDataFrames
#' route3 <- osrmRoute(src = src[1,], dst = dst[1,], sp = TRUE)
#' route3@data
#' }
#' @export
osrmRoute <- function(src, dst, overview = "simplified", sp = FALSE){
  tryCatch({
    
    # src = com[1, c("comm_id", "lon","lat")]
    # dst = com[2, c("comm_id", "lon","lat")]
    # sp=TRUE
    # overview = "simplified"
    
    oprj <- NA
    if(testSp(src)){
      oprj <- sp::proj4string(src)
      src <- src[1,]
      x <- spToDf(x = src)
      src <- c(x[1,1],x[1,2], x[1,3])
    }
    if(testSp(dst)){
      dst <- dst[1,]
      x <- spToDf(x = dst)
      dst <- c(x[1,1],x[1,2], x[1,3])
    }
    
    # build the query
    req <- paste(getOption("osrm.server"),
                 "route/v1/", getOption("osrm.profile"), "/", 
                 src[2], ",", src[3],
                 ";",
                 dst[2],",",dst[3], 
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview),
                 sep="")
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req),
                            useragent = "'osrm' R package")
    # Deal with \\u stuff
    vres <- jsonlite::validate(resRaw)[1]
    if(!vres){
      resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)

    # Error handling
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}

    if (overview == FALSE){
      return(round(c(duration = res$routes$duration/60,
                     distance = res$routes$distance/1000), 2))
    }
    if(!vres){
      res$routes$geometry <- gsub(pattern = "zorglub", replacement = "\\\\",
                                  x = res$routes$geometry)
    }
    # Coordinates of the line
    geodf <- gepaf::decodePolyline(res$routes$geometry)[,c(2,1)]

    # Convert to SpatialLinesDataFrame
    if (sp == TRUE){
      routeLines <- sp::Lines(slinelist = sp::Line(geodf[,1:2]),
                              ID = "x")
      routeSL <- sp::SpatialLines(LinesList = list(routeLines),
                                  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(src = src[1], dst = dst[1],
                       duration = res$routes$legs[[1]]$duration/60,
                       distance = res$routes$legs[[1]]$distance/1000)
      geodf <- sp::SpatialLinesDataFrame(routeSL, data = df, match.ID = FALSE)
      row.names(geodf) <- paste(src[1], dst[1],sep="_")
      if (!is.na(oprj)){
        geodf <- sp::spTransform(geodf, oprj)
      }
    }
    return(geodf)
  }, error=function(e) {message("osrmRoute function returns an error: \n", e)})
  return(NULL)
}


