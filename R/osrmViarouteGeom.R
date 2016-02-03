#' @name osrmViarouteGeom
#' @title Get the Travel Geometry Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry between two points.
#' This function interface the \emph{viaroute} OSRM service. 
#' @param src a numeric vector of identifier, latitude and longitude (WGS84), a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine 
#' point.
#' @param dst a numeric vector of identifier, latitude and longitude (WGS84), a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the destination 
#' point.
#' @param sp if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @return If sp is FALSE, a data frame is returned. It contains the longitudes and latitudes of 
#' the travel path between the two points.\cr
#' If sp is TRUE a SpatialLinesDataFrame is returned. It contains 4 fields : 
#' identifiers of origine and destination, travel time in minutes and travel distance in 
#' kilometers.
#' @seealso \link{osrmViaroute}
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' # Travel path between points
#' routeGeom <- osrmViarouteGeom(src = com[1, c("comm_id", "lat","lon")],
#'                               dst = com[15, c("comm_id", "lat","lon")])
#' 
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(routeGeom[,2:1], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' routeGeom2 <- osrmViarouteGeom(src=c("Bethune", 50.5199, 2.64781),
#'                                dst = c("Cassel", 50.80016, 2.486388),
#'                                sp = TRUE)
#' class(routeGeom2)
#' # Display the path
#' plot(com[c(1,16),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(routeGeom2, lty = 2, add=TRUE)
#' text(com[c(1,16),3:4], labels = com[c(1,16),2], pos = 2)
#' 
#' 
#' # Inputs are SpatialPointsDataFrame
#' routeGeom3 <- osrmViarouteGeom(src = src[1,], 
#'                                dst = dst[1,], 
#'                                sp = TRUE)
#' routeGeom3@data
#' }
#' @export
osrmViarouteGeom <- function(src, dst, sp = FALSE){
  tryCatch({
    oprj <- NA
    if(testSp(src)){
      oprj <- sp::proj4string(src)
      src <- src[1,]
      x <- spToDf(x = src)
      src <- c(x$loc[1,1],x$loc[1,2], x$loc[1,3])
    }
    
    if(testSp(dst)){
      dst <- dst[1,]
      x <- spToDf(x = dst)
      dst <- c(x$loc[1,1],x$loc[1,2], x$loc[1,3])
    }


    
    # build the query
    req <- paste(getOption("osrm.server"), 
                 "viaroute?loc=", 
                 src[2], ",", src[3], 
                 "&loc=",
                 dst[2],",",dst[3], 
                 "&alt=false&geometry=true&",
                 "output=json&compression=false",
                 sep="")
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req), 
                            useragent = "'osrm' R package")
    
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$status_message)
    if(res$status != "200"){stop(e)}
    
    # Coordinates of the line
    geodf <- data.frame(res$route_geometry)
    names(geodf) <-  c("lat", "lon")
    
    # Convert to SpatialLinesDataFrame
    if (sp==TRUE){
      if(!'package:sp' %in% search()){
        attachNamespace('sp')
      }
      routeLines <- sp::Lines(slinelist = sp::Line(geodf[,2:1]), 
                              ID = "x")
      routeSL <- sp::SpatialLines(LinesList = list(routeLines), 
                                  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(src = src[1], dst = dst[1], 
                       time = res$route_summary$total_time/60,
                       distance = res$route_summary$total_distance/1000)
      geodf <- sp::SpatialLinesDataFrame(routeSL, 
                                         data = df, 
                                         match.ID = FALSE)   
      row.names(geodf) <- paste(src[1], dst[1],sep="_")

      if (!is.na(oprj)){
        geodf <- sp::spTransform(geodf, oprj)
      }
    }
    return(geodf)
  }, error=function(e) { message("osrmViarouteGeom function returns an error: \n", e)})
  return(NULL)
}









