#' @name osrmViarouteGeom
#' @title Get the Travel Geometry Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry between two points.
#' This function interface the \emph{viaroute} OSRM service. 
#' @param srcLon longitude of the origine point.
#' @param srcLat latitude of the origine point.
#' @param dstLon longitude of the destination point.
#' @param dstLat latitude of the destination point.
#' @param sp if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @param srcId identifier of the origin point (use if sp is TRUE).
#' @param dstId identifier of the destination point (use if sp is TRUE).
#' @return A data frame is return. It contains the longitudes and latitudes of 
#' the travel path between the two points. If sp is TRUE a SpatialLinesDataFrame 
#' is return. It contains two fields : identifiers of origine and destination.
#' @seealso \link{osrmViaroute}
#' @examples 
#' \dontrun{
#' # Load data
#' data("com")
#' # Travel path between points
#' routeGeom <- osrmViarouteGeom(srcLon = com[1,"lon"], srcLat = com[1,"lat"],
#'                               dstLon = com[15,"lon"], dstLat = com[15,"lat"])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(routeGeom[,2:1], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' routeGeom2 <- osrmViarouteGeom(srcLon = com[1,"lon"], srcLat = com[1,"lat"],
#'                                dstLon = com[16,"lon"], dstLat = com[16,"lat"], 
#'                                sp=TRUE, 
#'                                srcId = com[1,"comm_id"], 
#'                                dstId = com[16,"comm_id"])
#' class(routeGeom2)
#' # Display the path
#' plot(com[c(1,16),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(routeGeom2, lty = 2, add=TRUE)
#' text(com[c(1,16),3:4], labels = com[c(1,16),2], pos = 2)
#' }
#' @export
osrmViarouteGeom <- function(srcLat, srcLon, dstLat, dstLon, sp = FALSE, 
                             srcId = "start", dstId = "end"){
  tryCatch({
    # build the query
    req <- paste(getOption("osrm.server"), 
                 "viaroute?loc=", 
                 srcLat, ",", srcLon, 
                 "&loc=",
                 dstLat,",",dstLon, 
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
      df <- data.frame(srcId = srcId, dstId = dstId)
      geodf <- sp::SpatialLinesDataFrame(routeSL, 
                                         data = df, 
                                         match.ID = FALSE)   
      row.names(geodf) <- paste(srcId, dstId,sep="_")
    }
    return(geodf)
  }, error=function(e) { message("osrmViarouteGeom function returns an error: \n", e)})
  return(NULL)
}







