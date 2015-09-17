#' @name osrmViarouteGeom
#' @title Query OSRM to Get the Travel Geometry Between Two Points
#' @description Query OSRM to get the travel geometry between two points.
#' This function interface the \emph{viaroute} OSRM service. 
#' @param xo longitude of the origine point.
#' @param yo latitude of the origine point.
#' @param xd longitude of the destination point.
#' @param yd latitude of the destination point.
#' @return A data frame is return. It contains the longitudes and latitudes of 
#' the travel path between the two points.
#' @examples 
#' # Load data
#' data("com")
#' # Travel path between points
#' routeGeom <- osrmViarouteGeom(xo = com[1,"lon"], yo = com[1,"lat"],
#'                               xd = com[15,"lon"], yd = com[15,"lat"])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(routeGeom[,2:1], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' @export
osrmViarouteGeom <- function(xo, yo, xd, yd){
  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "viaroute?loc=", sep = "")
    
    tab <- paste(tab, yo, ",", xo, "&loc=",yd,",",xd, 
                 "&alt=false&geometry=true&output=gpx",sep="")
    
    # Sending the query
    tab2 <- RCurl::getURL(URLencode(tab), useragent = "'osrm' R package")
    
    # Parsing
    pfile <- XML::htmlTreeParse(tab2, useInternalNodes = T)
    
    # Get all elevations, times and coordinates via the respective xpath
    coords <- XML::xpathSApply(pfile, path = "//rtept", XML::xmlAttrs)
    
    # Extract latitude and longitude from the coordinates
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    
    geodf <- data.frame(lat = lats, lon = lons)
    
    return(geodf)
  }, error=function(e) { message("osrmViarouteGeom function returns an error: \n", e)})
  return(NULL)
}





