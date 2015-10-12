#' @name osrmViarouteGeom
#' @title Get the Travel Geometry Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry between two points.
#' This function interface the \emph{viaroute} OSRM service. 
#' @param xo longitude of the origine point.
#' @param yo latitude of the origine point.
#' @param xd longitude of the destination point.
#' @param yd latitude of the destination point.
#' @param sp if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @param ido identifier of the origin point (use if sp is TRUE).
#' @param idd identifier of the destination point (use if sp is TRUE).
#' @return A data frame is return. It contains the longitudes and latitudes of 
#' the travel path between the two points. If sp is TRUE a SpatialLinesDataFrame 
#' is return. It contains two fields : identifiers of origine and destination.
#' @seealso \link{osrmViaroute}
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
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' routeGeom2 <- osrmViarouteGeom(xo = com[1,"lon"], yo = com[1,"lat"],
#'                                xd = com[16,"lon"], yd = com[16,"lat"], 
#'                                sp=TRUE, 
#'                                ido = com[1,"comm_id"], 
#'                                idd = com[16,"comm_id"])
#' class(routeGeom2)
#' # Display the path
#' plot(com[c(1,16),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(routeGeom2, lty = 2, add=TRUE)
#' text(com[c(1,16),3:4], labels = com[c(1,16),2], pos = 2)
#' 
#' 
#' 
#' 
#' 
#' @export
osrmViarouteGeom <- function(xo, yo, xd, yd, sp = FALSE, ido = "start", idd = "end"){
  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "viaroute?loc=", sep = "")
    
    tab <- paste(tab, yo, ",", xo, "&loc=",yd,",",xd, 
                 "&alt=false&geometry=true&output=gpx",sep="")
    
    # Sending the query
    tab2 <- RCurl::getURL(utils::URLencode(tab), useragent = "'osrm' R package")
    
    # Parsing
    pfile <- XML::htmlTreeParse(tab2, useInternalNodes = T)
    
    # Get all elevations, times and coordinates via the respective xpath
    coords <- XML::xpathSApply(pfile, path = "//rtept", XML::xmlAttrs)
    
    # Extract latitude and longitude from the coordinates
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    
    geodf <- data.frame(lat = lats, lon = lons)
    
    if (sp==TRUE){
      if(!'package:sp' %in% search()){
        attachNamespace('sp')
      }
      routeLines <- sp::Lines(slinelist = sp::Line(geodf[,2:1]), 
                              ID = "x")
      routeSL <- sp::SpatialLines(LinesList = list(routeLines), 
                                  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(ido = ido, idd = idd)
      geodf <- sp::SpatialLinesDataFrame(routeSL, 
                                         data = df, 
                                         match.ID = FALSE)   
      row.names(geodf) <- paste(ido,idd,sep="_")

    }
    
    return(geodf)
  }, error=function(e) { message("osrmViarouteGeom function returns an error: \n", e)})
  return(NULL)
}







