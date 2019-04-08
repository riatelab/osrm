#' @name osrmRoute
#' @title Get the Shortest Path Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry between two points.
#' This function interfaces the \emph{route} OSRM service. 
#' @param src a numeric vector of identifier, longitude and latitude (WGS84), a 
#' SpatialPointsDataFrame, a SpatialPolygonsDataFrame or an sf object of the origine 
#' point.
#' @param dst a numeric vector of identifier, longitude and latitude (WGS84), a 
#' SpatialPointsDataFrame, a SpatialPolygonsDataFrame or an sf object of the destination 
#' point.
#' @param overview "full", "simplified" or FALSE. Add geometry either full (detailed), simplified 
#' according to highest zoom level it could be display on, or not at all. 
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param sp deprecated, if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @param returnclass if returnclass="sf" an sf LINESTRING is returned. 
#' If returnclass="sp" a SpatialLineDataFrame is returned.
#' @return If returnclass, a data frame is returned. It contains the longitudes and latitudes of 
#' the travel path between the two points.\cr
#' If returnclass is TRUE a SpatialLinesDataFrame or an sf LINESTRING is returned. It contains 4 fields : 
#' identifiers of origine and destination, travel time in minutes and travel distance in 
#' kilometers.\cr
#' If overview is FALSE, a named numeric vector is returned. It contains travel time (in minutes) 
#' and travel distance (in kilometers).
#' @importFrom sf st_as_sfc st_crs st_geometry st_sf
#' @examples
#' \dontrun{
#' # Load data
#' data("berlin")
#' 
#' # Travel path between points
#' route <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
#'                    dst = apotheke.df[16, c("id", "lon","lat")])
#' # Travel path between points excluding motorways
#' route2 <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
#'                     dst = apotheke.df[16, c("id", "lon","lat")], 
#'                     exclude = "motorway")
#' # Display paths
#' plot(route[,1:2], type = "l", lty = 2, asp =1)
#' points(route2[,1:2], type = "l", lty = 2, asp = 1, col = "red")
#' points(apotheke.df[c(1,16),2:3], col = "red", pch = 20, cex = 1.5)
#' text(apotheke.df[c(1,16),2:3], labels = c("A","B"), pos = c(1, 2))
#' 
#' 
#' # Travel path between points between points - output a SpatialLinesDataFrame
#' route3 <- osrmRoute(src = c("A", 13.43853, 52.47728),
#'                     dst = c("B", 13.32247, 52.48882),
#'                     sp = TRUE, overview = "full")
#' # Travel path between points between points - output a SpatialLinesDataFrame 
#' # excluding motorways
#' route4 <- osrmRoute(src = c("A", 13.43853, 52.47728),
#'                     dst = c("B", 13.32247, 52.48882),
#'                     sp = TRUE, overview = "full", exclude = "motorway")
#' # Display the path
#' library(sp)
#' plot(route3, lty = 2, asp = 1)
#' plot(route4, lty = 2, asp = 1, col = "red", add = T)
#' points(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
#'        col = "red", pch = 20, cex = 1.5)
#' text(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
#'      labels = c("A","B"), pos = c(1, 2))
#' 
#' 
#' # Input is SpatialPointsDataFrames
#' route5 <- osrmRoute(src = apotheke.sp[1,], dst = apotheke.sp[2,], sp = TRUE)
#' route5@data
#' }
#' @export
osrmRoute <- function(src, dst, overview = "simplified", exclude = NULL,
                      sp, returnclass){
  tryCatch({
    if(!missing(sp)){
      warning("sp is deprecated; use returnclass instead.", call. = FALSE)
      if(sp){
        returnclass <- "sp"
      }
    }
    
    oprj <- NA
    if(testSp(src)){
      src <- sf::st_as_sf(src[1,])
    }    
    if(testSf(src)){
      oprj <- sf::st_crs(src)
      x <- sfToDf(x = src)
      src <- c(x[1,1],x[1,2], x[1,3])
    }
    if(testSp(dst)){
      dst <- sf::st_as_sf(dst[1,])
    }
    if(testSf(dst)){
      oprj <- sf::st_crs(dst)
      x <- sfToDf(x = dst)
      dst <- c(x[1,1],x[1,2], x[1,3])
    }
    
    if (!is.null(exclude)) {exclude_str <- paste("&exclude=", exclude, sep = "") }
    # build the query
    req <- paste(getOption("osrm.server"),
                 "route/v1/", getOption("osrm.profile"), "/", 
                 src[2], ",", src[3], ";", dst[2],",",dst[3], 
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview), exclude_str, sep="")
    
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
    if(is.null(res$code)){
      e <- simpleError(res$message)
      stop(e)
    }else{
      e <- simpleError(paste0(res$code,"\n",res$message))
      if(res$code != "Ok"){stop(e)}
    }
    
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
    if (!missing(returnclass)){
      rcoords <- paste0(geodf$lon, ' ', geodf$lat, collapse = ", ")
      rgeom <- (st_as_sfc(paste0("LINESTRING(",rcoords,")")))
      rosf <- st_sf(src = src[1], dst = dst[1],
                    duration = res$routes$legs[[1]]$duration/60,
                    distance = res$routes$legs[[1]]$distance/1000,
                    geometry = rgeom, crs = 4326)
      row.names(rosf) <- paste(src[1], dst[1],sep="_")
      if (!is.na(oprj)){
        rosf <- sf::st_transform(rosf, oprj)
      }
      names(rosf)[1:2] <- c("src", "dst")
      if(returnclass=="sp"){
        rosf <- methods::as(rosf, "Spatial")
      }
    }
    return(rosf)
  }, error=function(e) {message("The OSRM server returned an error:\n", e)})
  return(NULL)
}


