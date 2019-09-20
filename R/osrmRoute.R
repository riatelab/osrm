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
#' @return If returnclass is not set, a data frame is returned. It contains the longitudes and latitudes of 
#' the travel path between the two points.\cr
#' If returnclass is set to "sp", a SpatialLinesDataFrame is returned. If returnclass is set to "sf", 
#' an sf LINESTRING is returned. It contains 4 fields : 
#' identifiers of origine and destination, travel time in minutes and travel distance in 
#' kilometers.\cr
#' If overview is FALSE, a named numeric vector is returned. It contains travel time (in minutes) 
#' and travel distance (in kilometers).
#' @importFrom sf st_as_sfc st_crs st_geometry st_sf st_as_sf st_transform
#' @examples
#' \dontrun{
#' # Load data
#' data("berlin")
#' library(sf)
#' # Travel path between points
#' route1 <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
#'                     returnclass="sf")
#' # Travel path between points excluding motorways
#' route2 <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
#'                     returnclass="sf", exclude = "motorway")
#' # Display paths
#' plot(st_geometry(route1))
#' plot(st_geometry(route2), col = "red", add = TRUE)
#' plot(st_geometry(apotheke.sf[c(1,16),]), col = "red", pch = 20, add = TRUE)
#' 
#' # Return only duration and distance
#' route3 <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
#'                     overview = FALSE)
#' route3
#' 
#' # Using only coordinates
#' route4 <-  osrmRoute(src = c( 13.412, 52.502), 
#'                      dst = c( 13.454, 52.592),
#'                      returnclass = "sf")
#' plot(st_geometry(route4))
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
    
    # input mgmt
    oprj <- NA
    if(testSp(src)){
      src <- st_as_sf(src[1,])
    }    
    if(testSf(src)){
      oprj <- st_crs(src)
      x <- sfToDf(x = src)
      src <- c(x[1,1],x[1,2], x[1,3])
    }
    if(testSp(dst)){
      dst <- st_as_sf(dst[1,])
    }
    if(testSf(dst)){
      oprj <- st_crs(dst)
      x <- sfToDf(x = dst)
      dst <- c(x[1,1],x[1,2], x[1,3])
    }
    
    if(length(src)==2){src <- c("src", src)}
    if(length(dst)==2){dst <- c("dst", dst)}    
    
    exclude_str <- ""
    if (!is.null(exclude)) {exclude_str <- paste("&exclude=", exclude, sep = "") }
    # build the query
    req <- paste(getOption("osrm.server"),
                 "route/v1/", getOption("osrm.profile"), "/", 
                 format(src[2],scientific = FALSE, trim = TRUE), ",", 
                 format(src[3],scientific = FALSE, trim = TRUE), ";", 
                 format(dst[2],scientific = FALSE, trim = TRUE),",",
                 format(dst[3],scientific = FALSE, trim = TRUE), 
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
      # Deal with \\u stuff
      res$routes$geometry <- gsub(pattern = "zorglub", replacement = "\\\\",
                                  x = res$routes$geometry)
    }
    # Coordinates of the line
    geodf <- gepaf::decodePolyline(res$routes$geometry)[,c(2,1)]
    
    # Convert to LINESTRING
    if (!missing(returnclass)){
      rcoords <- paste0(geodf$lon, ' ', geodf$lat, collapse = ", ")
      rgeom <- (st_as_sfc(paste0("LINESTRING(",rcoords,")")))
      rosf <- st_sf(src = src[1], dst = dst[1],
                    duration = res$routes$legs[[1]]$duration/60,
                    distance = res$routes$legs[[1]]$distance/1000,
                    geometry = rgeom, crs = 4326)
      row.names(rosf) <- paste(src[1], dst[1],sep="_")
      if (!is.na(oprj)){
        rosf <- st_transform(rosf, oprj)
      }
      names(rosf)[1:2] <- c("src", "dst")
      # output mgmnt
      if(returnclass=="sp"){
        rosf <- methods::as(rosf, "Spatial")
      }
      return(rosf)
    }
    return(geodf)
  }, error=function(e) {message("The OSRM server returned an error:\n", e)})
  return(NULL)
}


