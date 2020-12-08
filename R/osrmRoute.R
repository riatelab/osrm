#' @name osrmRoute
#' @title Get the Shortest Path Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry 
#' between two points. This function interfaces the \emph{route} OSRM service. 
#' @param src a vector of identifier, longitude and latitude (WGS84), a vector 
#' of longitude and latitude (WGS84), a SpatialPointsDataFrame, a 
#' SpatialPolygonsDataFrame or an sf object of the origine point.
#' @param dst a vector of identifier, longitude and latitude (WGS84), a vector 
#' of longitude and latitude (WGS84), a SpatialPointsDataFrame, a 
#' SpatialPolygonsDataFrame or an sf object of the destination point.
#' @param loc a data.frame of identifier, longitude and latitude (WGS84), a 
#' SpatialPointsDataFrame, a SpatialPolygonsDataFrame or an sf object of via 
#' points. The first row is the origine, the last row is the destination.
#' @param overview "full", "simplified" or FALSE. Use "full" to return the 
#' detailed geometry, use "simplified" to return a simplified geometry, use 
#' FALSE to return only time and distance.
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param sp deprecated, if sp==TRUE the function returns a SpatialLinesDataFrame.
#' @param returnclass if returnclass="sf" an sf LINESTRING is returned. 
#' If returnclass="sp" a SpatialLineDataFrame is returned. If returnclass is not 
#' set a data.frame of coordinates is returned. 
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @return
#' If returnclass is not set, a data frame is returned. It contains the 
#' longitudes and latitudes of the travel path between the two points.\cr
#' If returnclass is set to "sp", a SpatialLinesDataFrame is returned. \cr
#' If returnclass is set to "sf", an sf LINESTRING is returned. \cr
#' SpatialLinesDataFrame and sf LINESTRING contain 4 fields: identifiers of 
#' origine and destination, travel time in minutes and travel distance in 
#' kilometers.\cr\cr
#' If overview is FALSE, a named numeric vector is returned. It contains travel 
#' time (in minutes) and travel distance (in kilometers).
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
#' route4 <-  osrmRoute(src = c(13.412, 52.502), 
#'                      dst = c(13.454, 52.592),
#'                      returnclass = "sf")
#' plot(st_geometry(route4))
#' 
#' # Using via points
#' pts <- structure(
#'  list(x = c(13.32500, 13.30688, 13.30519, 13.31025, 
#'             13.4721, 13.56651, 13.55303, 13.37263, 13.50919, 13.5682), 
#'       y = c(52.40566, 52.44491, 52.52084, 52.59318, 52.61063, 52.55317, 
#'             52.50186, 52.49468, 52.46441, 52.39669)), 
#'  class = "data.frame", row.names = c(NA, -10L))
#' route5 <- osrmRoute(loc = pts, returnclass = "sf")
#' plot(st_geometry(route5), col = "red", lwd = 2)
#' points(pts, pch = 20, cex = 2)
#' 
#' # Using a different routing server
#' u <- "https://routing.openstreetmap.de/routed-foot/"
#' route5 <- osrmRoute(apotheke.sf[1, ], apotheke.df[16, ], returnclass="sf", 
#'                     osrm.server = u)
#' 
#' # Using an open routing service with support for multiple modes
#' # see https://github.com/rCarto/osrm/issues/67
#' u <- "https://routing.openstreetmap.de/"
#' options(osrm.server = u)
#' route6 <- osrmRoute(apotheke.sf[1, ], apotheke.df[16, ], returnclass="sf", 
#'                     osrm.profile = "bike")
#' route7 <- osrmRoute(apotheke.sf[1, ], apotheke.df[16, ], returnclass="sf", 
#'                     osrm.profile = "car")
#' plot(st_geometry(route5), col = "green")
#' plot(st_geometry(route6), add = TRUE) # note the cycle route has fewer turns
#' plot(st_geometry(route7), col = "red", add = TRUE) # car route, indirect = good!
#' }
#' @export
osrmRoute <- function(src, dst, loc, overview = "simplified", exclude = NULL,
                      sp, returnclass,
                      osrm.server = getOption("osrm.server"),
                      osrm.profile = getOption("osrm.profile")){
  if(!missing(sp)){
    warning("sp is deprecated; use returnclass instead.", call. = FALSE)
    if(sp){
      returnclass <- "sp"
    }
  }
  
  exclude_str <- ""
  
  if(osrm.server == "https://routing.openstreetmap.de/") {
    osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
    osrm.profile = "driving"
  }
  
  if(missing(loc)){
    # From src to dst
    src <- input_route(x = src, id = "src", single = TRUE)
    dst <- input_route(x = dst, id = "dst", single = TRUE)
    id1 <- src$id
    id2 <- dst$id
    oprj <- src$oprj
    if (!is.null(exclude)) {exclude_str <- paste("&exclude=", exclude, sep = "")}
    req <- paste(osrm.server,
                 "route/v1/", 
                 osrm.profile, "/", 
                 src$lon, ",", src$lat, ";", dst$lon, ",", dst$lat, 
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview), exclude_str, sep="")
  }else{
    # from src to dst via x, y, z... (data.frame or sf input)
    loc <- input_route(x = loc, single = FALSE)
    oprj <- loc$oprj
    id1 <- loc$id1
    id2 <- loc$id2
    if (!is.null(exclude)) {exclude_str <- paste("&exclude=", exclude, sep = "")}
    req <- paste(osrm.server,
                 "route/v1/", 
                 osrm.profile, "/", 
                 paste0(apply(data.frame(loc$lon, loc$lat), 
                              MARGIN = 1, FUN = paste0, collapse=","),
                        collapse=";"),
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview), exclude_str, sep="")
  }  
  # print(req)
  
  tryCatch({
    # Sending the query
    resRaw <- RCurl::getURL(url = utils::URLencode(req), 
                            useragent = "'osrm' R package")
    # Deal with \\u stuff
    vres <- jsonlite::validate(txt = resRaw)[1]
    if(!vres){
      resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
    }
    
    # Parse the results
    res <- jsonlite::fromJSON(txt = resRaw)
    
    # Error handling
    if(is.null(res$code)){
      e <- simpleError(message = res$message)
      stop(e)
    }else{
      e <- simpleError(paste0(res$code,"\n",res$message))
      if(res$code != "Ok"){stop(e)}
    }
    
    if (overview == FALSE){
      return(round(c(duration = res$routes$duration / 60,
                     distance = res$routes$distance / 1000), 2))
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
      rosf <- st_sf(src = id1, dst = id2,
                    duration = res$routes$duration / 60,
                    distance = res$routes$distance / 1000,
                    geometry = rgeom, crs = 4326, 
                    row.names = paste(id1, id2, sep = "_"))
      # prj
      if (!is.na(oprj)){
        rosf <- st_transform(rosf, oprj)
      }
      
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

