#' @name osrmTrip
#' @title Get the Travel Geometry Between Multiple Unordered Points
#' @description Build and send an OSRM API query to get the shortest travel geometry between multiple points.
#' This function interfaces the \emph{trip} OSRM service. 
#' @param loc an sf object of the waypoints, or a data.frame with points as rows
#' and 3 columns: identifier, longitudes and latitudes (WGS84 decimal degrees).
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param overview "full", "simplified". Add geometry either full (detailed) or simplified 
#' according to highest zoom level it could be display on. 
#' @param returnclass deprecated. 
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @details As stated in the OSRM API, if input coordinates can not be joined by a single trip 
#' (e.g. the coordinates are on several disconnecte islands) multiple trips for 
#' each connected component are returned.
#' @return A list of connected components. Each component contains:
#' @return \describe{
#' \item{trip}{An sf LINESTRING (loc's CRS if there is one, WGS84 if not)
#' containing a line for each step of the trip.}
#' \item{summary}{A list with 2 components: duration (in minutes)
#' and distance (in kilometers).}
#' }
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"), 
#'                        quiet = TRUE)
#' # Get a trip with a set of points (sf POINT)
#' trips <- osrmTrip(loc = apotheke.sf[1:5, ])
#' mytrip <- trips[[1]]$trip
#' # Display the trip
#' plot(st_geometry(mytrip), col = "black", lwd = 4)
#' plot(st_geometry(mytrip), col = c("red", "white"), lwd = 1, add = TRUE)
#' plot(st_geometry(apotheke.sf[1:5, ]), pch = 21, bg = "red", cex = 1, 
#'      add = TRUE)
#' }
osrmTrip <- function(loc, exclude = NULL, overview = "simplified", 
                     returnclass, osrm.server = getOption("osrm.server"),
                     osrm.profile = getOption("osrm.profile")){
  
  
  opt <- options(error = NULL)
  on.exit(options(opt), add=TRUE)
  
  if(!missing(returnclass)){
    warning('"returnclass" is deprecated.', call. = FALSE)
  }
  
  url <- base_url(osrm.server, osrm.profile, "trip")
  
  
  #  transform and name columns
  oprj <- NA
  
  if(methods::is(loc,"sf")){
    oprj <- st_crs(loc)
    loc <- sf_2_df(x = loc)
  }else{
    names(loc) <- c("id", "lon", "lat")
  }
  

  url <- paste0(url, 
                paste(clean_coord(loc$lon) , clean_coord(loc$lat), 
                           sep=",",collapse = ";"), 
                "?steps=false&geometries=geojson&overview=", 
                tolower(overview))
  
  # adding exclude parameter
  if (!missing(exclude)) {url <- paste0(url, "&exclude=", exclude)}

  # Send the query
  e <- try({
    req_handle <- curl::new_handle(verbose = FALSE)
    curl::handle_setopt(req_handle, useragent = "osrm_R_package")
    r <- curl::curl_fetch_memory(utils::URLencode(url), handle = req_handle)
  }, silent = TRUE)
  if (inherits(e,"try-error")){ 
    stop(e, call. = FALSE)
  }
  
  # test result validity
  test_http_error(r)
  res <- RcppSimdJson::fparse(rawToChar(r$content))
  
  
  # return(res)
  # Get all the waypoints
  waypointsg <- data.frame(res$waypoints[,c(1,2,5)], 
                           matrix(unlist(res$waypoints$location), 
                                  byrow = T, ncol = 2), id = loc$id)
  
  # In case of island, multiple trips
  ntour <- dim(res$trips)[1]
  trips <- vector("list", ntour)
  
  for (nt in 1:ntour) {
    # nt=1
    # Coordinates of the line
    geodf <- data.frame(res$trips[nt,]$geometry[[1]]$coordinates)
    # In case of unfinnish trip
    if (geodf[nrow(geodf),1] != geodf[1,1]) {
      geodf <- rbind(geodf,geodf[1,])
    }
    geodf$ind <- 1:nrow(geodf)
    # Extract trip waypoints
    waypoints <- waypointsg[waypointsg$trips_index == (nt - 1),]
    
    # Get points order and indexes
    geodf <- merge(geodf, waypoints, 
                   by.x = c("X1", "X2"), by.y = c("X1","X2"), 
                   all.x = T)
    geodf <- geodf[order(geodf$ind, decreasing = F),]
    indexes <- geodf[!is.na(geodf$waypoint_index),"ind"]
    # xx <- geodf[!is.na(geodf$waypoint_index),]
    # indexes <- c(stats::aggregate(xx$ind, by  = list(xx$waypoint_index),
    #                               min)[,2], 
    #              nrow(geodf))
    # Build the polylines
    wktl <- rep(NA,nrow(waypoints))
    for (i in 1:(length(indexes) - 1)) {
      ind0 <- indexes[i]
      ind1 <- indexes[i+1]
      if(ind1==ind0){
        aind <- rep(ind0,2)
      }else{
        aind <- ind0:ind1
      }
      wktl[i] <- paste("LINESTRING(",
                       paste(geodf[aind,1]," ",
                             geodf[aind,2], 
                             sep = "", collapse = ",")
                       ,")",sep = "")
    }
    start <- (waypoints[order(waypoints$waypoint_index, decreasing = F),"id"])
    end <- start[c(2:length(start),1)]
    sldf <- st_sf(start = start, end = end, 
                  duration = res$trips[nt,]$legs[[1]][,"duration"] / 60, 
                  distance = res$trips[nt,]$legs[[1]][,"distance"] / 1000, 
                  geometry = st_as_sfc(wktl, crs = 4326))
    # Reproj
    if (!is.na(oprj)) {
      sldf <- sf::st_transform(sldf, oprj)
    }
    # Build tripSummary
    tripSummary <- list(duration = res$trips[nt,]$duration/60,
                        distance = res$trips[nt,]$distance/1000)   
    trips[[nt]] <- list(trip = sldf, summary = tripSummary)
  }
  return(trips)
  
}
