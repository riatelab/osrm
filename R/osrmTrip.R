#' @name osrmTrip
#' @title Get the Travel Geometry Between Multiple Unordered Points
#' @description Build and send an OSRM API query to get the shortest travel geometry between multiple points.
#' This function interfaces the \emph{trip} OSRM service. 
#' @param loc a SpatialPointsDataFrame or an sf object of the waypoints, or a data.frame with points as rows
#' and 3 columns: identifier, longitudes and latitudes (WGS84 decimal degrees).
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param overview "full", "simplified". Add geometry either full (detailed) or simplified 
#' according to highest zoom level it could be display on. 
#' @param returnclass if returnclass="sf" an sf LINESTRING is returned. 
#' If returnclass="sp" a SpatialLineDataFrame is returned.
#' @details As stated in the OSRM API, if input coordinates can not be joined by a single trip 
#' (e.g. the coordinates are on several disconnecte islands) multiple trips for 
#' each connected component are returned.
#' @return A list of connected components. Each component contains:
#' @return \describe{
#' \item{trip}{A SpatialLinesDataFrame or sf LINESTRING (loc's CRS if there is one, WGS84 if not)
#' containing a line for each step of the trip.}
#' \item{summary}{A list with 2 components: duration (in minutes)
#' and distance (in kilometers).}
#' }
#' @seealso \link{osrmRoute}
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("berlin")
#' library(sf)
#' # Get a trip with a set of points (sf POINT)
#' trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf")
#' mytrip <- trips[[1]]$trip
#' # Display the trip
#' plot(st_geometry(mytrip), col = "black", lwd = 4)
#' plot(st_geometry(mytrip), col = c("red", "white"), lwd = 1, add = TRUE)
#' plot(st_geometry(apotheke.sf), pch = 21, bg = "red", cex = 1, add = TRUE)
#' }
osrmTrip <- function(loc, exclude = NULL, overview = "simplified", returnclass="sp"){
  tryCatch({
    # check if inpout is sp, transform and name columns
    oprj <- NA
    if (methods::is(loc,"Spatial")){
      loc <- sf::st_as_sf(x = loc)
    }
    
    if(methods::is(loc,"sf")){
      oprj <- st_crs(loc)
      loc <- sfToDf(x = loc)
    }else{
      names(loc) <- c("id", "lon", "lat")
    }
    
    exclude_str <- ""
    if (!is.null(exclude)) { exclude_str <- paste("&exclude=", exclude, sep = "") }
    
    req <- paste(getOption("osrm.server"),
                 "trip/v1/", getOption("osrm.profile"), "/", 
                 paste(clean_coord(loc$lon) , clean_coord(loc$lat), 
                   sep=",",collapse = ";"),
                 "?steps=false&geometries=geojson&overview=",
                 tolower(overview), exclude_str, sep = "")
    
    osrmLimit(nSrc = nrow(loc), nDst = 0, nreq=1)
    # Send the query
    ua <- "'osrm' R package"
    resRaw <- RCurl::getURL(utils::URLencode(req), useragent = ua)
    
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
    
    # Get all the waypoints
    waypointsg <- data.frame(res$waypoints[,c(1,2,5)], 
                             matrix(unlist(res$waypoints$location), 
                                    byrow = T, ncol = 2), id = loc$id)
    
    # In case of island, multiple trips
    ntour <- dim(res$trips)[1]
    trips <- vector("list", ntour)
    
    for (nt in 1:ntour) {
      nt=1
      # Coordinates of the line
      geodf <- data.frame(res$trips[nt,]$geometry$coordinates)
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
      indexes2 <- geodf[!is.na(geodf$waypoint_index),"ind"]
      xx <- geodf[!is.na(geodf$waypoint_index),]
      indexes <- c(stats::aggregate(xx$ind, by  = list(xx$waypoint_index),
                                    min)[,2], 
                   nrow(geodf))
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
      # ouptut mgmt
      if(returnclass=="sp"){
        sldf <- methods::as(sldf, "Spatial")
      }
      # Build tripSummary
      tripSummary <- list(duration = res$trips[nt,]$duration/60,
                          distance = res$trips[nt,]$distance/1000)   
      trips[[nt]] <- list(trip = sldf, summary = tripSummary)
    }
    return(trips)
  }, error = function(e) { message("The OSRM server returned an error:\n", e)})
  return(NULL)
}
