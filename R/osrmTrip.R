#' @name osrmTrip
#' @title Get the Travel Geometry Between Multiple Unordered Points
#' @description Build and send an OSRM API query to get the shortest travel geometry between multiple points.
#' This function interfaces the \emph{trip} OSRM service. 
#' @param loc a SpatialPointsDataFrame of the waypoints, or a data.frame with points as rows
#' and 3 columns: identifier, longitudes and latitudes (WGS84 decimal degrees).
#' @param overview "full", "simplified". Add geometry either full (detailed) or simplified 
#' according to highest zoom level it could be display on. 
#' @details As stated in the OSRM API, if input coordinates can not be joined by a single trip 
#' (e.g. the coordinates are on several disconnecte islands) multiple trips for 
#' each connected component are returned.
#' @return A list of connected components. Each component contains:
#' @return \describe{
#' \item{trip}{A SpatialLinesDataFrame (loc's CRS if there is one, WGS84 else)
#' containing a line for each step of the trip.}
#' \item{summary}{A list with 2 components: duration (in minutes)
#' and distance (in kilometers).}
#' }
#' @seealso \link{osrmRoute}
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Get a trip with a id lat lon data.frame
#' trips <- osrmTrip(loc = com[1101:1150, c(1,3,4)])
#' 
#' # Display the trip
#' plot(trips[[1]]$trip , col = 1:5)
#' points(com[1101:1150, 3:4], pch = 20, col = "red", cex = 0.5)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(osm)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE)
#'   points(com[1101:1150, 3:4], pch = 20, col = "red", cex = 0.5)
#' }
#' 
#' # Get a trip with a SpatialPointsDataFrame
#' trips <- osrmTrip(loc = src)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(osm)
#'   plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE, lwd=2)
#' }
#' }
osrmTrip <- function(loc, overview = "simplified"){
  tryCatch({
    # check if inpout is sp, transform and name columns
    oprj <- NA
    if (testSp(loc)) {
      oprj <- sp::proj4string(loc)
      loc <- spToDf(x = loc)
    }else{
      names(loc) <- c("id", "lon", "lat")
    }
    
    # Build the query
    req <- paste(getOption("osrm.server"),
                 "trip/v1/", getOption("osrm.profile"), "/polyline(", 
                 gepaf::encodePolyline(loc[,c("lat","lon")]),
                 ")?steps=false&geometries=geojson&overview=",
                 tolower(overview), sep = "")
    # Send the query
    ua <- "'osrm' R package"
    resRaw <- RCurl::getURL(utils::URLencode(req), useragent = ua)


    # if (resRaw=="") {
    #   stop("OSRM returned an empty string.", call. = FALSE)
    # }
    # 
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if (res$code != "Ok") {stop(e)}
    
    # Get all the waypoints
    waypointsg <- data.frame(res$waypoints[,c(1,2,5)], 
                             matrix(unlist(res$waypoints$location), 
                                    byrow = T, ncol = 2), id = loc$id)
    
    # In case of island, multiple trips
    ntour <- dim(res$trips)[1]
    trips <- vector("list", ntour)

    for (nt in 1:ntour) {
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
        wktl[i] <- paste("LINESTRING(",
                         paste(geodf[indexes[i]:indexes[i + 1],1]," ",
                               geodf[indexes[i]:indexes[i + 1],2], 
                               sep = "", collapse = ",")
                         ,")",sep = "")
      }
      wkt <- paste("GEOMETRYCOLLECTION(", paste(wktl, collapse = ","),")", sep = "")
      sl <- rgeos::readWKT(wkt)
      sl@proj4string <- sp::CRS("+init=epsg:4326")
      start <- (waypoints[order(waypoints$waypoint_index, decreasing = F),"id"])
      end <- start[c(2:length(start),1)]
      df <- data.frame(start, end, 
                       duration = res$trips[nt,]$legs[[1]][,"duration"] / 60, 
                       distance = res$trips[nt,]$legs[[1]][,"distance"] / 1000)
      sldf <- sp::SpatialLinesDataFrame(sl = sl, data = df, match.ID = F)
      
      # Reproj
      if (!is.na(oprj)) {
        sldf <- sp::spTransform(sldf, oprj)
      }
      
      # Build tripSummary
      tripSummary <- list(duration = res$trips[nt,]$duration/60,
                          distance = res$trips[nt,]$distance/1000)   
      
      trips[[nt]] <- list(trip = sldf, summary = tripSummary)
    }
    return(trips)
  }, error = function(e) { message("OSRM returned an error:\n", e)})
  return(NULL)
}



