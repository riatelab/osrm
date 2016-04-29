#' @name osrmTrip
#' @title Get the Travel Geometry Between Multiple Unordered Points
#' @description Build and send an OSRM API query to get the shortest travel geometry between multiple points.
#' This function interfaces the \emph{trip} OSRM service. 
#' @param loc a SpatialPointsDataFrame of the waypoints, or a data.frame with points as rows
#' and 3 columns: identifier, longitudes and latitudes (WGS84 decimal degrees).
#' @param overview "full", "simplified" or FALSE. Add geometry either full (detailed), simplified 
#' according to highest zoom level it could be display on, or not at all. 
#' @details As stated in the OSRM API, if input coordinates can not be joined by a single trip 
#' (e.g. the coordinates are on several disconnecte islands) multiple trips for 
#' each connected component are returned.
#' @return A list of connected components. Each component contains:
#' @return \describe{
#' \item{trip}{A data.frame with the points lat/long (WGS84) and the step they are 
#' part of or, if sp is TRUE, a SpatialLinesDataFrame (loc's CRS if there is one, WGS84 else),
#' containing a line for each step of the trip.}
#' \item{summary}{A list with 4 components: startingPoint, endingPoint, time (in minutes)
#' and distance (in kilometers) or a message if a point is not connected to any trip}
#' }
#' @seealso \link{osrmRoute}
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Get a trip with a id lat lon data.frame
#' trips <- osrmTrip(loc = com[1101:1200, c(1,4,3)])
#' 
#' # Display the trip
#' plot(trips[[1]]$trip, col = 1:100)
#' points(com[1101:1200, 3:4], pch = 20, col = "red", cex = 0.5)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE)
#'   tilesLayer(osm)
#'   plot(trips[[1]]$trip, col = 1:100, add = TRUE)
#'   points(com[1101:1200, 3:4], pch = 20, col = "red", cex = 0.5)
#' }
#' 
#' 
#' # Get a trip with a SpatialPointsDataFrame
#' trips <- osrmTrip(loc = src)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE)
#'   tilesLayer(osm)
#'   plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
#'   plot(trips[[1]]$trip, add = TRUE, lwd=2)
#' }
#' }
osrmTrip <- function(loc, overview = "simplified"){
  tryCatch({
    data(com)
    loc = com[1:100, c(1,3,4)]
    options(osrm.server = "http://0.0.0.0:5000/")
    overview = "full"
    
    
    oprj <- NA
    if(testSp(loc)){
      oprj <- sp::proj4string(loc)
      loc <- spToDf(x = loc)
    }else{
      names(loc) <- c("id", "lon", "lat")
    }
    
    # build the query
    req <- paste(getOption("osrm.server"), 
                 "trip/v1/driving/polyline(", 
                 encodeToPolyline(loc[,c("lat","lon")]),
                 ")?steps=false&geometries=geojson&overview=",
                 tolower(overview), sep="")
    
    
    # Sending the query
    ua <- "'osrm' R package"
    resRaw <- RCurl::getURL(utils::URLencode(req), useragent = ua)
    
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}
    
    
    ntour <- dim(res$trips)[1]
    
    trips <- vector("list", ntour)
    
    
    # 
    # length(res$trips$legs[[1]])
    # 
    # res$trips$legs[[1]]$steps[[1]]$
    nt <- 1
    
    waypointsg <- data.frame(res$waypoints[,c(1,5)], 
                             matrix(unlist(res$waypoints$location), 
                                    byrow=T, ncol=2), id = loc$id)
    
    
    for (nt in 1:ntour){
      # Coordinates of the line
      geodf <- data.frame(res$trips[nt,]$geometry$coordinates)
      
      if(geodf[nrow(geodf),1]!=geodf[1,1]){
        geodf <- rbind(geodf,geodf[1,])
      }
      geodf$ind <- 1:nrow(geodf)
      waypoints <- waypointsg[waypointsg$trips_index==(nt-1),]
      geodf <- merge(geodf, waypoints, 
                     by.x=c("X1", "X2"), by.y=c("X1","X2"), 
                     all.x=T)
      
      geodf <- geodf[order(geodf$ind, decreasing = F),]
      xx <- geodf[!is.na(geodf$waypoint_index),]
      indexes <- c(1,(aggregate(xx$ind, by  = list(xx$waypoint_index), max)[,2]))
      
      wktl <- rep(NA,nrow(waypoints))
      for(i in 1:(length(indexes)-1)){
        wktl[i] <- paste("LINESTRING(",
                         paste(geodf[indexes[i]:indexes[i+1],1]," ",
                               geodf[indexes[i]:indexes[i+1],2], 
                               sep = "", collapse=",")
                         ,")",sep="")
      }
      
      wkt <- paste("GEOMETRYCOLLECTION(", paste(wktl, collapse=","),")", sep ="")
      sl <- rgeos::readWKT(wkt)
      sl@proj4string <- sp::CRS("+init=epsg:4326")
      start <- (waypoints[order(waypoints$waypoint_index, decreasing = F),"id"])
      end <- start[c(2:length(start),1)]
      sldf <- SpatialLinesDataFrame(sl = sl, 
                                    data = data.frame(start,end), 
                                    match.ID = F)

      plot(sldf[4,], col=1:length(sldf), lwd = seq(10,0.1, length.out = 100))
      if (nrow(geodf)==1){
        pointsOrder <- unlist(res$trips[nt,]$permutation) + 1
        trips[[nt]] <- list(trip = NA, 
                            summary = paste("This point (",
                                            coordsDF$id[pointsOrder],
                                            ") is not included in any trip.",
                                            " Please check its coordinates", 
                                            sep=""))
        
      }else{
        names(geodf) <-  c("lat", "lon")
        
        tripSummary <- list(startingPoint  = res$trips[nt,]$route_summary$start_point,
                            endingPoint = res$trips[nt,]$route_summary$end_point,
                            time = res$trips[nt,]$route_summary$total_time/60,
                            distance = res$trips[nt,]$route_summary$total_distance/1000)    
        
        pointsOrder <- unlist(res$trips[nt,]$permutation) + 1
        pointsIndexes <- unlist(res$trips[nt,]$via_indices) + 1
        pointsNames <- c(coordsDF$id[pointsOrder], coordsDF$id[pointsOrder][1])
        geodf$step <- NA
        
        tripSegments <- list()
        segmentNames <- c()
        
        # Convert to SpatialLinesDataFrame
        if (sp==TRUE){
          for (i in 1:(length(pointsIndexes)-1) ){
            dfSegment <- geodf[pointsIndexes[i]:pointsIndexes[i+1],]
            segmentName <- paste(pointsNames[i], pointsNames[i+1], sep="->")
            
            segmentNames <- cbind(segmentNames, segmentName)
            
            geodf[pointsIndexes[i]:pointsIndexes[i+1],"step"] <- segmentName
            
            lineSegment <- sp::Line(dfSegment[,2:1])
            linesSegment <- sp::Lines(lineSegment,  ID = segmentName)
            tripSegments[length(tripSegments) + 1] <- linesSegment
            
          }
          tripSL <- sp::SpatialLines(tripSegments,  
                                     proj4string = sp::CRS("+init=epsg:4326"))
          df <- data.frame(stringsAsFactors = FALSE, 
                           Name = as.character(segmentNames))
          
          sldf <- sp::SpatialLinesDataFrame(tripSL, 
                                            data = df, 
                                            match.ID = FALSE)   
          if (!is.na(oprj)){
            sldf <- sp::spTransform(sldf, oprj)
          }
          trips[[nt]] <- list(trip = sldf, summary = tripSummary)
          
        } else {
          for (i in 1:(length(pointsIndexes)-1) ){
            dfSegment <- geodf[pointsIndexes[i]:pointsIndexes[i+1],]
            segmentName <- paste(pointsNames[i], pointsNames[i+1], sep="->")
            
            segmentNames <- cbind(segmentNames, segmentName)
            
            geodf[pointsIndexes[i]:pointsIndexes[i+1],"step"] <- segmentName
          }
          trips[[nt]] <- list(trip = geodf, summary = tripSummary)
        }
        
      }
    }
    return(trips)
  }, error=function(e) { message("osrmTrip function returns an error: \n", e)})
  return(NULL)
}

loc







