#' @name osrmTripGeom
#' @title Get the travel geometry between multiple unordered points.
#' @description Build and send an OSRM API query to get the shortest travel geometry between multiple points.
#' This function interface the \emph{trip} OSRM service. 
#' @param userPoints a SpatialPointsDataFrame of the waypoints, or a data.frame with points as rows
#' and at least 3 columns : id, lat and long (WGS84 decimal degrees)
#' @param sp if sp is TRUE the function returns a SpatialLinesDataFrame.
#' @return A list with components
#' @return \describe{
#' \item{tripPoints}{A data.frame with the points lat/long (WGS84) and the step they're part of}
#' \item{geom}{Only when sp is TRUE, a SpatialLinesDataFrame (userPoints' CRS if there's one, WGS84 else),
#' containing a line for each step of the trip.}
#' \item{summary}{A list with 4 components : startingPoint, endingPoint, time (in minutes)
#' and distance (in kilometers)}
#' }
#' @seealso \link{osrmViarouteGeom}
#' @examples
#' \dontrun{
#' }
#' @export

osrmTripGeom <- function(userPoints, sp = FALSE){
  tryCatch({
      oprj <- NA
      if(testSp(userPoints)){
        oprj <- sp::proj4string(userPoints)
        x <- spToDf(x = userPoints)
        coordsDF <- x$loc
      } else {
        coordsDF <- userPoints
      }

      locationsString <- paste(as.numeric(coordsDF$lat), as.numeric(coordsDF$lon), sep=",", collapse="&loc=")


    
    # build the query
    req <- paste(getOption("osrm.server"), 
                 "trip?loc=", locationsString,
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
    geodf <- data.frame(res$trips$route_geometry)
    names(geodf) <-  c("lat", "lon")
    
    tripSummary <- list(startingPoint  = res$trips$route_summary$start_point,
                    endingPoint = res$trips$route_summary$end_point,
                    time = res$trips$route_summary$total_time/60,
                    distance = res$trips$route_summary$total_distance/1000
                    )
    
    pointsOrder <- unlist(res$trips$permutation) + 1
    pointsIndexes <- unlist(res$trips$via_indices) + 1
    pointsNames <- c(coordsDF$id[pointsOrder], coordsDF$id[pointsOrder][1])
    geodf$step <- NA
    
    tripSegments <- list()
    segmentNames <- c()
    # Convert to SpatialLinesDataFrame
    if (sp==TRUE){
      if(!'package:sp' %in% search()){
        attachNamespace('sp')
      }
      for (i in 1:(length(pointsIndexes)-1) ){
        dfSegment <- geodf[pointsIndexes[i]:pointsIndexes[i+1],]
        segmentName <- paste(pointsNames[i], pointsNames[i+1], sep="->")
        
        segmentNames <- cbind(segmentNames, segmentName)
        
        geodf[pointsIndexes[i]:pointsIndexes[i+1],"step"] <- segmentName
        
        lineSegment <- sp::Line(dfSegment[,2:1])
        linesSegment <- sp::Lines(lineSegment,  ID = segmentName)
        tripSegments[length(tripSegments) + 1] <- linesSegment
        
      }
      tripSL <- sp::SpatialLines(tripSegments,  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(stringsAsFactors = FALSE, Name = as.character(segmentNames))
      
      sldf <- sp::SpatialLinesDataFrame(tripSL, 
                                        data = df, 
                                        match.ID = FALSE)   
      if (!is.na(oprj)){
        sldf <- sp::spTransform(sldf, oprj)
      }
      return(list(tripPoints = geodf, geom = sldf, summary = tripSummary))
    } else {
      for (i in 1:(length(pointsIndexes)-1) ){
        dfSegment <- geodf[pointsIndexes[i]:pointsIndexes[i+1],]
        segmentName <- paste(pointsNames[i], pointsNames[i+1], sep="->")
        
        segmentNames <- cbind(segmentNames, segmentName)
        
        geodf[pointsIndexes[i]:pointsIndexes[i+1],"step"] <- segmentName
      }
    }
    return(list(tripPoints = geodf, summary = tripSummary))
  }, error=function(e) { message("osrmTripGeom function returns an error: \n", e)})
  return(NULL)
}









