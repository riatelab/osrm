#' @name osrmViaroute
#' @title Get Travel Time and Travel Distance Between Two Points
#' @description Build and send an OSRM API query to get travel time and
#' travel distance between two points.
#' This function interfaces the \emph{viaroute} OSRM service.
#' @param src a numeric vector of latitude and longitude (WGS84), a
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine
#' point.
#' @param dst a numeric vector of latitude and longitude (WGS84), a
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the
#' destination point.
#' @return A named numeric vector is returned. It contains travel time
#' (in minutes) and travel distance (in kilometers).
#' @seealso \link{osrmViarouteGeom}
#' @noRd
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' # Time and Distance between 2 points
#' route <- osrmViaroute(src = com[1,c("lat","lon")],
#'                       dst = com[15,c("lat","lon")] )
#' # Time travel distance (min)
#' route[1]
#' # Travel distance (km)
#' route[2]
#' # Mean Speed (km/h)
#' route[2]/(route[1]/60)
#' 
#' # SpatialPointsDataFrame input
#' route2 <- osrmViaroute(src = src[1,], dst = dst[5,])
#' route
#' 
#' }
# osrmViaroute <- function(src, dst){
#   tryCatch({
# 
#     if(testSp(src)){
#       src <- src[1,]
#       x <- spToDf(x = src)
#       src <- c(x$loc[1,2], x$loc[1,3])
#     }
#     if(testSp(dst)){
#       dst <- dst[1,]
#       x <- spToDf(x = dst)
#       dst <- c(x$loc[1,2], x$loc[1,3])
#     }
# 
# 
#     # Query build
#     req <- paste(getOption("osrm.server"), "viaroute?loc=",
#                  src[1], ",", src[2], "&loc=",dst[1],",",dst[2],
#                  "&alt=false&geometry=false",sep="")
# 
#     # Sending the query
#     resRaw <- RCurl::getURL(utils::URLencode(req),
#                             useragent = "'osrm' R package")
# 
#     # JSON parsing
#     res <- jsonlite::fromJSON(resRaw)
# 
#     e <- simpleError(res$status_message)
#     if(res$status != "200"){stop(e)}
# 
#     res <- round(c(total_time=res$route_summary$total_time/60,
#                    total_distance=res$route_summary$total_distance/1000),2)
# 
#     return(res)
#   }, error=function(e) { message("osrmViaroute function returns an error: \n", e)})
#   return(NULL)
# }
# 

