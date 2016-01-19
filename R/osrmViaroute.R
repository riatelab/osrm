#' @name osrmViaroute
#' @title Get Travel Time and Travel Distance Between Two Points
#' @description Build and send an OSRM API query to get travel time and travel distance between two points.
#' This function interface the \emph{viaroute} OSRM service. 
#' @param src latitude and longitude of the origine point (numeric vector of length 2)
#' @param dst latitude and longitude of the destination point (numeric vector of length 2).
#' @return A named numeric vector is return. It contains travel time (in minutes) 
#' and travel distance (in kilometers).  
#' @seealso \link{osrmViarouteGeom}
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
#' }
#' @export
osrmViaroute <- function(src, dst){
  tryCatch({
    # Query build

    req <- paste(getOption("osrm.server"), "viaroute?loc=", 
                 src[1], ",", src[2], "&loc=",dst[1],",",dst[2], 
                 "&alt=false&geometry=false",sep="")
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req), 
                            useragent = "'osrm' R package")
    
    # JSON parsing
    res <- jsonlite::fromJSON(resRaw)
    
    e <- simpleError(res$status_message)
    if(res$status != "200"){stop(e)}
    
    res <- round(c(total_time=res$route_summary$total_time/60,
                   total_distance=res$route_summary$total_distance/1000),2)
    
    return(res)
  }, error=function(e) { message("osrmViaroute function returns an error: \n", e)})
  return(NULL)
}


