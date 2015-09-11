#' @name osrmViaroute
#' @title Query OSRM to Get a Travel Distance, Time and Geometry
#' @description Query the OSRM API to get a travel time matrix between
#' points. This function interface the OSRM `viaroute` service. 
#' @param xo longitude of the origine point.
#' @param yo latitude of the origine point.
#' @param xd longitude of the destination point.
#' @param yd latitude of the destination point.
#' @return a list is returned
#' @export
osrmViaroute <- function(xo, yo, xd, yd){
  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "viaroute?loc=", sep = "")
    
    tab <- paste(tab, yo, ",", xo, "&loc=",yd,",",xd, 
                 "&alt=false&geometry=false",sep="")
    
    # Sending the query
    tab2 <- RCurl::getURL(URLencode(tab), useragent = "'osmdistance' R package")
    
    # JSON parsing
    tab3 <- RJSONIO::fromJSON(tab2)
    
    if (tab3$status==0){
      res <- round(c(total_time=tab3$route_summary$total_time/60,
                     total_distance=tab3$route_summary$total_distance/1000),2)
    }
    
    return(res)
  }, error=function(e) { message("osrmViaroute function returns an error: \n", e)})
  return(NULL)
}


