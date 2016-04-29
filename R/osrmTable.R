#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices 
#' between points. This function interfaces the \emph{table} OSRM service. 
#' @param loc a data frame containing 3 fields: points identifiers, longitudes 
#' and latitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers.
#' If loc parameter is used, all pair-wise distances are computed.
#' @param src a data frame containing origin points identifiers, longitudes 
#' and latitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers. 
#' If dst and src parameters are used, only pairs between scr/dst are computed.
#' @param dst a data frame containing destination points identifiers, longitudes 
#' and latitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers. 
#' @return A list containing 3 data frames is returned. 
#' durations is the matrix of travel times (in minutes), 
#' sources and destinations are the coordinates of 
#' the origin and destination points actually used to compute the travel 
#' times (WGS84).
#' @details If loc, src or dst are data frames we assume that the 3 first 
#' columns of the data.frame are: identifiers, longitudes and latitudes. 
#' @note The public OSRM API does not allow more than 10 000 
#' distances in query result. \cr
#' If you use an other OSRM API service, make sure that 
#' more distances are allowed in results 
#' (i.e. the "max-table-size" argument, Max. locations supported in distance 
#' table query).
#' @seealso \link{osrmIsochrone}
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Inputs are data frames  
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50, c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,c("comm_id","lon","lat")],
#'                       dst = com[11:20,c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' 
#' # Inputs are SpatialPointsDataFrames
#' distCom <- osrmTable(loc = src)
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = src, dst = dst)
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' }
#' @export
osrmTable <- function(loc, src = NULL, dst = NULL){
  tryCatch({
    if (is.null(src)){
      # check if inpout is sp, transform and name columns
      if(testSp(loc)){
        loc <- spToDf(x = loc)
      }else{
        names(loc) <- c("id", "lon", "lat")
      }
      
      # Check query size
      osrmLimit(nSrc = nrow(loc), nDst = nrow(loc))
      
      # Format
      src <- loc
      dst <- loc
      print(tableLoc(loc = loc))
      # Build the query
      req <- tableLoc(loc = loc)
    }else{
      # check if inpout is sp, transform and name columns
      if(testSp(src)){
        src <- spToDf(x = src)
      }else{
        names(src) <- c("id", "lon", "lat")
      }
      # check if inpout is sp, transform and name columns
      if(testSp(dst)){
        dst <- spToDf(x = dst)
      }else{
        names(dst) <- c("id", "lon", "lat")
      }
      
      # Check query size
      osrmLimit(nSrc = nrow(src), nDst = nrow(dst))
      
      # Build the query
      loc <- rbind(src, dst)

      req <- paste(tableLoc(loc = loc),
                   "?sources=", 
                   paste(0:(nrow(src)-1), collapse = ";"), 
                   "&destinations=", 
                   paste(nrow(src):(nrow(loc)-1), collapse = ";"), 
                   sep="")
    }

    # Get the result
    resRaw <- RCurl::getURL(utils::URLencode(req), 
                            useragent = "'osrm' R package")
    # Error if URL is too long
    e <- simpleError("The URL sent to the OSRM public API is too long. Enter less 
         sources or destinations.")
    if(getOption("osrm.server") == "http://router.project-osrm.org/" & resRaw==""){
      stop(e)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Check results
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}
    
    # get the distance table
    durations <- distTableFormat(res = res, src = src, dst = dst)
    
    # get the coordinates
    coords <- coordFormat(res = res, src = src, dst = dst)
    
    return(list(durations = durations, 
                sources = coords$sources, 
                destinations = coords$destinations))
  }, error=function(e) {message("osrmTable function returns an error: \n", e)})
  return(NULL)
}

