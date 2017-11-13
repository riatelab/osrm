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
#' @param gepaf a boolean indicating if coordinates are sent encoded with the
#' google encoded algorithm format (TRUE) or not (FALSE). Must be FALSE if using
#' the public OSRM API.
#' @return A list containing 3 data frames is returned. 
#' durations is the matrix of travel times (in minutes), 
#' sources and destinations are the coordinates of 
#' the origin and destination points actually used to compute the travel 
#' times (WGS84).
#' @details If loc, src or dst are data frames we assume that the 3 first 
#' columns of the data.frame are: identifiers, longitudes and latitudes. 
#' @note 
#' If you want to get a large number of distances make sure to set the 
#' "max-table-size" argument (Max. locations supported in table) of the OSRM 
#' server accordingly.
#' @seealso \link{osrmIsochrone}
#' @examples
#' \dontrun{
#' # Load data
#' data("berlin")
#' 
#' # Inputs are data frames
#' # Travel time matrix
#' distA <- osrmTable(loc = apotheke.df[1:50, c("id","lon","lat")])
#' # First 5 rows and columns
#' distA$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distA2 <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
#'                     dst = apotheke.df[11:20,c("id","lon","lat")])
#' # First 5 rows and columns
#' distA2$durations[1:5,1:5]
#' 
#' # Inputs are SpatialPointsDataFrames
#' distA3 <- osrmTable(loc = apotheke.sp[1:10,])
#' # First 5 rows and columns
#' distA3$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distA4 <- osrmTable(src = apotheke.sp[1:10,], dst = apotheke.sp[11:20,])
#' # First 5 rows and columns
#' distA4$durations[1:5,1:5]
#' }
#' @export
osrmTable <- function(loc, src = NULL, dst = NULL, gepaf = FALSE){
  tryCatch({
    if (is.null(src)){
      # check if inpout is sp, transform and name columns
      if(testSp(loc)){
        loc <- spToDf(x = loc)
      }else{
        names(loc) <- c("id", "lon", "lat")
      }
      # Format
      src <- loc
      dst <- loc
      # Build the query
      req <- tableLoc(loc = loc, gepaf = gepaf)
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

      # Build the query
      loc <- rbind(src, dst)

      req <- paste(tableLoc(loc = loc, gepaf = gepaf),
                   "?sources=", 
                   paste(0:(nrow(src)-1), collapse = ";"), 
                   "&destinations=", 
                   paste(nrow(src):(nrow(loc)-1), collapse = ";"), 
                   sep="")
    }
    
    req <- utils::URLencode(req)
    
    osrmLimit(nSrc = nrow(src), nDst = nrow(dst), nreq = nchar(req))
    
    
    # Get the result
    resRaw <- RCurl::getURL(req, 
                            useragent = "'osrm' R package")
    
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)

    # Check results
    e <- simpleError(paste0(res$code,"\n",res$message))
    if(res$code != "Ok"){stop(e)}

    # get the distance table
    durations <- distTableFormat(res = res, src = src, dst = dst)
    
    # get the coordinates
    coords <- coordFormat(res = res, src = src, dst = dst)
    
    return(list(durations = durations, 
                sources = coords$sources, 
                destinations = coords$destinations))
  }, error=function(e) {message("OSRM returned an error:\n", e)})
  return(NULL)
}

