#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices 
#' between points. This function interfaces the \emph{table} OSRM service. 
#' @param loc a data frame containing 3 fields: points identifiers, latitudes and 
#' longitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers.
#' If loc parameter is used, all pair-wise distances are computed.
#' @param locId identifier field in loc (optional, see Details). 
#' @param locLat latitude field in loc (optional).
#' @param locLon longitude field in loc (optional).
#' @param src a data frame containing origin points identifiers, latitudes and 
#' longitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers. 
#' If dst and src parameters are used, only pairs between scr/dst are computed.
#' @param srcId identifier field in src (optional).
#' @param srcLat latitude field in src (optional).
#' @param srcLon longitude field in src (optional).
#' @param dst a data frame containing destination points identifiers, latitudes 
#' and longitudes (WGS84). It can also be a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame, then row names are used as identifiers. 
#' @param dstId identifier field in dst (optional).
#' @param dstLat latitude field in dst (optional).
#' @param dstLon longitude field in dst (optional).
#' @return A list containing 3 data frames is returned. 
#' distance_table is the matrix of travel times (in minutes), 
#' source_coordinates and destination_coordinates are the coordinates of 
#' the origin and destination points actually used to compute the travel 
#' times.
#' @details If loc, src or dst are data frames we assume that the 3 first 
#' columns of the data.frame are: identifier, latitude and longitude. 
#' If it is not the case, use fields parameters.
#' @note The public OSRM API does not allow more than 10 000 
#' distances in query result. \cr
#' If you use an other OSRM API service, make sure that 
#' more distances are allowed in results 
#' (i.e. the "max-table-size" argument, Max. locations supported in distance 
#' table query).
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Inputs are data frames  
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50, c("comm_id","lat","lon")])
#' # First 5 rows and columns
#' distCom$distance_table[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,c("comm_id","lat","lon")],
#'                       dst = com[11:20,c("comm_id","lat","lon")])
#' # First 5 rows and columns
#' distCom2$distance_table[1:5,1:5]
#' 
#' 
#' # Inputs are SpatialPointsDataFrames
#' distCom <- osrmTable(loc = src)
#' # First 5 rows and columns
#' distCom$distance_table[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = src, dst = dst)
#' # First 5 rows and columns
#' distCom2$distance_table[1:5,1:5]
#' }
#' @export
osrmTable <- function(loc, src = NULL, dst = NULL, 
                      locId = NULL, locLat = NULL, locLon = NULL, 
                      srcId = NULL, srcLat = NULL, srcLon = NULL, 
                      dstId = NULL, dstLat = NULL, dstLon = NULL){
  tryCatch({
    if (is.null(src)){
      # check if inpout is sp
      if(testSp(loc)){
        x <- spToDf(x = loc)
        loc <- x$loc
        locId <- x$id
        locLat <- x$lat
        locLon <- x$lon
      }else{
        n <- names(loc)
        if(is.null(locId))locId <- n[1]
        if(is.null(locLat))locLat <- n[2]
        if(is.null(locLon))locLon <- n[3]
      }
      nSrc <- nrow(loc)
      nDst <- nSrc
      osrmLimit(nSrc = nSrc, nDst = nDst)
      # Build the query
      req <- tableLoc(loc = loc, locLat = locLat, locLon = locLon)
      # Get the result
      resRaw <- RCurl::getURL(utils::URLencode(req), 
                              useragent = "'osrm' R package")
      # Parse the results
      res <- jsonlite::fromJSON(resRaw)
      # get the distance table
      distance_table <- distTableFormat(res = res, 
                                        src = loc, srcId = locId, 
                                        dst = loc, dstId = locId)
      # get the coordinates
      coords <- coordFormat(res = res, 
                            src = loc, srcId = locId, 
                            dst = loc, dstId = locId)
      return(list(distance_table = distance_table, 
                  source_coordinates = coords$source_coordinates, 
                  destination_coordinates = coords$destination_coordinates))
      
    } else {
      # check if input is sp
      if(testSp(src)){
        x <- spToDf(x = src)
        src <- x$loc
        srcId <- x$id
        srcLat <- x$lat
        srcLon <- x$lon
      }else{
        n <- names(src)
        if(is.null(srcId))srcId <- n[1]
        if(is.null(srcLat))srcLat <- n[2]
        if(is.null(srcLon))srcLon <- n[3]
      }
      # check if input is sp
      if(testSp(dst)){
        x <- spToDf(x = dst)
        dst <- x$loc
        dstId <- x$id
        dstLat <- x$lat
        dstLon <- x$lon
      }else{
        n <- names(dst)
        if(is.null(dstId))dstId <- n[1]
        if(is.null(dstLat))dstLat <- n[2]
        if(is.null(dstLon))dstLon <- n[3]
      }
      
      nSrc <- nrow(src)
      nDst <- nrow(dst)
      osrmLimit(nSrc = nSrc, nDst = nDst)
      # Build the query
      req <- tableSrcDst(src = src, srcLat = srcLat, srcLon = srcLon, 
                         dst = dst, dstLat = dstLat, dstLon = dstLon)
      # Get the result
      resRaw <- RCurl::getURL(utils::URLencode(req), 
                              useragent = "'osrm' R package")
      # Parse the results
      res <- jsonlite::fromJSON(resRaw)
      # get the distance table
      distance_table <- distTableFormat(res = res, 
                                        src = src, srcId = srcId, 
                                        dst = dst, dstId = dstId)
      # get the coordinates
      coords <- coordFormat(res = res, 
                            src = src, srcId = srcId, 
                            dst = dst, dstId = dstId)
      return(list(distance_table = distance_table, 
                  source_coordinates = coords$source_coordinates, 
                  destination_coordinates = coords$destination_coordinates))
    }
  }, error=function(e) { message("osrmTable function returns an error: \n", e)})
  return(NULL)
}

distTableFormat <- function(res, src, srcId, dst, dstId){
  # extract distance table
  mat <- res$distance_table
  # From millisec to minutes
  mat <- round(mat/(600), 1)
  # NA management
  mat[mat == 3579139.4] <- NA
  # col and row names management
  dimnames(mat) <- list(src[, srcId], dst[, dstId])
  return(mat)
}  

coordFormat <- function(res, src, srcId, dst, dstId){
  source_coordinates <- res$source_coordinates
  row.names(source_coordinates) <- src[,srcId]
  colnames(source_coordinates) <- c("Lat", "Lon")
  destination_coordinates <- res$destination_coordinates
  row.names(destination_coordinates) <- dst[,dstId]
  colnames(destination_coordinates) <- c("Lat", "Lon")
  return(list(source_coordinates = source_coordinates, 
              destination_coordinates = destination_coordinates)
  )
}


tableLoc <- function(loc, locLat, locLon){
  # Query build
  tab <- paste(getOption("osrm.server"), "table?loc=", sep = "")
  n <- nrow(loc)
  for (i in 1:n){
    tab <- paste(tab, loc[i, locLat], ",", loc[i, locLon], "&loc=", sep="")
  }
  tab <- substr(x = tab, start = 1, stop = (nchar(tab)-5))
  return(tab)
}

tableSrcDst <- function(src, srcLat, srcLon,
                        dst, dstLat, dstLon){
  # Query build
  tab <- paste(getOption("osrm.server"), "table?src=", sep = "")
  n <- nrow(src)
  for (i in 1:n){
    tab <- paste(tab, src[i,srcLat], ",", src[i,srcLon], "&src=", sep="")
  }
  tab <- substr(x = tab, start = 1, stop = (nchar(tab)-5))
  tab <- paste(tab, "&dst=", sep = "")
  m <- nrow(dst)
  for (i in 1:m){
    tab <- paste(tab, dst[i,dstLat], ",", dst[i,dstLon], "&dst=", sep="")
  }
  tab <- substr(x = tab, start = 1, stop = (nchar(tab)-5))
  return(tab)
}

osrmLimit <- function(nSrc, nDst){
  e <- simpleError("The public OSRM API does not allow results with 
  a number of distances higher than 10000")
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & (nSrc*nDst) > 10000){
    stop(e)
  }
}




