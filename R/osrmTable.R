#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices 
#' between points. This function interface the \emph{table} OSRM service. 
#' @param loc a data frame containing points identifiers, longitudes and 
#' latitudes (WGS84) or a SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' If loc parameter is used, all pair-wise distances are computed.
#' @param locId identifier field in loc. If loc is a SpatialPointsDataFrame 
#' or a SpatialPolygonsDataFrame this parameter is not used and row names 
#' of the Spatial*DataFrame are used as identifiers.
#' @param locLat latitude field in loc. Not used if loc is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @param locLon longitude field in loc. Not used if loc is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @param src data frame containing origin points identifiers, longitudes 
#' and latitudes (WGS84) or a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame. If dst and src parameters are used, only 
#' pairs between scr/dst are computed.
#' @param srcId identifier field in src. If src is a SpatialPointsDataFrame 
#' or a SpatialPolygonsDataFrame this parameter is not used and row names 
#' of the Spatial*DataFrame are used as identifiers.
#' @param srcLat latitude field in src. Not used if src is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @param srcLon longitude field in src. Not used if src is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @param dst data frame containing destination points identifiers, 
#' longitudes and latitudes (WGS84) or a SpatialPointsDataFrame or a 
#' SpatialPolygonsDataFrame.. If dst and src parameters are used, only 
#' pairs between scr/dst are computed.
#' @param dstId identifier field in dst. If dst is a SpatialPointsDataFrame 
#' or a SpatialPolygonsDataFrame this parameter is not used and row names 
#' of the Spatial*DataFrame are used as identifiers.
#' @param dstLat latitude field in dst. Not used if dst is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @param dstLon longitude field in dst. Not used if dst is a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame.
#' @return A list containing 3 data frames is returned. 
#' distance_table is the matrix of travel times (in minutes), 
#' source_coordinates and destination_coordinates are the coordinates of 
#' the origin and destination points actually used to compute the travel 
#' times.
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
#' distCom <- osrmTable(loc = com[1:50,],
#'                      locId = "comm_id",
#'                      locLat = "lat",
#'                      locLon = "lon")
#' # First 5 rows and columns
#' distCom$distance_table[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,],
#'                       srcId = "comm_id", srcLat = "lat", srcLon = "lon",
#'                       dst = com[11:20,],
#'                       dstId = "comm_id", dstLat = "lat", dstLon = "lon")
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
osrmTable <- function(loc, locId, locLat, locLon, 
                      src = NULL, srcId, srcLat, srcLon, 
                      dst = NULL, dstId, dstLat, dstLon){
  tryCatch({
    if (is.null(src)){
      # check if inpout is sp
      if(testSp(loc)){
        x <- spToDf(x = loc)
        loc <- x$loc
        locId <- x$id
        locLat <- x$lat
        locLon <- x$lon
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
      # check if inpout is sp
      if(testSp(src)){
        x <- spToDf(x = src)
        src <- x$loc
        srcId <- x$id
        srcLat <- x$lat
        srcLon <- x$lon
      }
      # check if inpout is sp
      if(testSp(dst)){
        x <- spToDf(x = dst)
        dst <- x$loc
        dstId <- x$id
        dstLat <- x$lat
        dstLon <- x$lon
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




