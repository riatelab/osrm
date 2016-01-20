#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices between points. 
#' This function interface the \emph{table} OSRM service. 
#' @param loc data frame containing points identifiers, longitudes and latitudes 
#' (WGS84). If loc parameter is used, all pair-wise distances are computed.
#' @param locId identifier field in loc.
#' @param locLat latitude field in loc.
#' @param locLon longitude field in loc.
#' @param src data frame containing origin points identifiers, longitudes and latitudes 
#' (WGS84). If dst and src parameters are used, only pairs between scr/dst are computed.
#' @param srcId identifier field in src
#' @param srcLat latitude fiels in src
#' @param srcLon longitude field in src
#' @param dst data frame containing destination points identifiers, longitudes and latitudes 
#' (WGS84). If dst and src parameters are used, only pairs between scr/dst are computed.
#' @param dstId identifier field in dst
#' @param dstLat latitude field in dst
#' @param dstLon longitude field in dst
#' @return A list containing 3 data frames is returned. 
#' distance_table is the matrix of travel times (in minutes), 
#' source_coordinates and destination_coordinates are the coordinates of 
#' the origin and destination points actually used to compute the travel times.
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
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50,], 
#'                      locId = "comm_id", 
#'                      locLat = "lat", 
#'                      locLon = "lon")
#' # First 5 rows and columns
#' distCom$distance_table[1:5,1:5]
#' distCom2 <- osrmTable(src = com[1:50,], 
#'                       srcId = "comm_id", srcLat = "lat", srcLon = "lon", 
#'                       dst = com[51:100,], 
#'                       dstId = "comm_id", dstLat = "lat", dstLon = "lon")
#' # First 5 rows and columns
#' distCom2$distance_table[1:5,1:5]
#' }
#' @export
osrmTable <- function(loc, locId, locLat, locLon, 
                      src = NULL, srcId, srcLat, srcLon, 
                      dst = NULL, dstId, dstLat, dstLon){
  tryCatch({
    if (is.null(src)){
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
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & 
     (nSrc*nDst) > 10000){
    stop(e)
  }
}


# src = com
# srcId = "comm_id"
# srcLat = "lat"
# srcLon = "lon"
# dst = com[1,]
# dstId = "comm_id"
# dstLat = "lat"
# dstLon = "lon"
# 
# nSrc <- nrow(src)
# nDst <- nrow(dst)
# dimMat <- nSrc * nDst
# d_t <- matrix(data = 1:dimMat, nrow = nSrc, ncol = nDst, 
#               dimnames = list(src[,srcId], dst[, dstId]))
# source_coordinates <- data.frame(row.names = src[,srcId], 
#                                  Lat = rep(NA, nSrc), 
#                                  Lon = rep(NA, nSrc))
# destination_coordinates <- data.frame(row.names = dst[,dstId], 
#                                       Lat = rep(NA, nDst), 
#                                       Lon = rep(NA, nDst))
# 
# 
# # nb de colonnes Full pour une requetes
# nFullCol1Req <- 10000 %/% nSrc
# 
# # nb de requetes full necessaire
# nReqFullCol <- nDst %/% nFullCol1Req
# 
# # nb col restantes
# nColRest <- nDst - nReqFullCol * nFullCol1Req
# 
# init <- 1
# for (i in 1:nReqFullCol){
#   req <- tableSrcDst(src = src, srcLat = srcLat, srcLon = srcLon, 
#                      dst = dst[init:(init+nFullCol1Req-1),], 
#                      dstLat = dstLat, dstLon = dstLon)
#   resRaw <- RCurl::getURL(utils::URLencode(req), 
#                           useragent = "'osrm' R package")
#   res <- jsonlite::fromJSON(resRaw)
#   print(i)
#   distance_table <- distTableFormat(res = res, 
#                                     src = src, srcId = srcId, 
#                                     dst = dst[init:(init+nFullCol1Req-1),], 
#                                     dstId = dstId)
#   d_t[,init:(init+nFullCol1Req-1)] <- distance_table
#   init <- init+nFullCol1Req
# }
# 
# # Pour les autres colonnes
# req <- tableSrcDst(src = src, srcLat = srcLat, srcLon = srcLon, 
#                    dst = dst[init:nDst,], 
#                    dstLat = dstLat, dstLon = dstLon)
# resRaw <- RCurl::getURL(utils::URLencode(req), 
#                         useragent = "'osrm' R package")
# res <- jsonlite::fromJSON(resRaw)
# 
# distance_table <- distTableFormat(res = res, 
#                                   src = src, srcId = srcId, 
#                                   dst = dst[init:nDst,], 
#                                   dstId = dstId)
# d_t[,init:nDst] <- distance_table

