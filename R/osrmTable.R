#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices 
#' between points. This function interfaces the \emph{table} OSRM service. 
#' @param loc a data frame containing 3 fields: points identifiers, longitudes 
#' and latitudes (WGS84). It can also be an sf object. If so, row names are used 
#' as identifiers.
#' If loc parameter is used, all pair-wise distances are computed.
#' @param src a data frame containing origin points identifiers, longitudes 
#' and latitudes (WGS84). It can also be an sf object. If so, row names are used
#' as identifiers. 
#' If dst and src parameters are used, only pairs between scr/dst are computed.
#' @param dst a data frame containing destination points identifiers, longitudes 
#' and latitudes (WGS84). It can also be an sf object. If so, row names are used 
#' as identifiers. 
#' @param measure a character indicating what measures are calculated. It can 
#' be "duration" (in minutes), "distance" (meters), or both c('duration',
#' 'distance').  
#' @param exclude pass an optional "exclude" request option to the OSRM API
#' (not allowed with the OSRM demo server).
#' @param osrm.server the base URL of the routing server.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot".
#' @return A list containing 3 data frames is returned. 
#' durations is the matrix of travel times (in minutes), 
#' sources and destinations are the coordinates of 
#' the origin and destination points actually used to compute the travel 
#' times (WGS84).
#' @details If loc, src or dst are data frames we assume that the 3 first 
#' columns of the data.frame are: identifiers, longitudes and latitudes.\cr
#' The OSRM demo server does not allow large queries (more than 10000 distances 
#' or durations). 
#' @note 
#' If you want to get a large number of distances make sure to set the 
#' "max-table-size" option (Max. locations supported in table) of the OSRM 
#' server accordingly.
#' @examples
#' \dontrun{
#' # Inputs are data frames
#' apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
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
#' # Inputs are sf points
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"), 
#'                        quiet = TRUE)
#' distA3 <- osrmTable(loc = apotheke.sf[1:10,])
#' # First 5 rows and columns
#' distA3$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distA4 <- osrmTable(src = apotheke.sf[1:10,], dst = apotheke.sf[11:20,])
#' # First 5 rows and columns
#' distA4$durations[1:5,1:5]
#' }
#' @export
osrmTable <- function(loc, src, dst, exclude, measure = "duration", 
                      osrm.server = getOption("osrm.server"),
                      osrm.profile = getOption("osrm.profile")){
  

  
  opt <- options(error = NULL)
  on.exit(options(opt), add=TRUE)
  
  url <- base_url(osrm.server, osrm.profile, "table")
  
  # input management
  if(!missing(loc)){
    if (methods::is(loc,"sf")){
      loc <- sf_2_df(loc)
    }
    names(loc) <- c("id", "lon", "lat")
    dst <- src <- loc
    url <- paste0(url, encode_coords(x = loc), "?")
    
  }else{
    if (methods::is(src,"sf")){
      src <- sf_2_df(src)
    }
    if (methods::is(dst,"sf")){
      dst <- sf_2_df(dst)
    }
    names(src) <- c("id", "lon", "lat")
    names(dst) <- c("id", "lon", "lat")
    loc <- rbind(src, dst)
    url <- paste0(url, 
                  encode_coords(x = loc), 
                  paste0("?sources=", 
                         paste(0:(nrow(src)-1), collapse = ";"), 
                         "&destinations=", 
                         paste(nrow(src):(nrow(loc)-1), collapse = ";")),
                  "&")
  }
  
  # adding exclude parameter
  if (!missing(exclude)) {
    url <- paste0(url, "exclude=", exclude, "&")
  }
  # adding measure parameter
  url <- paste0(url, "annotations=", paste0(measure, collapse=',') )
  # print(url)
  e <- try({
    req_handle <- curl::new_handle(verbose = FALSE)
    curl::handle_setopt(req_handle, useragent = "osrm_R_package")
    r <- curl::curl_fetch_memory(utils::URLencode(url), handle = req_handle)
  }, silent = TRUE)
  if (inherits(e,"try-error")){
    stop(e, call. = FALSE)
  }
  
  # test result validity
  test_http_error(r)

  res <- RcppSimdJson::fparse(rawToChar(r$content))

  # format results
  output <- list()
  if(!is.null(res$durations)){
    output$durations <- tab_format(res = res, src = src, dst = dst, 
                                   type = "duration")
  }
  if(!is.null(res$distances)){
    output$distances <- tab_format(res = res, src = src, dst = dst, 
                                   type = "distance")
  }
  # get the coordinates
  coords <- coord_format(res = res, src = src, dst = dst)
  output$sources <- coords$sources
  output$destinations = coords$destinations
  return(output)
  
  return(r)
}
