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
#' 'distance'). The demo server only allows "duration". 
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
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
#' @importFrom sf st_as_sf
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
  
  
  
  
  # construct the base url
  base_url <- function(osrm.server, osrm.profile, query){
    if(osrm.server == "https://routing.openstreetmap.de/") {
      url <- paste0(osrm.server, "routed-", osrm.profile, "/table/v1/driving/")
    }else{
      url <- paste0(osrm.server, query, "/v1/", osrm.profile, "/")
    }
    return(url)
  }
  
  # create short and clean coordinates
  clean_coord <- function(x){
    format(round(as.numeric(x),5), scientific = FALSE, justify = "none", 
           trim = TRUE, nsmall = 5, digits = 5)
  }
  
  
  # this function takes an sf and transforms it into a dataframe  
  sf_2_df <- function(x){    
    # transform to centroid and to wgs84
    if (methods::is(sf::st_geometry(x), "sfc_GEOMETRY") ||  
        methods::is(sf::st_geometry(x), "sfc_GEOMETRYCOLLECTION")){
      x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
    }
    if (methods::is(sf::st_geometry(x), "sfc_POLYGON") || 
        methods::is(sf::st_geometry(x), "sfc_MULTIPOLYGON")){
      sf::st_geometry(x) <- sf::st_centroid(x = sf::st_geometry(x),
                                            of_largest_polygon = TRUE)
    }
    x <- sf::st_transform(x = x, crs = 4326)
    coords <- sf::st_coordinates(x)
    x <- data.frame(id = row.names(x), 
                    lon = clean_coord(coords[,1]), 
                    lat = clean_coord(coords[,2]))
    return(x)
  }
  
  
  encode_coords <- function(x){
    x$lat <- as.numeric(as.character(x$lat))
    x$lon <- as.numeric(as.character(x$lon))
    res <- paste0("polyline(", googlePolylines::encode(x[,c("lon","lat")]), ")")
    return(res)
  }
  test_http_error <- function(r){
    if (httr::http_error(r)) {
      if (httr::http_type(r) != "application/json") {
        stop(
          sprintf(
            "OSRM API request failed [%s]", 
            httr::status_code(r)),
          call. = FALSE)
      }else{
        rep <- RcppSimdJson::fparse(rawToChar(r$content))
        stop(
          sprintf(
            "OSRM API request failed [%s]\n%s\n%s", 
            httr::status_code(r), 
            rep$code, 
            rep$message
          ),
          call. = FALSE
        )
      }
    }
  }
  
  
  args <- list()
  url <- base_url(osrm.server, osrm.profile, "table")
  
  # input management
  if(!missing(loc)){
    if (methods::is(loc,"sf")){
      loc <- sf_2_df(loc)
    }
    names(loc) <- c("id", "lon", "lat")
    dst <- src <- loc
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
    args['sources'] <- paste0(paste(0:(nrow(src)-1), collapse = ";"), 
                             "&destinations=", 
                             paste(nrow(src):(nrow(loc)-1), collapse = ";"))
  }
  
  # coord encoding
  coords_en <- encode_coords(x = loc)
  
  # adding parameters
  args["annotations"] <- paste0(measure, collapse=',')
  if (!missing(exclude)) {
    args["exclude"] = exclude
  }
  
  
  # Send / test / receive / inspect query & results
  e <- try({
    r <- httr::GET(url = utils::URLencode(paste0(url, coords_en)), 
                   query = args,
                   config = httr::config(useragent = "osrm R package",
                                         upload_buffersize = 512))
  }, silent = TRUE)
  
  if (class(e) =="try-error"){
    stop(e, call. = FALSE)
  }
  # print( args['source'] )
  test_http_error(r)

  res <- RcppSimdJson::fparse(rawToChar(r$content))
  
  
  
  
  output <- list()
  if(!is.null(res$durations)){
    # get the duration table
    output$durations <- durTableFormat(res = res, src = src, dst = dst)
  }
  if(!is.null(res$distances)){
    # get the distance table
    output$distances <- distTableFormat(res = res, src = src, dst = dst)
  }
  # get the coordinates
  coords <- coordFormat(res = res, src = src, dst = dst)
  output$sources <- coords$sources
  output$destinations = coords$destinations
  return(output)
  
  return(r)
}


# 
#   
#   httr::get(r)
#   rawToChar(r$content)
#   ?simpleError
#   
#   
#   res <- RcppSimdJson::fparse(rawToChar(r$content)) 
#   
#   
#   
#   
#   

#       if(testSf(src)){
#         src <- sfToDf(x = src)
#       }
#       if(testSf(dst)){
#         dst <- sfToDf(x = dst)
#       }
#       
#       names(src) <- c("id", "lon", "lat")
#       names(dst) <- c("id", "lon", "lat")
#       
#       
#       # Build the query
#       loc <- rbind(src, dst)
#       sep = "&"
#       req <- paste(tableLoc(loc = loc, osrm.server = osrm.server, 
#                             osrm.profile = osrm.profile),
#                    "?sources=", 
#                    paste(0:(nrow(src)-1), collapse = ";"), 
#                    "&destinations=", 
#                    paste(nrow(src):(nrow(loc)-1), collapse = ";"),
#                    sep="")
#     }
#     
#     # exclude mngmnt
#     if (!is.null(exclude)) {
#       exclude_str <- paste0(sep,"exclude=", exclude, sep = "") 
#       sep="&"
#     }else{
#       exclude_str <- ""
#     }
#     
#     # annotation mngmnt
#     annotations <- paste0(sep, "annotations=", paste0(measure, collapse=','))
#     
#     # final req
#     req <- paste0(req, exclude_str, annotations)
#     
#     # print(req)
#     req <- utils::URLencode(req)
#     osrmLimit(nSrc = nrow(src), nDst = nrow(dst), nreq = nchar(req))
#     
#     # Get the result
#     bo=0
#     while(bo!=10){
#       x = try({
#         req_handle <- curl::new_handle(verbose = FALSE)
#         curl::handle_setopt(req_handle, useragent = "osrm_R_package")
#         resraw <- curl::curl_fetch_memory(utils::URLencode(req), handle = req_handle)
#         resjson <- jsonlite::prettify(rawToChar(resraw$content))
#         res <- jsonlite::fromJSON(resjson)
#         
#       }, silent = TRUE)
#       if (class(x)=="try-error") {
#         Sys.sleep(1)
#         bo <- bo+1
#       } else
#         break 
#     }
#     
#     # Check results
#     if(is.null(res$code)){
#       e <- simpleError(res$message)
#       stop(e)
#     }else{
#       e <- simpleError(paste0(res$code,"\n",res$message))
#       if(res$code != "Ok"){stop(e)}
#     }
#     
#     output <- list()
#     if(!is.null(res$durations)){
#       # get the duration table
#       output$durations <- durTableFormat(res = res, src = src, dst = dst)
#     }
#     if(!is.null(res$distances)){
#       # get the distance table
#       output$distances <- distTableFormat(res = res, src = src, dst = dst)  
#     }
#     # get the coordinates
#     coords <- coordFormat(res = res, src = src, dst = dst)
#     output$sources <- coords$sources
#     output$destinations = coords$destinations
#     return(output)
#   }, error=function(e) {message("The OSRM server returned an error:\n", e)})
#   return(NULL)
# }
# 
