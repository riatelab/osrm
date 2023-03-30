#' @name osrmNearest
#' @title Get the Nearest Point on the Street Network 
#' @description This function interfaces with the \emph{nearest} OSRM 
#' service.\cr
#' @param loc a point to snap to the street network. \code{loc} can be: \itemize{
#'   \item a vector of coordinates (longitude and latitude, WGS 84), 
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#'}
#' If \code{src} is a data.frame, a matrix, an sfc object or an sf object then 
#' only the first row or element is considered.
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param osrm.server the base URL of the routing server.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot".
#' @return
#' The output of this function is an sf POINT of the point on the street 
#' network.\cr
#' It contains 2 fields: \itemize{
#'   \item id, the point identifierv
#'   \item distance, the distance in meters to the supplied input point.
#'   }
#' @importFrom sf st_as_sfc st_crs st_geometry st_sf st_as_sf st_transform
#' @examples
#' \dontrun{
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#'                        quiet = TRUE)
#' pt <- osrmNearest(apotheke.sf[56, ])
#' pt$distance
#' }
#' @export
osrmNearest <- function( 
    loc, 
    exclude,
    osrm.server = getOption("osrm.server"),
    osrm.profile = getOption("osrm.profile")){
  
  opt <- options(error = NULL)
  on.exit(options(opt), add=TRUE)
  
  url <- base_url(osrm.server, osrm.profile, "nearest")
  
  # from src to dst via x, y, z... (data.frame or sf input)
  loc <- input_route(x = loc, single = TRUE, id = "loc")
  id <- loc$id
  oprj <- loc$oprj
  coords <- paste0(loc$lon, ",", loc$lat)
  
  url <- paste0(url, coords, "?number=1&generate_hints=false")
  
  # adding exclude parameter
  if (!missing(exclude)) {url <- paste0(url, "&exclude=", exclude)}
  
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

  # Coordinates of the point
  r <-   res$waypoints$location[[1]]
  # Convert to POINT
  rcoords <- paste0(r, collapse = " ")
  rosf <- st_sf(id = id,
                distance = round(res$waypoints$distance, 1),
                geometry = st_as_sfc(paste0("POINT(",rcoords,")")), 
                crs = 4326, 
                row.names = id)
  # prj
  if (!is.na(oprj)){
    rosf <- st_transform(rosf, oprj)
  }
  
  return(rosf)
  
}
