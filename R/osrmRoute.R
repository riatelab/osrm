#' @name osrmRoute
#' @title Get the Shortest Path Between Two Points
#' @description Build and send an OSRM API query to get the travel geometry
#' between two points. This function interfaces with the \emph{route} OSRM
#' service.\cr
#' Use \code{src} and \code{dst} to get the shortest direct route between
#' two points.\cr
#' Use \code{loc} to get the shortest route between two points using
#' ordered waypoints.
#'
#'
#' @param src starting point of the route.
#' \code{src} can be: \itemize{
#'   \item a vector of coordinates (longitude and latitude, WGS 84),
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If relevant, row names are used as identifiers.\cr
#' If \code{src} is a data.frame, a matrix, an sfc object or an sf object then
#' only the first row or element is considered.
#' @param dst destination of the route.
#' \code{dst} can be: \itemize{
#'   \item a vector of coordinates (longitude and latitude, WGS 84),
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If relevant, row names are used as identifiers.\cr
#' If \code{dst} is a data.frame, a matrix, an sfc object or an sf object then
#' only the first row or element is considered.
#'
#' @param loc starting point, waypoints (optional) and destination of the
#' route. \code{loc} can be: \itemize{
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' The first row or element is the starting point then waypoints are used in
#' the order they are stored in \code{loc} and the last row or element is
#' the destination.\cr
#' If relevant, row names are used as identifiers.\cr
#' @param overview "full", "simplified" or FALSE. Use "full" to return the
#' detailed geometry, use "simplified" to return a simplified geometry, use
#' FALSE to return only time and distance.
#' @param exclude pass an optional "exclude" request option to the OSRM API.
#' @param osrm.server the base URL of the routing server.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot".
#' @param snapping_distance if TRUE, a list is returned: the first element
#' is the route, the second is a table of snapping distances.
#' @return
#' If \code{snapping_distance = FALSE} (default): \itemize{
#'   \item If \code{overview} is not FALSE, an sf LINESTRING of the shortest
#'   route is returned. It contains 4 fields: src, dst, duration (in minutes)
#'   and distance (in kilometers).
#'   \item If \code{overview = FALSE}, a named numeric vector is returned
#'   (duration and distance).
#'   }
#' If \code{snapping_distance = TRUE}, a list is returned: \itemize{
#'   \item \code{route}: the route as described above.
#'   \item \code{snapping}: an sf POINT of the snapped waypoints. It contains
#'   the \code{id} and the \code{snapping_distance} (in kilometers).
#'   }
#'
#' If src (or loc) is a vector, a data.frame or a matrix, the coordinate
#' reference system (CRS) of the route is EPSG:4326 (WGS84).\cr
#' If src (or loc) is an sfc or sf object, the route has the same CRS
#' as src (or loc).
#' @importFrom sf st_as_sfc st_crs st_geometry st_sf st_as_sf st_transform
#' @examples
#' \dontrun{
#' library(sf)
#' apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#'   quiet = TRUE
#' )
#' # Travel path between points
#' route1 <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.sf[16, ])
#' # Display paths
#' plot(st_geometry(route1))
#' plot(st_geometry(apotheke.sf[c(1, 16), ]), col = "red", pch = 20, add = TRUE)
#'
#' # Return the route and snapping distances
#' route2 <- osrmRoute(
#'   src = apotheke.sf[1, ], dst = apotheke.sf[16, ],
#'   snapping_distance = TRUE
#' )
#' route2$route
#' route2$snapping
#'
#' # Return only duration and distance
#' route3 <- osrmRoute(
#'   src = apotheke.df[1, c("lon", "lat")],
#'   dst = apotheke.df[16, c("lon", "lat")],
#'   overview = FALSE
#' )
#' route3
#'
#' # Using only coordinates
#' route4 <- osrmRoute(
#'   src = c(13.412, 52.502),
#'   dst = c(13.454, 52.592)
#' )
#' plot(st_geometry(route4))
#'
#' # Using via points
#' route5 <- osrmRoute(loc = apotheke.sf[c(1, 2, 4, 3), ])
#' plot(st_geometry(route5), col = "red", lwd = 2)
#' plot(st_geometry(apotheke.sf[c(1, 2, 4, 3), ]), add = TRUE)
#'
#' # Using a different routing server
#' u <- "https://routing.openstreetmap.de/routed-foot/"
#' route5 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ], osrm.server = u)
#' route5
#'
#' # Using an open routing service with support for multiple modes
#' # see https://github.com/riatelab/osrm/issues/67
#' u <- "https://routing.openstreetmap.de/"
#' options(osrm.server = u)
#' route6 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ],
#'   osrm.profile = "bike"
#' )
#' route7 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ],
#'   osrm.profile = "car"
#' )
#' plot(st_geometry(route7), col = "green") # car
#' plot(st_geometry(route6), add = TRUE) # bike
#' plot(st_geometry(route5), col = "red", add = TRUE) # foot
#' }
#' @export
osrmRoute <- function(src,
                      dst,
                      loc,
                      overview = "simplified",
                      exclude,
                      osrm.server = getOption("osrm.server"),
                      osrm.profile = getOption("osrm.profile"),
                      snapping_distance = FALSE) {
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)

  url <- base_url(osrm.server, osrm.profile, "route")

  if (missing(loc)) {
    # From src to dst
    src <- input_route(x = src, id = "src", single = TRUE)
    dst <- input_route(x = dst, id = "dst", single = TRUE)
    id1 <- src$id
    id2 <- dst$id
    ids <- c(id1, id2)
    oprj <- src$oprj
    coords <- paste0(src$lon, ",", src$lat, ";", dst$lon, ",", dst$lat)
  } else {
    # from src to dst via x, y, z... (data.frame or sf input)
    loc <- input_route(x = loc, single = FALSE, all.ids = TRUE)
    oprj <- loc$oprj
    id1 <- loc$id[1]
    id2 <- loc$id[length(loc$id)]
    ids <- loc$id
    coords <- paste0(
      apply(cbind(loc$lon, loc$lat), MARGIN = 1, FUN = paste0, collapse = ","),
      collapse = ";"
    )
  }

  url <- paste0(
    url,
    coords,
    "?alternatives=false&geometries=polyline&steps=false&overview=",
    tolower(overview),
    "&generate_hints=false"
  )

  # adding exclude parameter
  if (!missing(exclude)) {
    url <- paste0(url, "&exclude=", exclude)
  }

  e <- try(
    {
      req_handle <- curl::new_handle(verbose = FALSE)
      curl::handle_setopt(req_handle, useragent = "osrm_R_package")
      r <- curl::curl_fetch_memory(utils::URLencode(url), handle = req_handle)
    },
    silent = TRUE
  )
  if (inherits(e, "try-error")) {
    stop(e, call. = FALSE)
  }

  # test result validity
  test_http_error(r)
  res <- RcppSimdJson::fparse(rawToChar(r$content))

  if (overview == FALSE) {
    res_out <- round(c(
      duration = res$routes$duration / 60,
      distance = res$routes$distance / 1000
    ), 2)
  } else {
    # Coordinates of the line
    geodf <- googlePolylines::decode(res$routes$geometry)[[1]][, c(2, 1)]
    # Convert to LINESTRING
    rcoords <- paste0(geodf$lon, " ", geodf$lat, collapse = ", ")
    res_out <- st_sf(
      src = id1, dst = id2,
      duration = res$routes$duration / 60,
      distance = res$routes$distance / 1000,
      geometry = st_as_sfc(paste0("LINESTRING(", rcoords, ")")),
      crs = 4326,
      row.names = paste(id1, id2, sep = "_")
    )

    # prj
    if (!is.na(oprj)) {
      res_out <- st_transform(res_out, oprj)
    }
  }

  if (snapping_distance) {
    snapping <- res$waypoints
    snapping$snapping_distance <- snapping$distance / 1000
    snapping$id <- ids
    snapping <- snapping[, c("id", "snapping_distance")]
    snapping$lon <- sapply(res$waypoints$location, "[[", 1)
    snapping$lat <- sapply(res$waypoints$location, "[[", 2)
    snapping_sf <- st_as_sf(snapping, coords = c("lon", "lat"), crs = 4326)
    if (!is.na(oprj)) {
      snapping_sf <- st_transform(snapping_sf, oprj)
    }
    return(list(route = res_out, snapping = snapping_sf))
  }

  return(res_out)
}

