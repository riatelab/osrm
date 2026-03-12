#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices
#' between points. This function interfaces the \emph{table} OSRM service.\cr
#' Use \code{src} and \code{dst} to set different origins and destinations.\cr
#' Use \code{loc} to compute travel times or travel distances between all
#' points.
#' @param src origin points.
#' \code{src} can be: \itemize{
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If relevant, row names are used as identifiers.
#' @param dst destination.
#' \code{dst} can be: \itemize{
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If relevant, row names are used as identifiers.
#' @param loc points. \code{loc} can be: \itemize{
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If relevant, row names are used as identifiers.
#' @param measure a character indicating what measures are calculated. It can
#' be "duration" (in minutes), "distance" (meters), "total_distance"
#' (network distance + snapping distance, in meters) or any combination
#' of them.
#' @param exclude pass an optional "exclude" request option to the OSRM API
#' (not allowed with the OSRM demo server).
#' @param osrm.server the base URL of the routing server.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot".
#' @return
#' The output of this function is a list composed of one or several matrices
#' and 2 data.frames
#' \itemize{
#'   \item{durations}: a matrix of travel times (in minutes)
#'   \item{distances}: a matrix of network distances (in meters)
#'   \item{total_distances}: a matrix of network + snapping distances
#'   (in meters)
#'   \item{sources}: a data.frame of the coordinates of the points actually
#'   used as starting points (EPSG:4326 - WGS84),
#'   including their snapping distance in meters
#'
#'   \item{destinations}: a data.frame of the coordinates of the points actually
#'   used as destinations (EPSG:4326 - WGS84),
#'   including their snapping distance in meters
#'   }
#' @note
#' The OSRM demo server does not allow large queries (more than 10000 distances
#' or durations).\cr
#' If you use your own server and if you want to get a large number of distances
#' make sure to set the "max-table-size" option (Max. locations supported in
#' table) of the OSRM server accordingly.
#' @examples
#' \dontrun{
#' # Inputs are data frames
#' apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
#' # Travel time matrix
#' distA <- osrmTable(loc = apotheke.df[1:50, c("lon", "lat")])
#' # First 5 rows and columns
#' distA$durations[1:5, 1:5]
#'
#' # Travel time matrix with different sets of origins and destinations
#' distA2 <- osrmTable(
#'   src = apotheke.df[1:10, c("lon", "lat")],
#'   dst = apotheke.df[11:20, c("lon", "lat")]
#' )
#' # First 5 rows and columns
#' distA2$durations[1:5, 1:5]
#'
#' # Inputs are sf points
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#'   quiet = TRUE
#' )
#' distA3 <- osrmTable(loc = apotheke.sf[1:10, ])
#' # First 5 rows and columns
#' distA3$durations[1:5, 1:5]
#'
#' # Travel time matrix with different sets of origins and destinations
#' distA4 <- osrmTable(src = apotheke.sf[1:10, ], dst = apotheke.sf[11:20, ])
#' # First 5 rows and columns
#' distA4$durations[1:5, 1:5]
#'
#' # Road distance matrix with different sets of origins and destinations
#' distA5 <- osrmTable(
#'   src = apotheke.sf[1:10, ], dst = apotheke.sf[11:20, ],
#'   measure = "distance"
#' )
#' # First 5 rows and columns
#' distA5$distances[1:5, 1:5]
#' }
#' @export
osrmTable <- function(
  src,
  dst = src,
  loc,
  exclude,
  measure = "duration",
  osrm.server = getOption("osrm.server"),
  osrm.profile = getOption("osrm.profile")
) {
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)

  url <- base_url(osrm.server, osrm.profile, "table")

  # input management
  if (!missing(loc)) {
    loc <- input_table(x = loc, id = "loc")
    dst_r <- src_r <- loc
    url <- paste0(url, encode_coords(x = loc, osrm.server = osrm.server), "?")
  } else {
    src_r <- input_table(x = src, id = "src")
    dst_r <- input_table(x = dst, id = "dst")
    loc <- rbind(src_r, dst_r)
    url <- paste0(
      url,
      encode_coords(x = loc, osrm.server),
      paste0(
        "?sources=",
        paste(0:(nrow(src_r) - 1), collapse = ";"),
        "&destinations=",
        paste(nrow(src_r):(nrow(loc) - 1), collapse = ";")
      ),
      "&"
    )
  }

  # adding exclude parameter
  if (!missing(exclude)) {
    url <- paste0(url, "exclude=", exclude, "&")
  }

  # Manage "total_distance" measure
  total <- "total_distance" %in% measure
  measure_api <- setdiff(measure, "total_distance")
  if (total && !("distance" %in% measure_api)) {
    measure_api <- c(measure_api, "distance")
  }

  # adding measure parameter
  url <- paste0(
    url,
    "annotations=",
    paste0(measure_api, collapse = ","),
    "&generate_hints=false"
  )
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

  # format results
  output <- list()
  if (!is.null(res$durations)) {
    output$durations <- tab_format(
      res = res,
      src = src_r,
      dst = dst_r,
      type = "duration"
    )
  }
  if (!is.null(res$distances)) {
    output$distances <- tab_format(
      res = res,
      src = src_r,
      dst = dst_r,
      type = "distance"
    )
  }
  # get the coordinates
  coords <- coord_format(res = res, src = src_r, dst = dst_r)
  output$sources <- coords$sources
  output$destinations <- coords$destinations

  # compute total distances
  if (total && !is.null(output$distances)) {
    src_snap <- output$sources$snapping_distance
    dst_snap <- output$destinations$snapping_distance
    snap_sum <- outer(src_snap, dst_snap, "+")
    output$total_distances <- output$distances + round(snap_sum, 0)

    # fix self-distance
    ids_match <- outer(
      rownames(output$total_distances),
      colnames(output$total_distances),
      "=="
    )
    output$total_distances[ids_match] <- 0

    if (!("distance" %in% measure)) {
      output$distances <- NULL
    }
  }

  return(output)
}
