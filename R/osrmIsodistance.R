#' @name osrmIsodistance
#' @title Get Polygons of Isodistances
#' @description This function computes areas that are reachable within a
#' given road distance from a point and returns the reachable regions as
#' polygons. These areas of equal travel distance are called isodistances.
#' @param loc origin point. \code{loc} can be: \itemize{
#'   \item a vector of coordinates (longitude and latitude, WGS 84),
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' If \code{loc} is a data.frame, a matrix, an sfc object or an sf object then
#' only the first row or element is considered.
#' @param breaks a numeric vector of break values to define isodistance areas,
#' in meters.
#' @param exclude pass an optional "exclude" request option to the OSRM API.
#' @param n number of points used to compute isodistances, possible values are
#' c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000).
#' @param res deprecated
#' @param smooth if TRUE a moving window with a gaussian blur is applied to
#' distances. This option may be usefull to remove small patches of hard to
#' reach areas. The computed isodistances are less precise but better looking.
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @return
#' The output of this function is an sf MULTIPOLYGON of isodistances.\cr
#' It contains 3 fields: \itemize{
#'   \item id, an identifier
#'   \item isomin, the minimum value of the isodistance polygon in meters
#'   \item isomax, the maximum value of the isodistance polygon in meters
#' }
#' If loc is a vector, a data.frame or a matrix the coordinate
#' reference system (CRS) of the output is EPSG:4326 (WGS84).\cr
#' If loc is an sfc or sf object, the output has the same CRS
#' as loc.\cr
#' @importFrom sf st_as_sf st_crs st_transform st_convex_hull st_union st_intersects
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#'   quiet = TRUE
#' )
#' # Get isochones with lon/lat coordinates
#' iso <- osrmIsodistance(loc = c(13.43, 52.47), breaks = seq(0, 500, 100))
#' # Map
#' plot(iso["isomax"], breaks = sort(unique(c(iso$isomin, iso$isomax))))
#'
#' # Get isochones with an sf POINT
#' iso2 <- osrmIsodistance(loc = apotheke.sf[11, ], breaks = seq(0, 500, 100))
#' # Map
#' if (require("mapsf")) {
#'   mapsf::mf_map(
#'     x = iso2, var = "isomin", type = "choro",
#'     breaks = sort(unique(c(iso2$isomin, iso2$isomax))),
#'     pal = "Burg", border = NA, leg_pos = "topleft",
#'     leg_val_rnd = 0,
#'     leg_frame = TRUE, leg_title = "Isodistance\n(m)"
#'   )
#' }
#' }
osrmIsodistance <- function(loc, breaks = seq(from = 0, to = 10000, length.out = 4),
                            exclude, n = 500, smooth = FALSE, res,
                            osrm.server = getOption("osrm.server"),
                            osrm.profile = getOption("osrm.profile")) {
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)

  # input management
  loc <- input_route(x = loc, id = "loc", single = TRUE)
  oprj <- loc$oprj
  loc <- st_as_sf(data.frame(lon = loc$lon, lat = loc$lat),
    coords = c("lon", "lat"), crs = 4326
  )
  loc <- st_transform(loc, "epsg:3857")

  # max distance management to see how far to extend the grid to get measures
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  dmax <- tmax * 1.2

  # gentle sleeptime & param for demo server
  if (osrm.server != "https://routing.openstreetmap.de/") {
    sleeptime <- 0
    deco <- 999
  } else {
    sleeptime <- 1
    deco <- 75
  }

  # get the resolution
  res <- get_resolution(res = res, n = n)
  # create a grid to obtain measures
  ogrid <- rgrid(loc = loc, dmax = dmax, res = res)
  sgrid <- ogrid[sf::st_is_within_distance(ogrid, loc, dmax, sparse = FALSE), ]

  # slice the grid to make several API calls
  lsgr <- nrow(sgrid)
  niter <- lsgr %/% deco
  nitersup <- lsgr %% deco
  ltot <- niter + ifelse(nitersup > 0, 1, 0)
  listDur <- listDest <- vector(mode = "list", length = ltot)
  # get measures and destinations points
  if (niter > 0) {
    for (i in 1:niter) {
      dmat <- osrmTable(
        src = loc,
        dst = sgrid[(((i - 1) * deco) + 1):(i * deco), ],
        exclude = exclude,
        measure = "distance",
        osrm.server = osrm.server,
        osrm.profile = osrm.profile
      )
      listDur[[i]] <- dmat$distances
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
  }
  if (nitersup > 0) {
    dmat <- osrmTable(
      src = loc,
      dst = sgrid[((niter * deco) + 1):lsgr, ],
      exclude = exclude,
      measure = "distance",
      osrm.server = osrm.server,
      osrm.profile = osrm.profile
    )
    listDur[[ltot]] <- dmat$distances
    listDest[[ltot]] <- dmat$destinations
  }

  measure <- do.call(c, listDur)
  destinations <- do.call(rbind, listDest)
  # for testing purpose
  # return(list(destinations = destinations, measure = measure,
  #             sgrid = sgrid, res = res, tmax = tmax))


  # assign values to the grid
  g <- fill_grid(
    destinations = destinations, measure = measure,
    sgrid = ogrid, res = res, tmax = tmax
  )

  if (min(g$measure, na.rm = TRUE) > tmax) {
    warning(
      paste0(
        "An empty object is returned. ",
        "'loc' is too far from the OSRM network."
      ),
      call. = FALSE
    )
    empty_res <- st_sf(
      crs = ifelse(is.na(oprj), 4326, oprj),
      id = integer(),
      isomin = numeric(),
      isomax = numeric(),
      geometry = st_sfc()
    )
    return(empty_res)
  }

  # All values not within breaks are set to tmax+1
  g[is.na(g$measure), "measure"] <- tmax + .1
  g[is.nan(g$measure), "measure"] <- tmax + .1
  g[is.infinite(g$measure), "measure"] <- tmax + .1

  if (isTRUE(smooth)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(paste0(
        "'terra' package is needed for this function to work.",
        "Please install it."
      ), call. = FALSE)
    }
    r <- terra::rast(g[, c("COORDX", "COORDY", "measure"), drop = TRUE],
      crs = "epsg:3857"
    )
    k <- terra::res(r)[1] / 2
    rr <- terra::disagg(x = r, fact = 4, method = "near")
    mat <- terra::focalMat(x = rr, d = k, type = "Gauss")
    g <- terra::focal(x = rr, w = mat, fun = mean, na.rm = TRUE)
  }

  # computes isopolygones
  iso <- mapiso(x = g, breaks = breaks, var = "measure")
  # get rid of out of max breaks polys
  iso <- iso[-nrow(iso), ]
  # fisrt line always start at 0
  iso[1, "isomin"] <- 0

  # proj mgmnt
  if (!is.na(oprj)) {
    iso <- st_transform(x = iso, oprj)
  } else {
    iso <- st_transform(x = iso, 4326)
  }

  return(iso)
}
