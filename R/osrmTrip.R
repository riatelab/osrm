#' @name osrmTrip
#' @title Get the Travel Geometry Between Multiple Unordered Points
#' @description Build and send an OSRM API query to get the shortest travel
#' geometry between multiple unordered points.
#' This function interfaces the \emph{trip} OSRM service.\cr
#' Use this function to resolve the travelling salesman problem.
#' @param loc starting point and waypoints to reach along the
#' route. \code{loc} can be: \itemize{
#'   \item a data.frame of longitudes and latitudes (WGS 84),
#'   \item a matrix of longitudes and latitudes (WGS 84),
#'   \item an sfc object of type POINT,
#'   \item an sf object of type POINT.
#' }
#' The first row or element is the starting point.\cr
#' Row names, if relevant, or element indexes are used as identifiers.
#' @param exclude pass an optional "exclude" request option to the OSRM API.
#' @param overview "full", "simplified". Add geometry either full (detailed) or simplified
#' according to highest zoom level it could be display on.
#' @param returnclass deprecated.
#' @param osrm.server the base URL of the routing server.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot".
#' @details As stated in the OSRM API, if input coordinates can not be joined by a single trip
#' (e.g. the coordinates are on several disconnected islands) multiple trips for
#' each connected component are returned.
#' @return A list of connected components is returned. Each component contains:
#' \describe{
#' \item{trip}{
#' An sf LINESTRING. If loc is a data.frame or a matrix the coordinate
#' reference system (CRS) of the route is EPSG:4326 (WGS84). If loc is an sfc or
#' sf object, the route has the same CRS as loc.\cr
#' Each line of the returned route is a step of the trip. The object has four
#' columns: start (identifier of the starting point),
#' end (identifier of the destination), duration (duration of the step in minutes),
#' distance (length of the step in kilometers).
#' }
#' \item{summary}{A list with 2 components: total duration (in minutes)
#' and total distance (in kilometers) of the trip.}
#' }
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#'   quiet = TRUE
#' )
#' # Get a trip with a set of points (sf POINT)
#' trips <- osrmTrip(loc = apotheke.sf[1:5, ])
#' mytrip <- trips[[1]]$trip
#' # Display the trip
#' plot(st_geometry(mytrip), col = "black", lwd = 4)
#' plot(st_geometry(mytrip), col = c("red", "white"), lwd = 1, add = TRUE)
#' plot(st_geometry(apotheke.sf[1:5, ]),
#'   pch = 21, bg = "red", cex = 1,
#'   add = TRUE
#' )
#' }
osrmTrip <- function(loc, exclude = NULL, overview = "simplified",
                     returnclass, osrm.server = getOption("osrm.server"),
                     osrm.profile = getOption("osrm.profile")) {
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)

  if (!missing(returnclass)) {
    warning('"returnclass" is deprecated.', call. = FALSE)
  }

  url <- base_url(osrm.server, osrm.profile, "trip")

  # input points
  loc <- input_route(x = loc, id = "loc", single = FALSE, all.ids = TRUE)
  oprj <- loc$oprj

  # build url
  url <- paste0(
    url,
    paste(clean_coord(loc$lon), clean_coord(loc$lat),
      sep = ",", collapse = ";"
    ),
    "?steps=false&geometries=geojson&overview=",
    tolower(overview),
    "&generate_hints=false"
  )

  # adding exclude parameter
  if (!missing(exclude)) {
    url <- paste0(url, "&exclude=", exclude)
  }

  # Send the query
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


  # Get all the waypoints
  waypointsg <- data.frame(res$waypoints[, c(1, 2, 5)],
    matrix(unlist(res$waypoints$location),
      byrow = TRUE, ncol = 2
    ),
    id = loc$id
  )

  # In case of island, multiple trips
  ntour <- dim(res$trips)[1]
  trips <- vector("list", ntour)
  for (nt in 1:ntour) {
    # Coordinates of the line
    geodf <- data.frame(res$trips[nt, ]$geometry[[1]]$coordinates)
    geodf$id <- seq_len(nrow(geodf))

    # In case of unfinnish trip
    if (geodf[nrow(geodf), 1] != geodf[1, 1]) {
      geodf <- rbind(geodf, geodf[1, ])
    }

    # Extract trip waypoints
    waypoints <- waypointsg[waypointsg$trips_index == (nt - 1), ]
    geodf$id_wp <- NA
    waypoints <- waypoints[order(waypoints$waypoint_index, decreasing = FALSE), ]
    j <- 1
    for (i in 1:(nrow(geodf) - 1)) {
      if (any(geodf[i, c("X1", "X2")] == waypoints[j, c("X1", "X2")])) {
        geodf$id_wp[i] <- waypoints[j, "id"]
        j <- j + 1
        if (j > nrow(waypoints)) {
          j <- 1
        }
      }
    }

    if (is.na(geodf[nrow(geodf), "id_wp"])) {
      geodf[nrow(geodf), "id_wp"] <- geodf[1, "id_wp"]
    }

    l <- list()
    j <- 1
    for (i in seq_len(nrow(geodf))) {
      if (!is.na(geodf[i, "id_wp"])) {
        l[[j]] <- geodf[i, "id"]
        j <- j + 1
      }
    }
    # Build the polylines
    wktl <- rep(NA, nrow(waypoints))
    for (i in seq_along(wktl)) {
      aind <- l[[i]]:l[[i + 1]]
      wktl[i] <- paste("LINESTRING(",
        paste(geodf[aind, 1], " ",
          geodf[aind, 2],
          sep = "", collapse = ","
        ),
        ")",
        sep = ""
      )
    }
    start <- (waypoints[order(waypoints$waypoint_index, decreasing = FALSE), "id"])
    end <- start[c(2:length(start), 1)]
    sldf <- st_sf(
      start = start, end = end,
      duration = res$trips[nt, ]$legs[[1]][, "duration"] / 60,
      distance = res$trips[nt, ]$legs[[1]][, "distance"] / 1000,
      geometry = st_as_sfc(wktl, crs = 4326)
    )
    # Reproj
    if (!is.na(oprj)) {
      sldf <- sf::st_transform(sldf, oprj)
    }
    # Build tripSummary
    tripSummary <- list(
      duration = res$trips[nt, ]$duration / 60,
      distance = res$trips[nt, ]$distance / 1000
    )
    trips[[nt]] <- list(trip = sldf, summary = tripSummary)
  }
  return(trips)
}
