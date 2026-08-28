rgrid <- function(loc, dmax, res) {
  # create a regular grid centerd on loc
  coords <- sf::st_coordinates(loc)
  xf <- coords[1, 1]
  yf <- coords[1, 2]
  boxCoordX <- seq(
    from = xf - dmax,
    to = xf + dmax,
    length.out = res
  )
  boxCoordY <- seq(
    from = yf - dmax,
    to = yf + dmax,
    length.out = res
  )
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(
    ID = seq(1, nrow(sgrid), 1),
    COORDX = sgrid[, 1],
    COORDY = sgrid[, 2]
  )
  sgrid <- sf::st_as_sf(sgrid,
    coords = c("COORDX", "COORDY"),
    crs = st_crs(loc), remove = FALSE
  )
  return(sgrid)
}


# output formating
tab_format <- function(res, src, dst, type) {
  if (type == "duration") {
    mat <- res$durations
    # From sec to minutes
    mat <- round(mat / (60), 1)
  } else {
    mat <- res$distances
    mat <- round(mat, 0)
  }
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
}


coord_format <- function(res, src, dst) {
  sources <- data.frame(matrix(
    unlist(res$sources$location,
      use.names = TRUE
    ),
    ncol = 2, byrow = TRUE,
    dimnames = list(src$id, c("lon", "lat"))
  ))
  sources$snapping_distance <- round(res$sources$distance, 0)

  destinations <- data.frame(matrix(
    unlist(res$destinations$location,
      use.names = TRUE
    ),
    ncol = 2, byrow = TRUE,
    dimnames = list(dst$id, c("lon", "lat"))
  ))
  destinations$snapping_distance <- round(res$destinations$distance, 0)

  return(list(sources = sources, destinations = destinations))
}

# flag points with missing (NA/NaN) or non-finite coordinates and warn
# about the skipped points, encoding or formatting such coordinates would
# produce an invalid OSRM query
valid_coords <- function(lon, lat, id) {
  ok <- is.finite(lon) & is.finite(lat)
  n_skip <- sum(!ok)
  if (n_skip > 0) {
    warning(
      paste0(
        n_skip, ' point(s) with missing (NA/NaN) or non-finite ',
        'coordinates found in "', id, '" and skipped.'
      ),
      call. = FALSE
    )
  }
  ok
}

input_table <- function(x, id) {
  if (inherits(x = x, what = c("sfc", "sf"))) {
    lx <- length(st_geometry(x))
    if (lx < 1) {
      stop(paste0('"', id, '" should have at least 1 row or element.'),
        call. = FALSE
      )
    }
    type <- sf::st_geometry_type(x, by_geometry = TRUE)
    type <- as.character(unique(type))
    if (length(type) > 1 || type != "POINT") {
      stop(paste0('"', id, '" geometry should be of type POINT.'),
        call. = FALSE
      )
    }
    if (inherits(x, "sfc")) {
      idx <- 1:lx
    } else {
      idx <- row.names(x)
    }
    x <- sf::st_transform(x = x, crs = 4326)
    coords <- sf::st_coordinates(x)
    ok <- valid_coords(coords[, 1], coords[, 2], id)
    if (sum(ok) < 1) {
      stop(
        paste0(
          '"', id,
          '" should have at least 1 row or element with valid coordinates.'
        ),
        call. = FALSE
      )
    }
    x <- data.frame(
      id = idx[ok],
      lon = clean_coord(coords[ok, 1]),
      lat = clean_coord(coords[ok, 2])
    )
    return(x)
  }
  if (inherits(x = x, what = c("data.frame", "matrix"))) {
    lx <- nrow(x)
    if (lx < 1) {
      stop(paste0('"', id, '" should have at least 1 row or element.'),
        call. = FALSE
      )
    }
    if (ncol(x) == 2 && is.numeric(x[, 1, drop = TRUE]) && is.numeric(x[, 2, drop = TRUE])) {
      rn <- row.names(x)
      if (is.null(rn)) {
        rn <- 1:lx
      }
      ok <- valid_coords(x[, 1, drop = TRUE], x[, 2, drop = TRUE], id)
      if (sum(ok) < 1) {
        stop(
          paste0(
            '"', id,
            '" should have at least 1 row or element with valid coordinates.'
          ),
          call. = FALSE
        )
      }
      x <- data.frame(
        id = rn[ok],
        lon = clean_coord(x[ok, 1, drop = TRUE]),
        lat = clean_coord(x[ok, 2, drop = TRUE])
      )
      return(x)
    } else {
      stop(paste0('"', id, '" should contain coordinates.'),
        call. = FALSE
      )
    }
  } else {
    stop(
      paste0(
        '"', id, '" should be ',
        "a data.frame or a matrix ",
        "of coordinates, an sfc POINT object or an ",
        "sf POINT object."
      ),
      call. = FALSE
    )
  }
}


# x <- x_v
input_route <- function(x, id, single = TRUE, all.ids = FALSE) {
  # test various cases (vector, data.frame, sf or sfc)
  oprj <- NA
  if (single) {
    if (is.vector(x)) {
      if (length(x) == 2 && is.numeric(x)) {
        if (any(!is.finite(x))) {
          stop(
            paste0(
              '"', id,
              '" contains missing (NA/NaN) or non-finite coordinates.'
            ),
            call. = FALSE
          )
        }
        if (x[1] > 180 || x[1] < -180 || x[2] > 90 || x[2] < -90) {
          stop(
            paste0(
              "longitude is bounded by the interval [-180, 180], ",
              "latitude is bounded by the interval [-90, 90]"
            ),
            call. = FALSE
          )
        }
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = id, lon = lon, lat = lat, oprj = oprj))
      } else {
        stop(
          paste0(
            '"', id, '" should be a numeric vector of length 2, ',
            "i.e., c(lon, lat)."
          ),
          call. = FALSE
        )
      }
    }
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      if (length(st_geometry(x)) > 1) {
        message(paste0('Only the first row/element of "', id, '" is used.'))
      }
      if (inherits(x, "sfc")) {
        x <- x[1]
        idx <- id
      } else {
        x <- x[1, ]
        idx <- row.names(x)
      }
      if (sf::st_geometry_type(x, by_geometry = FALSE) != "POINT") {
        stop(paste0('"', id, '" geometry should be of type POINT.'),
          call. = FALSE
        )
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      if (any(!is.finite(coords[1, ]))) {
        stop(
          paste0(
            '"', id,
            '" contains missing (NA/NaN) or non-finite coordinates.'
          ),
          call. = FALSE
        )
      }
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      if (nrow(x) > 1) {
        message(paste0('Only the first row of "', id, '" is used.'))
        x <- x[1, , drop = FALSE]
      }
      idx <- row.names(x)
      if (is.null(idx)) {
        idx <- id
      }
      x <- unlist(x)
      if (length(x) == 2 && is.numeric(x)) {
        if (any(!is.finite(x))) {
          stop(
            paste0(
              '"', id,
              '" contains missing (NA/NaN) or non-finite coordinates.'
            ),
            call. = FALSE
          )
        }
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
      } else {
        stop(paste0('"', id, '" should contain coordinates.'),
          call. = FALSE
        )
      }
    } else {
      stop(
        paste0(
          '"', id, '" should be a vector of coordinates, ',
          "a data.frame or a matrix ",
          "of coordinates, an sfc POINT object or an ",
          "sf POINT object."
        ),
        call. = FALSE
      )
    }
  } else {
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      lx <- length(st_geometry(x))
      if (lx < 2) {
        stop('"loc" should have at least 2 rows or elements.',
          call. = FALSE
        )
      }
      type <- sf::st_geometry_type(x, by_geometry = FALSE)
      type <- as.character(unique(type))
      if (length(type) > 1 || type != "POINT") {
        stop('"loc" geometry should be of type POINT', call. = FALSE)
      }
      if (inherits(x, "sfc")) {
        id1 <- "src"
        id2 <- "dst"
        rn <- 1:lx
      } else {
        rn <- row.names(x)
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      ok <- valid_coords(coords[, 1], coords[, 2], "loc")
      if (sum(ok) < 2) {
        stop(
          '"loc" should have at least 2 rows or elements with valid coordinates.',
          call. = FALSE
        )
      }
      coords <- coords[ok, , drop = FALSE]
      rn <- rn[ok]
      if (!inherits(x, "sfc")) {
        id1 <- rn[1]
        id2 <- rn[length(rn)]
      }
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      if (!all.ids) {
        return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
      } else {
        return(list(id = rn, lon = lon, lat = lat, oprj = oprj))
      }
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      lx <- nrow(x)
      if (lx < 2) {
        stop('"loc" should have at least 2 rows.', call. = FALSE)
      }
      if (ncol(x) == 2 && is.numeric(x[, 1, drop = TRUE]) && is.numeric(x[, 2, drop = TRUE])) {
        rn <- row.names(x)
        if (is.null(rn)) {
          rn <- 1:lx
        }
        ok <- valid_coords(x[, 1, drop = TRUE], x[, 2, drop = TRUE], "loc")
        if (sum(ok) < 2) {
          stop(
            '"loc" should have at least 2 rows with valid coordinates.',
            call. = FALSE
          )
        }
        lon <- clean_coord(x[ok, 1, drop = TRUE])
        lat <- clean_coord(x[ok, 2, drop = TRUE])
        rn <- rn[ok]
        id1 <- rn[1]
        id2 <- rn[length(rn)]
        if (!all.ids) {
          return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
        } else {
          return(list(id = rn, lon = lon, lat = lat, oprj = oprj))
        }
      } else {
        stop(paste0('"loc" should contain coordinates.'),
          call. = FALSE
        )
      }
    } else {
      stop(
        paste0(
          '"loc" should be ',
          "a data.frame or a matrix ",
          "of coordinates, an sfc POINT object or an ",
          "sf POINT object."
        ),
        call. = FALSE
      )
    }
  }
}


# construct the base url
base_url <- function(osrm.server, osrm.profile, query) {
  if (osrm.server == "https://routing.openstreetmap.de/") {
    url <- paste0(
      osrm.server, "routed-", osrm.profile, "/",
      query, "/v1/driving/"
    )
  } else {
    url <- paste0(osrm.server, query, "/v1/", osrm.profile, "/")
  }
  return(url)
}

# create short and clean coordinates
clean_coord <- function(x) {
  format(round(as.numeric(x), 5),
    scientific = FALSE, justify = "none",
    trim = TRUE, nsmall = 5, digits = 5
  )
}


encode_coords <- function(x, osrm.server) {
  x$lat <- as.numeric(as.character(x$lat))
  x$lon <- as.numeric(as.character(x$lon))
  if (osrm.server == "https://routing.openstreetmap.de/") {
    result <- paste(
      clean_coord(x$lon),
      clean_coord(x$lat),
      sep = ",", collapse = ";"
    )
  } else {
    result <- paste0(
      "polyline(",
      googlePolylines::encode(x[, c("lon", "lat")]),
      ")"
    )
  }
  return(result)
}


test_http_error <- function(r) {
  if (r$status_code >= 400) {
    if (substr(r$type, 1, 16) != "application/json") {
      stop(
        sprintf(
          "OSRM API request failed [%s]",
          r$status_code
        ),
        call. = FALSE
      )
    } else {
      rep <- RcppSimdJson::fparse(rawToChar(r$content))
      stop(
        sprintf(
          "OSRM API request failed [%s]\n%s\n%s",
          r$status_code,
          rep$code,
          rep$message
        ),
        call. = FALSE
      )
    }
  }
  return(NULL)
}

fill_grid <- function(destinations, measure, sgrid, res, tmax) {
  rpt <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)
  rpt <- st_transform(rpt, 3857)
  rpt$measure <- measure
  b <- as.numeric(st_distance(sgrid[1, ], sgrid[2, ]) / 2)
  xx <- st_make_grid(
    x = st_buffer(
      x = st_as_sfc(st_bbox(sgrid)),
      dist = b
    ),
    n = c(res, res)
  )
  ag_pt <- function(x) {
    if (length(x) > 0) {
      min(rpt[["measure"]][x], na.rm = TRUE)
    } else {
      NA
    }
  }
  inter <- st_intersects(xx, rpt)
  sgrid$measure <- unlist(lapply(inter, ag_pt))
  sgrid[is.infinite(sgrid$measure), "measure"] <- NA
  sgrid[is.nan(sgrid$measure), "measure"] <- NA
  sgrid
}

get_resolution <- function(res, n) {
  ref <- data.frame(
    res = c(13, 18, 27, 37, 52, 81, 114, 161, 254),
    n = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)
  )

  if (!missing(res)) {
    message("'res' is deprecated, use 'n' instead.")
    return(res)
  }
  if (!n %in% ref$n) {
    warning("n is not set to an accepted value, n = 500 will be used.",
      call. = FALSE
    )
    return(27)
  }
  return(ref[ref$n == n, "res"])
}
