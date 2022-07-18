rgrid <- function(loc, dmax, res){
  # create a regular grid centerd on loc
  coords <- sf::st_coordinates(loc)
  xf <- coords[1,1]
  yf <- coords[1,2]
  boxCoordX <- seq(from = xf - dmax,
                   to = xf + dmax,
                   length.out = res)
  boxCoordY <- seq(from = yf - dmax,
                   to = yf + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1),
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sf::st_as_sf(sgrid,  coords = c("COORDX", "COORDY"),
                        crs = st_crs(loc), remove = FALSE)
  return(sgrid)
}



# output formating
tab_format <- function(res, src, dst, type){
  if(type == "duration"){
    mat <- res$durations
    # From sec to minutes
    mat <- round(mat/(60), 1)
  }else{
    mat <- res$distances
    mat <- round(mat, 0)
  }
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
}  


coord_format <- function(res, src, dst){
  sources <- data.frame(matrix(unlist(res$sources$location, 
                                      use.names = T), 
                               ncol = 2, byrow = T, 
                               dimnames = list(src$id, c("lon", "lat"))))
  destinations <- data.frame(matrix(unlist(res$destinations$location, 
                                           use.names = T), 
                                    ncol = 2, byrow = T, 
                                    dimnames = list(dst$id, c("lon", "lat"))))
  return(list(sources = sources, destinations = destinations)
  )
}




input_route <- function(x, id, single = TRUE){
  # test various cases (vector, data.frame, with or without id, sf)
  oprj <- NA
  if(single){
    if(is.vector(x)){
      if(length(x) == 2){
        id <- id
        i <- 0
      }else{
        i <- 1
        id <- x[i]
      }
      lon <- clean_coord(x[i+1])
      lat <- clean_coord(x[i+2])       
    }
    if(is.data.frame(x)){
      if(methods::is(x,"sf")){
        oprj <- sf::st_crs(x)
        x <- sf_2_df(x)
        i <- 1
        id <- x[1, i]
      }else{
        if(length(x) == 2){
          i <- 0
          id <- id
        }else{
          i <- 1
          id <- x[1, i]
        }
      }
      lon <- clean_coord(x[1, i+1])
      lat <- clean_coord(x[1, i+2])       
    }
    return(list(id = id, lon = lon, lat = lat, oprj = oprj))
  }else{
    if(is.data.frame(x)){
      if(methods::is(x,"sf")){
        oprj <- sf::st_crs(x)
        x <- sf_2_df(x)
        i <- 1
        id1 <- x[1,1]
        id2 <- x[nrow(x),1]
      }else{
        if(length(x) == 2){
          i <- 0
          id1 <- "src"
          id2 <- "dst"
        }else{
          i <- 1
          id1 <- x[1,1]
          id2 <- x[nrow(x),1]
        }
      }
      lon <- clean_coord(x[, i+1])
      lat <- clean_coord(x[, i+2])       
    }
    return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
  }
} 


# construct the base url
base_url <- function(osrm.server, osrm.profile, query){
  if(osrm.server == "https://routing.openstreetmap.de/") {
    url <- paste0(osrm.server, "routed-", osrm.profile, "/", 
                  query, "/v1/driving/")
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
  paste0("polyline(", googlePolylines::encode(x[,c("lon","lat")]),")")
  # paste(clean_coord(x$lon), clean_coord(x$lat), sep=",",collapse = ";")
}



test_http_error <- function(r){
  if (r$status_code >= 400) {
    if (substr(r$type, 1, 16) != "application/json") {
      stop(
        sprintf(
          "OSRM API request failed [%s]", 
          r$status_code),
        call. = FALSE)
    }else{
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

