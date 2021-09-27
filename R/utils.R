## All Functions Utils
testSf <- function(x){
  if (methods::is(x,"sf")){
    if (is.na(sf::st_crs(x))){
      stop(
        paste(
          "Your input (", quote(x),
          ") does not have a valid coordinate reference system.", sep=""),
        call. = F)
    }
    return(TRUE)
  }
  return(FALSE)
}

clean_coord <- function(x){
  format(round(as.numeric(x),5), scientific = FALSE, justify = "none", 
         trim = TRUE, nsmall = 5, digits = 5)
}



sfToDf <- function(x){    
  if (is.na(sf::st_crs(x))){
    stop(
      paste(
        "Your input (", quote(x),
        ") does not have a valid coordinate reference system.", sep=""),
      call. = F)
  }
  # transform to centroid and to wgs84
  if (methods::is(st_geometry(x), c("sfc_GEOMETRY", 'sfc_GEOMETRYCOLLECTION'))){
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
  }
  if (methods::is(st_geometry(x), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
    sf::st_geometry(x) <- sf::st_centroid(x = sf::st_geometry(x),
                                          of_largest_polygon = T)
  }
  x <- sf::st_transform(x = x, crs = 4326)
  coords <- sf::st_coordinates(x)
  # this function takes an sf and transforms it into a dataframe
  x <- data.frame(id = row.names(x), 
                  lon = clean_coord(coords[,1]), 
                  lat = clean_coord(coords[,2]), 
                  stringsAsFactors = FALSE)
  return(x)
}



## osrmIsochrone Utils
#' @import sf
isopoly <- function(x, breaks, 
                    xcoords = "COORDX", ycoords = "COORDY", var = "OUTPUT"){
  
  # get initial min and max values
  vmin <- min(x[[var]], na.rm = TRUE)
  vmax <- max(x[[var]], na.rm = TRUE)
  breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
  # data points to matrix
  m <- matrix(data = x[[var]], nrow = length(unique(x[[xcoords]])), 
              dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]])))
  # compute isobands
  lev_low = breaks[1:(length(breaks)-1)]
  lev_high = breaks[2:length(breaks)]
  raw <- isoband::isobands(x = as.numeric(rownames(m)), 
                           y = as.numeric(colnames(m)), z = t(m), 
                           levels_low = lev_low,
                           levels_high = c(lev_high[-length(lev_high)], 
                                           vmax + 1e-10))
  bands <- isoband::iso_to_sfg(raw)
  iso <- st_sf(id = 1:length(bands), 
               min = lev_low, 
               max = lev_high,
               geometry = sf::st_sfc(bands), 
               crs = st_crs(x))
  iso[1,"min"] <- 0
  iso$center = iso$min + (iso$max - iso$min) / 2
  
  # invalid polygons mgmnt
  st_geometry(iso) <- st_make_valid(st_geometry(iso))  

  
  if(methods::is(st_geometry(iso),c("sfc_GEOMETRYCOLLECTION", "sfc_GEOMETRY"))){
    st_geometry(iso) <-   sf::st_collection_extract(st_geometry(iso), "POLYGON")
  }
  # get rid of out of breaks polys
  iso <- iso[-nrow(iso),]
  return(iso)
}


rgrid <- function(loc, dmax, res){
  # create a regular grid centerd on loc
  boxCoordX <- seq(from = sf::st_coordinates(loc)[1,1] - dmax,
                   to = sf::st_coordinates(loc)[1,1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = sf::st_coordinates(loc)[1,2] - dmax,
                   to = sf::st_coordinates(loc)[1,2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1),
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sf::st_as_sf(sgrid,  coords = c("COORDX", "COORDY"),
                        crs = st_crs(loc), remove = FALSE)
  return(sgrid)
}




## osrmTable Utils
durTableFormat <- function(res, src, dst){
  # extract distance table
  mat <- res$durations
  # From sec to minutes
  mat <- round(mat/(60), 1)
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
}  


distTableFormat <- function(res, src, dst){
  # extract distance table
  mat <- res$distances
  # rounding meters
  mat <- round(mat, 0)
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
} 

coordFormat <- function(res, src, dst){
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

tableLoc <- function(loc, gepaf = FALSE,  osrm.server, 
                     osrm.profile){
  # Query build
  if (gepaf == TRUE){
    tab <- paste0(osrm.server, "table/v1/", osrm.profile, "/polyline(")
    loc$lat <- as.numeric(as.character(loc$lat))
    loc$lon <- as.numeric(as.character(loc$lon))
    tab <- paste0(tab, googlePolylines::encode(loc[,c("lon","lat")]),")")
  }else{
    tab <- paste0(osrm.server, "table/v1/", osrm.profile, "/")
    tab <- paste0(tab, paste(clean_coord(loc$lon), 
                             clean_coord(loc$lat), 
                             sep=",",collapse = ";"))
  }
  return(tab)
}

osrmLimit <- function(nSrc, nDst, nreq){
  e <- simpleError("The public OSRM API does not allow results with a number of durations 
higher than 10000. Ask for fewer durations or use your own server and set its 
--max-table-size option.")
  e2 <- simpleError("This request is to large for the public OSRM API. Ask for 
fewer durations or use your own server and set its --max-table-size option.")
  e3 <- simpleError("This request is to large for the public OSRM API. Ask for 
fewer locations or use your own server and set its --max-trip-size option.")
  if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & (nSrc*nDst) > 9998){
    stop(e)
  }
  if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nreq >= 8000){
    stop(e2)
  }
  if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nSrc > 99 & nDst==0){
    stop(e3)
  }
  if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nSrc > 99 & nDst==0){
    stop(e3)
  }
}



input_route <- function(x, id, single = TRUE){
  # test various cases (vector, data.frame, with or without id, sf, sp)
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
    if(methods::is(x,"Spatial")){
      warn_sp()
      x <- st_as_sf(x[1,])
    }
    if(is.data.frame(x)){
      if(methods::is(x,"sf")){
        oprj <- sf::st_crs(x)
        x <- sfToDf(x)
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
    if(methods::is(x,"Spatial")){
      warn_sp()
      x <- st_as_sf(x)
    }
    if(is.data.frame(x)){
      if(methods::is(x,"sf")){
        oprj <- sf::st_crs(x)
        x <- sfToDf(x)
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


warn_sp <- function(){
  .Deprecated(
    msg = paste0("sp support will be dropped in the next release, ",
                 "please use sf objects instead.")
  )
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop(
      "'sp' is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
}
