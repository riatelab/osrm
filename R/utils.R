## All Functions Utils
testSp <- function(x){
  if (methods::is(x,"Spatial")){
    return(TRUE)
  }
  return(FALSE)
}

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


sfToDf <- function(x){
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
                  lon = format(round(coords[,1],5), 
                               scientific = FALSE, trim = TRUE), 
                  lat = format(round(coords[,2],5), 
                               scientific = FALSE, trim = TRUE), 
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
  st_geometry(iso) <- lwgeom::st_make_valid(st_geometry(iso))
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
tableLoc <- function(loc, gepaf = FALSE){
  # Query build
  if (gepaf == TRUE){
    tab <- paste0(getOption("osrm.server"), "table/v1/", getOption("osrm.profile"), "/polyline(")
    tab <- paste0(tab, gepaf::encodePolyline(loc[,c("lat","lon")]),")")
  }else{
    tab <- paste0(getOption("osrm.server"), "table/v1/", getOption("osrm.profile"), "/")
    tab <- paste0(tab, paste(format(loc$lon, scientific = FALSE, trim = TRUE), 
                             format(loc$lat, scientific = FALSE, trim = TRUE), 
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
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & (nSrc*nDst) > 10000){
    stop(e)
  }
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & nreq >= 8000){
    stop(e2)
  }
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & nSrc > 100 & nDst==0){
    stop(e3)
  }
  
}