#' @name osrmIsochrone
#' @title Get a SpatialPolygonsDataFrame of Isochrones
#' @description Based on osrmTable this function buid a SpatialPolygonsDataFrame 
#' of isochrones. 
#' @param loc a numeric vector of latitude and longitude (WGS84) or a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values.
#' @return A SpatialPolygonsDateFrame of isochrones is returned. 
#' The data frame of the outputed SpatialPolygonsDataFrame contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes)
#' @note This function uses the rgeos package.
#' @export
osrmIsochrone <- function(loc, breaks = seq(from = 0,to = 60, length.out = 7)){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("'raster' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:raster' %in% search()){
    attachNamespace('raster')
  }
  
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  speed <- 150
  dmax <-  4 * 100000 * tmax/speed
  res <- 30
  oprj <- NA
  if(testSp(loc)){
    oprj <- sp::proj4string(loc)
    loc <- loc[1,]
    loc <- sp::spTransform(x = loc, CRSobj = "+init=epsg:3857")
  }else{
    loc <- sp::SpatialPointsDataFrame(coords = data.frame(loc), 
                                      data = loc, 
                                      proj4string = CRS("+init=epsg:4326"))
    loc <- sp::spTransform(x = loc, CRSobj =CRS( "+init=epsg:3857"))
  }
  sgrid <- rgrid(loc = sp::coordinates(loc), dmax = dmax, res = res)
  plot(sgrid)
  layoutLayer()
  dmat <- osrmTable(src = loc, dst = sgrid)
  rpt <- sp::SpatialPointsDataFrame(coords = dmat$destination_coordinates[ , c(2, 1)],
                                    data = data.frame(dmat$destination_coordinates),
                                    proj4string = CRS("+init=epsg:4326"))
  
  rpt <- sp::spTransform(rpt, proj4string(loc))
  rpt$d <- as.vector(dmat$distance_table)
  rpt$d[is.na(rpt$d)] <- max(rpt$d, na.rm=TRUE)
  sp::gridded(sgrid) <- TRUE
  
  r <- raster::raster(sgrid)
  r <- raster::rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,
                         background= max(rpt$d, na.rm=TRUE)+1)
  plot(r)
  
  isolines <- rasterToContourPoly(r = r, breaks = breaks)
  isolines@data
  if (!is.na(oprj)){
    isolines <- sp::spTransform(x = isolines, CRSobj = oprj)
  }else{
    isolines <- sp::spTransform(x = isolines, CRSobj = "+init=epsg:4326")
  }
  
  return(isolines)
}
