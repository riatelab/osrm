#' @name osrmIsochrone
#' @title Get a SpatialPolygonsDataFrame of Isochrones
#' @description Based on \code{\link{osrmTable}}, this function buids a 
#' SpatialPolygonsDataFrame of isochrones. 
#' @param loc a numeric vector of longitude and latitude (WGS84) or a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values (in minutes).
#' @return A SpatialPolygonsDateFrame of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @note This function uses raster and rgeos packages.
#' @seealso \link{osrmTable}
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(5.936036, 49.24882))
#' plot(iso)
#' points(5.936036, 49.24882, pch = 20, col = "red")
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = iso, crop = TRUE)
#'   tilesLayer(osm)
#'   osm@crs
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso, df = iso@data,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[7,], breaks = seq(from = 0,to = 30, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(spdf = iso2, crop = TRUE)
#'   tilesLayer(osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2, df = iso2@data,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' }
osrmIsochrone <- function(loc, breaks = seq(from = 0,to = 60, length.out = 7)){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("'raster' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:raster' %in% search()){
    attachNamespace('raster')
  }

  oprj <- NA
  if(testSp(loc)){
    oprj <- sp::proj4string(loc)
    loc <- loc[1,]
    loc <- sp::spTransform(x = loc, CRSobj = "+init=epsg:3857")
  }else{
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- sp::SpatialPointsDataFrame(coords = loc[,1:2], 
                                      data = loc, 
                                      proj4string = sp::CRS("+init=epsg:4326"))
    loc <- sp::spTransform(x = loc, CRSobj = sp::CRS( "+init=epsg:3857"))
  }
  
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  speed <- 140 * 1000/60
  dmax <- tmax * speed
  res <- 30
  sgrid <- rgrid(loc = sp::coordinates(loc), dmax = dmax, res = res)
  
  row.names(loc) <- "0"
  
  if(getOption("osrm.server") != "http://router.project-osrm.org/"){
    dmat <- osrmTable(src = loc, dst = sgrid)
    durations <- dmat$durations
    destinations <- dmat$destinations
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid[1:500,])
    Sys.sleep(1)
    dmat1 <- osrmTable(src = loc, dst = sgrid[501:900,])
    durations <- cbind(dmat$durations, dmat1$durations)
    destinations <- rbind(dmat$destinations, dmat1$destinations)
  }
  
  rpt <- sp::SpatialPointsDataFrame(coords = destinations[ , c(1, 2)],
                                    data = data.frame(destinations),
                                    proj4string = sp::CRS("+init=epsg:4326"))
  rpt <- sp::spTransform(rpt, sp::proj4string(loc))
  rpt$d <- as.vector(durations)
  rpt$d[is.na(rpt$d)] <- max(rpt$d, na.rm=TRUE)
  sp::gridded(sgrid) <- TRUE
  r <- raster::raster(sgrid)
  r <- raster::rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,
                         background= max(rpt$d, na.rm=TRUE)+1)
  isolines <- rasterToContourPoly(r = r, breaks = breaks)
  # contour correction
  isolines <- isolines[-1,]
  isolines@data[nrow(isolines), "min"] <- 0
  isolines@data[nrow(isolines), "center"] <- (isolines@data[nrow(isolines), "max"] - 
                                                isolines@data[nrow(isolines), "min"]) / 2
  # reproj
  if (!is.na(oprj)){
    isolines <- sp::spTransform(x = isolines, CRSobj = oprj)
  }else{
    isolines <- sp::spTransform(x = isolines, CRSobj = "+init=epsg:4326")
  }
  return(isolines)
}
