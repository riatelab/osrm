#' @name osrmIsochrone
#' @title Get a SpatialPolygonsDataFrame of Isochrones
#' @description Based on \code{\link{osrmTable}}, this function buids a 
#' SpatialPolygonsDataFrame of isochrones. 
#' @param loc a numeric vector of longitude and latitude (WGS84) or a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values (in minutes).
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param res number of points used to compute isochrones, one side of the square 
#' grid, the total number of points will be res*res.  
#' @return A SpatialPolygonsDateFrame of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @seealso \link{osrmTable}
#' @import sp
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("berlin")
#' 
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(13.43853,52.47728), breaks = seq(0,15,1), res = 70)
#' library(sp)
#' plot(iso, col = colorRampPalette(colors = c('grey80', 'grey20'))(14))
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso,
#'                           var = "center", breaks = breaks,
#'                           col = paste0(rev(carto.pal("green.pal",
#'                                                      length(breaks)+1)),99),
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE,
#'                           legend.title.txt = "Isochrones\n(min)",
#'                           add = TRUE)
#' }
#' 
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = apotheke.sp[10,],
#'                       breaks = seq(from = 0, to = 16, by = 2))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(x = iso2, crop = FALSE, type = "osmgrayscale")
#'   tilesLayer(x = osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE,
#'                           legend.title.txt = "Isochrones\n(min)",
#'                           add = TRUE)
#' }
#' }
osrmIsochrone <- function(loc, breaks = seq(from = 0,to = 60, length.out = 7), 
                          exclude = NULL, res = 30){
  # loc = c(13.43853,52.47728)
  # breaks = seq(0,15,1)
  # res = 30
  # 
  oprj <- NA
  if(testSp(loc)){
    loc <- sf::st_as_sf(loc[1,])
  }    
  if(testSf(loc)){
    oprj <- sf::st_crs(loc)
    loc <- loc[1,]
  }else{
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- st_as_sf(loc, coords=c("lon","lat"), crs = 4326)
  }
  loc <- sf::st_transform(loc, 3857)

  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if(options('osrm.profile')=="walk"){
    speed =  10 * 1000/60
  }
  if(options('osrm.profile')=="bike"){
    speed =  20 * 1000/60
  }
  if(options('osrm.profile')=="driving"){
    speed =  140 * 1000/60
  }
  # speed <- 140 * 1000/60
  dmax <- tmax * speed
  
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% 300
  r500 <- lsgr %% 300

  row.names(loc) <- "0"
  listDur <- list()
  listDest <- list()

  
  
  if(getOption("osrm.server") != "http://router.project-osrm.org/"){
    sleeptime <- 0
  }else{
    sleeptime <- 1
  }
  # exclude = NULL
  
  
  if(f500>0){
    for (i in 1:f500){
      st <- (i-1) * 300 + 1
      en <- i * 300
      dmat <- osrmTable(src = loc, dst = sgrid[st:en,], exclude = exclude)
      durations <- dmat$durations
      listDur[[i]] <- dmat$durations
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if(r500>0){
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),], exclude = exclude)
      listDur[[i+1]] <- dmat$durations
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude)
    listDur[[1]] <- dmat$durations
    listDest[[1]] <- dmat$destinations
  }

  durations <- do.call(c, listDur)
  destinations <- do.call(rbind, listDest)
  rpt <- sf::st_as_sf(destinations, coords = c('lon', 'lat'),
                      crs = 4326)
  rpt <- sf::st_transform(rpt, sf::st_crs(loc))
  # rpt$d <- as.vector(durations)
  # rpt$d[is.na(rpt$d)] <- max(rpt$d, na.rm=TRUE)
  # r <- raster::raster(sgrid, ncols = res, nrows =res)
  # r <- raster::rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,
  #                        background= max(rpt$d, na.rm=TRUE)+1)
  # plot(rpt, add=T)
  sgrid$OUTPUT <- durations
  isolines <- isopoly(x = sgrid, xcoords = "COORDX", ycoords = "COORDY", var = "OUTPUT", breaks = breaks)
  
  # plot(xx$geometry)
  # 
  # durations
  # bbox(r)
  # 
  # sgrid
  # isolines <- rasterToContourPoly(r = r, breaks = breaks)
  # 
#   plot(isolines$geometry)
# tail(isolines)
#     # contour correction
  isolines <- isolines[-nrow(isolines),]
#   isolines@data[nrow(isolines), "min"] <- 0
#   isolines@data[nrow(isolines), "center"] <- (isolines@data[nrow(isolines), "max"] -
#                                                 isolines@data[nrow(isolines), "min"]) / 2
  # reproj
  if (!is.na(oprj)){
    isolines <- sf::st_transform(x = isolines, oprj)
  }else{
    isolines <- sf::st_transform(x = isolines, 4326)
  }
  return(isolines)
}





isopoly <- function(x, nclass = 8, breaks, mask, 
                    xcoords = "COORDX", ycoords = "COORDY", 
                    var = "OUTPUT"){
  # x = sgrid
  # xcoords = "COORDX"
  # ycoords = "COORDY"
  # var = "OUTPUT"
  library(isoband)
  library(lwgeom)
  
  # get initial min and max values
  vmin <- min(x[[var]], na.rm = TRUE)
  vmax <- max(x[[var]], na.rm = TRUE)
  
  if(missing(breaks)){
    breaks <- seq(from = vmin, to = vmax, length.out = (nclass+1))
  }else{
    breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
  }
  
  m <- matrix(data = x[[var]], nrow = length(unique(x[[xcoords]])), 
              dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]])))
  
  lev_low = breaks[1:(length(breaks)-1)]
  lev_high = breaks[2:length(breaks)]
  raw <- isobands(x = as.numeric(rownames(m)), 
                  y = as.numeric(colnames(m)), z = t(m), 
                  levels_low = lev_low,
                  levels_high = c(lev_high[-length(lev_high)], vmax + 1e-10))
  
  bands <- iso_to_sfg(raw)
  iso <- st_sf(id = 1:length(bands), 
               min = lev_low, 
               max = lev_high,
               geometry = st_sfc(bands), 
               crs = st_crs(x))
  iso$center = iso$min + (iso$max - iso$min) / 2
  
  
  st_geometry(iso) <- st_make_valid(st_geometry(iso))
  if(methods::is(st_geometry(iso),"sfc_GEOMETRY")){
    st_geometry(iso) <-   st_collection_extract(st_geometry(iso), "POLYGON")
  }
  
  if(!missing(mask)){
    if(is(mask, "Spatial")){mask <- st_as_sf(mask)}
    st_agr(iso) <- "constant"
    iso <- st_cast(st_intersection(x = iso, y = st_union(mask)))
  }
  
  return(iso)
}
