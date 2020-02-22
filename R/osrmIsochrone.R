#' @name osrmIsochrone
#' @title Get Polygons of Isochrones
#' @description Based on \code{\link{osrmTable}}, this function buids polygons 
#'  of isochrones. 
#' @param loc a numeric vector of longitude and latitude (WGS84), an sf object, 
#' a SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values (in minutes).
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param res number of points used to compute isochrones, one side of the square 
#' grid, the total number of points will be res*res.  
#' @param returnclass class of the returned polygons. Either "sp" of "sf".
#' @return A SpatialPolygonsDateFrame or an sf MULTIPOLYGON of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @seealso \link{osrmTable}
#' @importFrom sf st_as_sf st_crs st_transform st_convex_hull st_union st_intersects
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' library(sf)
#' data("berlin")
#' # Get isochones with lon/lat coordinates
#' iso <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), 
#'                      returnclass="sf")
#' plot(st_geometry(iso), col = c('grey80','grey60','grey50',
#'                                'grey40','grey30','grey20'))
#' # Map
#' if(require("cartography")){
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(x = iso,
#'                           var = "center", breaks = breaks,
#'                           col = rev(carto.pal("green.pal",6)),
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE,
#'                           legend.title.txt = "Isochrones\n(min)")
#' }
#' 
#' # Get isochones with an sf POINT
#' iso2 <- osrmIsochrone(loc = apotheke.sf[10,], returnclass="sf",
#'                       breaks = seq(from = 0, to = 16, by = 2))
#' # Map
#' if(require("cartography")){
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(x = iso2, var = "center", 
#'                           breaks = breaks2, border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE,
#'                           legend.title.txt = "Isochrones\n(min)")
#' }
#' }
osrmIsochrone <- function(loc, breaks = seq(from = 0,to = 60, length.out = 7), 
                          exclude = NULL, res = 30, returnclass = "sp"){
  # imput mngmnt
  oprj <- NA
  if (methods::is(loc,"Spatial")){
    loc <- st_as_sf(loc[1,])
  }    
  if(testSf(loc)){
    oprj <- st_crs(loc)
    loc <- loc[1,]
  }else{
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- st_as_sf(loc, coords=c("lon","lat"), crs = 4326)
  }
  loc <- st_transform(loc, 3857)
  row.names(loc) <- "0"
  
  # max distance mngmnt to see how far to extend the grid to get measures
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if(options('osrm.profile')=="walk"){
    speed =  10 * 1000/60
  }
  if(options('osrm.profile')=="bike"){
    speed =  20 * 1000/60
  }
  if(options('osrm.profile')=="driving"){
    speed =  100 * 1000/60
  }
  dmax <- tmax * speed
  
  # create a grid to obtain measures
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  
  # slice the grid to make several API calls  
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% 300
  r500 <- lsgr %% 300
  listDur <- list()
  listDest <- list()
  # gentle sleeptime for demo server
  if(getOption("osrm.server") != "http://router.project-osrm.org/"){
    sleeptime <- 0
  }else{
    sleeptime <- 1
  }
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
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),], 
                        exclude = exclude)
      listDur[[i+1]] <- dmat$durations
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude)
    listDur[[1]] <- dmat$durations
    listDest[[1]] <- dmat$destinations
  }
  durations <- do.call(c, listDur)
  
  # mgmnt of edge cases of points out of reach
  ########### QUICK FIX ######################
  destinations <- do.call(rbind, listDest)
  rpt <- st_as_sf(destinations, coords = c('lon', 'lat'), crs = 4326)
  rpt <- st_transform(rpt, st_crs(loc))
  rpt$durations <- durations
  b <- as.numeric(st_distance(sgrid[1,], sgrid[2,]) / 2)
  xx <- st_make_grid(x = st_buffer(st_union(sgrid), b), n = c(res, res))
  inter <- st_intersects(xx, rpt)
  sgrid$durations <- unlist(lapply(inter, function(x)mean(rpt[["durations"]][x], na.rm=TRUE)))
  sgrid[is.nan(sgrid$durations), "durations"] <- tmax + 1
  sgrid[sgrid$durations > tmax, "durations"] <- tmax + 1
  if(min(sgrid$durations) > tmax){
    e <- "Use lower values for 'breaks' or increase 'res'"
    stop(e, call. = FALSE)
  }
  ########### END OF QUICK FIX ################
  
  # computes isopolygones
  iso <- isopoly(x = sgrid, breaks = breaks, var = "durations")
  
  # proj mgmnt
  if (!is.na(oprj)){
    iso <- st_transform(x = iso, oprj)
  }else{
    iso <- st_transform(x = iso, 4326)
  }
  
  # output mgmnt
  if(returnclass=="sp"){
    iso <- methods::as(iso, "Spatial")
  }
  
  return(iso)
}

#' @export
osrmIsometric <- function(loc, breaks = seq(from = 0, to = 10000, length.out = 4), 
                          exclude = NULL, res = 30, returnclass = "sp", dmax_multiplier){
  # imput mngmnt
  oprj <- NA
  if (methods::is(loc,"Spatial")){
    loc <- st_as_sf(loc[1,])
  }    
  if(testSf(loc)){
    oprj <- st_crs(loc)
    loc <- loc[1,]
  }else{
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- st_as_sf(loc, coords=c("lon","lat"), crs = 4326)
  }
  loc <- st_transform(loc, 3857)
  row.names(loc) <- "0"
  
  # max distance mngmnt to see how far to extend the grid to get measures
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if(options('osrm.profile')=="walk"){
    speed =  10 * 1000/60
  }
  if(options('osrm.profile')=="bike"){
    speed =  20 * 1000/60
  }
  if(options('osrm.profile')=="driving"){
    speed =  100 * 1000/60
  }
  dmax <- tmax * speed * dmax_multiplier
  
  # create a grid to obtain measures
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  
  # slice the grid to make several API calls  
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% 300
  r500 <- lsgr %% 300
  listDur <- list()
  listDest <- list()
  # gentle sleeptime for demo server
  if(getOption("osrm.server") != "http://router.project-osrm.org/"){
    sleeptime <- 0
  }else{
    sleeptime <- 1
  }
  if(f500>0){
    for (i in 1:f500){
      st <- (i-1) * 300 + 1
      en <- i * 300
      dmat <- osrmTable(src = loc, dst = sgrid[st:en,], exclude = exclude, measure = "distance")
      distances <- dmat$distances
      listDur[[i]] <- dmat$distances
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if(r500>0){
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),], 
                        exclude = exclude, measure = "distance")
      listDur[[i+1]] <- dmat$distances
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude, measure = "distance")
    listDur[[1]] <- dmat$distances
    listDest[[1]] <- dmat$destinations
  }
  distances <- do.call(c, listDur)
  
  # mgmnt of edge cases of points out of reach
  ########### QUICK FIX ######################
  destinations <- do.call(rbind, listDest)
  rpt <- st_as_sf(destinations, coords = c('lon', 'lat'), crs = 4326)
  rpt <- st_transform(rpt, st_crs(loc))
  rpt$distances <- distances
  b <- as.numeric(st_distance(sgrid[1,], sgrid[2,]) / 2)
  xx <- st_make_grid(x = st_buffer(st_union(sgrid), b), n = c(res, res))
  inter <- st_intersects(xx, rpt)
  sgrid$distances <- unlist(lapply(inter, function(x)mean(rpt[["distances"]][x], na.rm=TRUE)))
  sgrid[is.nan(sgrid$distances), "distances"] <- tmax + 1
  sgrid[sgrid$distances > tmax, "distances"] <- tmax + 1
  if(min(sgrid$distances) > tmax){
    e <- "Use lower values for 'breaks' or increase 'res'"
    stop(e, call. = FALSE)
  }
  ########### END OF QUICK FIX ################
  
  # computes isopolygones
  iso <- isopoly(x = sgrid, breaks = breaks, var = "distances")
  
  # proj mgmnt
  if (!is.na(oprj)){
    iso <- st_transform(x = iso, oprj)
  }else{
    iso <- st_transform(x = iso, 4326)
  }
  
  # output mgmnt
  if(returnclass=="sp"){
    iso <- methods::as(iso, "Spatial")
  }
  
  return(iso)
}

