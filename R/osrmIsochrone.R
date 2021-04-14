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
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @return A SpatialPolygonsDateFrame or an sf MULTIPOLYGON of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @seealso \link{osrmTable}
#' @importFrom sf st_as_sf st_crs st_transform st_convex_hull st_union st_intersects st_bbox
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
#' if(require("mapsf")){
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   mapsf::mf_map(x = iso, var = "center", type = "choro", 
#'                 breaks = breaks, pal = "Greens",
#'                 border = NA, leg_pos = "topleft",
#'                 leg_frame = TRUE, leg_title = "Isochrones\n(min)")
#' }
#' 
#' # Get isochones with an sf POINT
#' iso2 <- osrmIsochrone(loc = apotheke.sf[10,], returnclass="sf",
#'                       breaks = seq(from = 0, to = 16, by = 2))
#' # Map
#' if(require("mapsf")){
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   mapsf::mf_map(x = iso2, var = "center", type = "choro", 
#'                 breaks = breaks2, pal = "Blues",
#'                 border = NA, leg_pos = "topleft", leg_val_rnd = 0,
#'                 leg_frame = TRUE, leg_title = "Isochrones\n(min)")
#' }
#' }
osrmIsochrone <- function(loc, breaks = seq(from = 0,to = 60, length.out = 7), 
                          exclude = NULL, res = 30, returnclass = "sp",  
                          osrm.server = getOption("osrm.server"),
                          osrm.profile = getOption("osrm.profile")){
  
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
  if(osrm.profile %in% c("foot", "walk")){
    speed =  10 * 1000/60
  }
  if(osrm.profile =="bike"){
    speed =  20 * 1000/60
  }
  if(osrm.profile %in% c("driving","car")){
    speed =  130 * 1000/60
  }
  dmax <- tmax * speed
  
  # create a grid to obtain measures
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  
  # gentle sleeptime & param for demo server
  if(osrm.server != "https://routing.openstreetmap.de/"){
    sleeptime <- 0
    deco <- 300
  }else{
    sleeptime <- 1
    deco <- 75
    osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
    osrm.profile = "driving"
  }
  

  # slice the grid to make several API calls  
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% deco
  r500 <- lsgr %% deco
  listDur <- list()
  listDest <- list()

  if(f500>0){
    for (i in 1:f500){
      st <- (i-1) * deco + 1
      en <- i * deco
      dmat <- osrmTable(src = loc, dst = sgrid[st:en,], exclude = exclude, 
                        osrm.server = osrm.server, osrm.profile = osrm.profile)
      durations <- dmat$durations
      listDur[[i]] <- dmat$durations
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if(r500>0){
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),], 
                        exclude = exclude, osrm.server = osrm.server, 
                        osrm.profile = osrm.profile)
      listDur[[i+1]] <- dmat$durations
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude,
                      osrm.server = osrm.server, osrm.profile = osrm.profile)
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
  xx <- st_make_grid(x = st_buffer(st_as_sfc(st_bbox(sgrid)), b), n = c(res, res))
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

#' @name osrmIsometric
#' @title Get Polygons of Isodistances
#' @description Based on \code{\link{osrmTable}}, this function buids polygons 
#'  of isometric road distances. 
#' @param loc a numeric vector of longitude and latitude (WGS84), an sf object, 
#' a SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isometric values (in meters).
#' @param exclude pass an optional "exclude" request option to the OSRM API. 
#' @param res number of points used to compute isochrones, one side of the square 
#' grid, the total number of points will be res*res.  
#' @param returnclass class of the returned polygons. Either "sp" of "sf".
#' @param osrm.server the base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile the routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @return A SpatialPolygonsDateFrame or an sf MULTIPOLYGON of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @seealso \link{osrmTable}
#' @importFrom sf st_as_sf st_crs st_transform st_convex_hull st_union st_intersects
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' data("berlin")
#' # Get isochones with lon/lat coordinates
#' iso <- osrmIsometric(loc = c(13.43,52.47), breaks = c(0,100,200, 500, 1000),
#'                      returnclass="sf")
#' plot(st_geometry(iso))
#' }
osrmIsometric <- function(loc, breaks = seq(from = 0, to = 10000, length.out = 4), 
                          exclude = NULL, res = 30, returnclass = "sp", 
                          osrm.server = getOption("osrm.server"),
                          osrm.profile = getOption("osrm.profile")){
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
  if(osrm.profile %in% c("foot", "walk")){
    speed =  10 * 1000/60
  }
  if(osrm.profile =="bike"){
    speed =  20 * 1000/60
  }
  if(osrm.profile %in% c("driving","car")){
    speed =  100 * 1000/60
  }
  # 2.2 seems to be a reasonable multiplier to max distance given
  dmax <- 2.2 * max(breaks)
  
  # create a grid to obtain measures
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  
  # slice the grid to make several API calls  
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% 150
  r500 <- lsgr %% 150
  listDur <- list()
  listDest <- list()
  # gentle sleeptime for demo server
  if(osrm.server != "https://routing.openstreetmap.de/"){
    sleeptime <- 0
  }else{
    sleeptime <- 1
    osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
    osrm.profile = "driving"
  }
  if(f500>0){
    for (i in 1:f500){
      st <- (i-1) * 150 + 1
      en <- i * 150
      dmat <- osrmTable(src = loc, dst = sgrid[st:en,], exclude = exclude,
                        measure = "distance", 
                        osrm.server = osrm.server, osrm.profile = osrm.profile)
      distances <- dmat$distances
      listDur[[i]] <- dmat$distances
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if(r500>0){
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),], 
                        exclude = exclude, measure = "distance" ,
                        osrm.server = osrm.server, osrm.profile = osrm.profile)
      listDur[[i+1]] <- dmat$distances
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude, 
                      measure = "distance" ,
                      osrm.server = osrm.server, osrm.profile = osrm.profile)
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
  xx <- st_make_grid(x = st_buffer(st_as_sfc(st_bbox(sgrid)), b), n = c(res, res))
  inter <- st_intersects(xx, rpt)
  sgrid$distances <- unlist(lapply(inter, function(x)mean(rpt[["distances"]][x], na.rm=TRUE)))
  sgrid[is.na(sgrid$distances), "distances"] <- tmax + 1
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

