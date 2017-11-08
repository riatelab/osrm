#' @name osrmIsochrone
#' @title Get a SpatialPolygonsDataFrame of Isochrones
#' @description Based on \code{\link{osrmTable}}, this function buids a 
#' SpatialPolygonsDataFrame of isochrones. 
#' @param loc a numeric vector of longitude and latitude (WGS84) or a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values (in minutes).
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
#' data("com")
#' 
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(6.026875, 48.93447))
#' plot(iso, col = paste0(rep("grey", nrow(iso)), c(seq(80,20,length.out = nrow(iso)))))
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[1,], breaks = seq(from = 0, to = 40, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(x = iso2, crop = TRUE, type = "osmgrayscale")
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
                          res = 30){
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
    loc <- sp::spTransform(x = loc, CRSobj = sp::CRS("+init=epsg:3857"))
  }
  
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  speed <- 140 * 1000/60
  dmax <- tmax * speed
  sgrid <- rgrid(loc = sp::coordinates(loc), dmax = dmax, res = res)
  
  lsgr <- length(sgrid)
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
  
  
  
  if(f500>0){
    for (i in 1:f500){
      st <- (i-1) * 300 + 1
      en <- i * 300
      dmat <- osrmTable(src = loc, dst = sgrid[st:en,])
      durations <- dmat$durations
      listDur[[i]] <- dmat$durations
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if(r500>0){
      dmat <- osrmTable(src = loc, dst = sgrid[(en+1):(en+r500),])
      listDur[[i+1]] <- dmat$durations
      listDest[[i+1]] <- dmat$destinations
    }
  }else{
    dmat <- osrmTable(src = loc, dst = sgrid)
    listDur[[1]] <- dmat$durations
    listDest[[1]] <- dmat$destinations
  }

  durations <- do.call(c, listDur)
  destinations <- do.call(rbind, listDest)

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
