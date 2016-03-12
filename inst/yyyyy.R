rgrid <- function(pt, dmax, res){
  ptc <- coordinates(pt)
  boxCoordX <- seq(from = ptc[1] - dmax,
                   to = ptc[1] + dmax, 
                   length.out = res)
  boxCoordY <- seq(from = ptc[2] - dmax,
                   to = ptc[2] + dmax, 
                   length.out = res)
  spatGrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(spatGrid), 1)
  spatGrid <- data.frame(ID = idSeq, 
                         COORDX = spatGrid[, 1], 
                         COORDY = spatGrid[, 2])
  spatGrid <- SpatialPointsDataFrame(coords = spatGrid[ , c(2, 3)], 
                                     data = spatGrid, 
                                     proj4string = CRS(proj4string(pt)))
  return(spatGrid)
}
masker <- function(x){

  bb <- bbox(x)
  mm <- matrix(data = c(bb[1,1], bb[2,1],
                        bb[1,2], bb[2,1],
                        bb[1,2], bb[2,2],
                        bb[1,1], bb[2,2],
                        bb[1,1], bb[2,1]),
               nrow = 5, byrow = T)
  mask <- SpatialPolygons(
    Srl = list(Polygons(
      srl = list(Polygon(coords = mm, hole = FALSE)), ID = "id")),
    proj4string = CRS(proj4string(x)))
  return(mask)
}
rasterer <- function(x){
  gridded(x) <- TRUE
  r <- raster(x)
  r <- rasterize(x, r, field = 'd')
  return(r)
}
library(osrm)
library(cartography)
library(SpatialPosition)
library(rgeos)
data("com")
pt <- src[5,]
tmax <- 120
speed <- 150
dmax <-  4 * 100000 * tmax/speed
res <- 30

pt <- sp::spTransform(x = pt, CRSobj =CRS( "+init=epsg:3857"))
spatGrid <- rgrid(pt = pt, dmax = dmax, res = res)
osm <- getTiles(spatGrid)
par(mar=c(0,0,0,0))
tilesLayer(x = osm, add=F)
plot(pt, add=T)
mask <- masker(x = spatGrid)
plot(mask, add=T, border = "red")
dmat <- osrmTable(src = pt, dst = spatGrid)

rpt <- SpatialPointsDataFrame(coords = dmat$destination_coordinates[ , c(2, 1)],
                              data = data.frame(dmat$destination_coordinates),
                              proj4string = CRS("+init=epsg:4326"))
rpt <- spTransform(rpt, proj4string(pt))
plot(spatGrid, add=T, cex = 0.2)
plot(rpt, add=T, cex = 0.2, col = 'red')
rpt$d <- as.vector(dmat$distance_table)
gridded(spatGrid) <- TRUE
r <- raster(spatGrid)
ras <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,background= max(rpt$d)+1 )
plot(ras)

?rasterize
head(rpt@data)


breaks <- seq(0,120,length.out = 13)

rasterToContourPoly(r = ras, breaks = breaks)



data(nuts2006)
nuts3.spdf@data <- nuts3.df[match(nuts3.spdf$id, nuts3.df$id),]
poppot <- stewart(knownpts = nuts3.spdf,
                  varname = "pop2008",
                  typefct = "exponential",
                  span = 75000,
                  beta = 2,
                  resolution = 45000,
                  mask = nuts0.spdf)

ras <- rasterStewart(poppot)
rasterToContourPoly(r = ras)
