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

library(cartography)
library(SpatialPosition)
data("com")
pt <- src[10,]
tmax <- 120
speed <- 150
dmax <- 2* 100000 * tmax/speed
res <- 50

pt <- sp::spTransform(x = pt, CRSobj =CRS( "+init=epsg:3857"))
spatGrid <- rgrid(pt = pt, dmax = dmax, res = res)

plot(spatGrid)
osm <- getTiles(spatGrid)
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
ras <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE, background= 2147483647)
ras <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE, background= 0)
cellStats(ras, summary)
plot(ras)
breaks <- seq(0,120,length.out = 12)

cellStats(ras, summary)
xx <- rasterToContour(x = ras, levels = breaks)



cellStats(ras, summary)
r <- ras
r[r!=max(rpt$d, na.rm=T)*10] <-  1
r[r!=1] <-  NA

mask1 <- rasterToPolygons(x = r, dissolve = T)
mask <- gBuffer(spgeom = mask1, width = +(5*(res(r)[1])))
nclass <- 12
breaks <- seq(0,120, length.out = (nclass+1))


tilesLayer(osm)
plot(mask1, add=T, border = "grey20")

plot(mask, add=T, border = "blue")

mlines <- contourStewart(x = ras, breaks = breaks, type = "line")
plot(mlines, add=T, col = "red")



