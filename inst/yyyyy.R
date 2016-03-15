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
dmat <- osrmTable(src = pt, dst = spatGrid)
rpt <- SpatialPointsDataFrame(coords = dmat$destination_coordinates[ , c(2, 1)],
                              data = data.frame(dmat$destination_coordinates),
                              proj4string = CRS("+init=epsg:4326"))
rpt <- spTransform(rpt, proj4string(pt))
rpt$d <- as.vector(dmat$distance_table)
gridded(spatGrid) <- TRUE
r <- raster(spatGrid)
ras <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,background= max(rpt$d)+1 )
breaks <- seq(0,120,length.out = 13)



cp <- rasterToContourPoly(r = ras, breaks = breaks)

choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center",
           add=F, border = NA, breaks = sort(unique(c(cp$lmin, cp$lmax))),
           legend.values.rnd = 2)

