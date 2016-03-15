library(cartography)
library(SpatialPosition)
library(rgeos)

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
# 
data(spatData)
# Compute Stewart potentials from known points (spatPts) on a
# grid defined by its resolution
mystewart <- stewart(knownpts = spatPts, varname = "Capacite",
                     typefct = "exponential", span = 500, beta = 3,
                     resolution = 50, longlat = FALSE, mask = spatMask)
# Create a raster of potentials values
ras <- rasterStewart(x = mystewart)


plot(ras)
# source('/data/depot/osrm/inst/l2p.R')
# 
# rasterToContourPoly(r = ras)

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL) {
  
  
  r <- ras
  nclass <- 8
  breaks <- NULL
  levels
  mask <- NULL
  
  
  # default breaks and nclass
  if(is.null(breaks)){
    levels <- seq(from = cellStats(r, min, na.rm = TRUE), 
                  to = cellStats(r, max, na.rm = TRUE), 
                  length.out = (nclass+1))
  }else{
    levels <- breaks
    nclass <- length(breaks)-1
  }
  
  ## default mask
  if(is.null(mask)){
    xy <- coordinates(r)[which(!is.na(values(r))),]
    i <- chull(xy)
    b <- xy[c(i,i[1]),]
    mask <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), 
                                          ID = "1")), 
                            proj4string = CRS(proj4string(r)))
  }else{
    mask <- rgeos::gUnaryUnion(spgeom = mask, id = NULL)
  }
  
  ## set-up levels
  levels <- sort(levels)

  rmax <- cellStats(r, max)
  rmin <- cellStats(r, min)
  
  levels <- c(rmin,levels[levels>rmin & levels<rmax])
if(levels[1]==0){
  levels[1] <- 0.0001 
  }
  
  
  cl <- rasterToContour(r, levels = levels)
  plot(cl)
  
  cl@data
  plevels <- 1:nlevels(cl$level)
  cl$level <- plevels
  
  
  bcl <- gBuffer(cl, width = 0.1)
  cp <- gSymdifference(mask, bcl )
  cp <-  disaggregate(cp)
  cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)), match.ID = F)
plot(cp[,])

  
  m <- as(mask, "SpatialLines")
  row.names(m) <- "C_0"
  m <- SpatialLinesDataFrame(sl = m, data = data.frame(level=0, row.names = "C_0")  )
  
  
  # bcl2 <- gBuffer(rbind(m,cl), width = 0.3, byid = TRUE) 
  bcl2 <- gBuffer(cl, width = 0.3, byid = TRUE) 
  df <- data.frame(x = numeric(length = 0), y = numeric(length = 0), level = numeric(length = 0))
  for (j in 1:length(bcl2)){
    x <- bcl2[j,]
    for (i in 1:length(x@polygons[[1]]@Polygons)){
      df <- rbind(df, data.frame(x@polygons[[1]]@Polygons[[i]]@coords, 
                                 level = x$level, stringsAsFactors = FALSE))
    }
  }
  pts <- SpatialPointsDataFrame(coords = df[,1:2], data = df, proj4string = cl@proj4string)
  
plot(cp)
plot(pts, add=T, col = 'red', pch = 20, cex = 0.2)

  x <- over(x = cp, y = pts, returnList = T)
  cp$min <- NA
  cp$max <- NA
  cp$center <- NA
  for (i in 1:length(cp)){
    # cp[i,"min"] 
    v <- unique(x[[i]]$level)
    if(length(v)>0){
      cp[i,"min"] <- min(v)
      cp[i,"max"] <- max(v)
    }
  }
  cp@data
  levels
  
  cl@data
  rc <- cut(r, breaks = c(levels,rmax), include.lowest = TRUE)
  plot(rc, col = carto.pal("wine.pal", nclass+1))

  plot(cp, add=T)
  nclass
  levels
  
    
  cp$lmin <- NA
  cp$lmax <- NA
  
  cp[cp$min == cp$max & cp$max==min(plevels), 'lmax'] <-   cp@data[cp$min == cp$max & cp$max==min(plevels), 'min'] +1
  cp[cp$min == cp$max & cp$max==min(plevels), 'lmin'] <-   cp@data[cp$min == cp$max & cp$max==min(plevels), 'min'] 
  
  cp[cp$min == cp$max & cp$max==max(plevels), 'lmax'] <-   cp@data[cp$min == cp$max & cp$max==max(plevels), 'min'] +2
  cp[cp$min == cp$max & cp$max==max(plevels), 'lmin'] <-   cp@data[cp$min == cp$max & cp$max==max(plevels), 'min'] +1
  
  cp[cp$min != cp$max , 'lmax'] <-   cp@data[cp$min != cp$max , 'max'] +1
  cp[cp$min != cp$max , 'lmin'] <-   cp@data[cp$min != cp$max , 'min'] +1
  
  cp[cp$min == cp$max & cp$max!=min(plevels), 'lmax'] <-   cp@data[cp$min == cp$max & cp$max!=min(plevels), 'min'] +1
  cp[cp$min == cp$max & cp$max!=min(plevels), 'lmin'] <-   cp@data[cp$min == cp$max & cp$max!=min(plevels), 'min'] 
  
  
  levels <- c(0,levels,rmax)
  cp$lmax <- levels[cp$lmax]
  cp$lmin <- levels[cp$lmin]
  
  cp@data
  plot(cp, add=T)
  levels
  
  
  
  
  class(cp)
  plot(r)
  plot(cp, add=T)
  plot(cp[12,], add=T, col = "red")
  
  
  plot(cp, col = "red", add=T)
  plot(cp, col = "#92000050", add=T)
  levels
  cp@data
  levels
  
  plevels
  levels
  
  cp@data
plevels
  levels
  cp[cp$min == cp$max & cp$max==max(plevels), 'max'] <- cp@data[cp$min == cp$max, 'max']+1
cp@data  

plot(cp[cp$min==1 & cp$max==1,], col = "red")
  
levels
cp$center <- cp$lmin + (cp$lmax-cp$lmin)/2
  choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center", 
             breaks = c(levels), add=F)
cp@data
  levels
  levels
    summary(cp$center)
  
  
    plot(r)
  
cp[is.na(cp$min), 'min'] <- 0 
cp[is.na(cp$max), 'max'] <- 1
cp[cp$min == cp$max & cp$min != nclass, 'max'] <-  cp@data[cp$min == cp$max & cp$min != nclass, 'min'] + 1 
cp$center <- cp$min + (cp$max-cp$min)/2

plot(cp[cp$center==1.5,])







dim(cp)
str(pts@data)

plot(cp[2, ])
plot(pts, add=T)
for (i in 1:length(x)){
  print(table(x[[i]]$level))
}
length(x)

x[[1]]
str(x)

levels
plot(cp, col = 1:length(cp))
plot(cp[188,], add=T, border="red", col = "green")

plot(pts, add=T)


length(x)

dim(x)
dim(cp)

x[1:10,1:10]

class(cp)
sp::over(spgeom1 = cp, spgeom2 = pts)
?overGeomGeomDF



plot(x)

plot(cp[2,], col = "green")
points(df)


# reclassify the raster
rc <- cut(r, breaks = levels, include.lowest = TRUE)
rc
plot(rc, col = carto.pal(pal1 = "wine.pal", n1 = 8)
levels
))
# plot(rc)

for (i in 1:nrow(cp)){
  cp[i,"levels"] <- names(which.max(table(extract(rc,cp[i,]), 
                                          useNA = "always")))  
}

cp <- cp[!is.na(cp$levels),]
cp$id <- 1:nrow(cp)
cp$min <- levels[as.numeric(cp$levels)]
cp$max <- levels[as.numeric(cp$levels)+1]
cp$center <- cp$min + (cp$max-cp$min)/2

choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center", 
           breaks = 0:(nclass+0.2), add=F)
}  
