


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
                  resolution = 50000, 
                  mask = nuts0.spdf)

?rasterStewart

ras <- rasterStewart(poppot)





data(spatData)
# Compute Stewart potentials from known points (spatPts) on a
# grid defined by its resolution
mystewart <- stewart(knownpts = spatPts, varname = "Capacite",
                     typefct = "exponential", span = 1000, beta = 3,
                     resolution = 50, longlat = FALSE, mask = spatMask)
# Create a raster of potentials values
ras <- rasterStewart(x = mystewart)

plot(ras)


# Discretize of the variable

cx <- contourStewart(x=ras, breaks = levels, mask = nuts0.spdf, type = "poly")





raster2contourPolys <- function(r, nclass = 8, breaks = NULL, mask = NULL) {
  
  breaks = NULL
  r <- ras
  nclass = 8
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
  plevels <- c(min(values(r), na.rm=TRUE), levels, max(values(r), na.rm=TRUE)) 
  plevels <- sort(unique(plevels))
  llevels <- 1:length(plevels) 
  
  ## get contour lines and convert to SpatialLinesDataFrame
  clevels <- levels
  if (levels[1]==0) {
    clevels[1] <- 0.0001
  }
  cl <- rasterToContour(r, levels = clevels)
  
  plot(cl)
  plot(mask, add=T)
  
  ## Converting contour lines to polygons
  cp <- contourlines2contourpoly(cl)

  plot(cp)
  final <- rgeos::gIntersection(spgeom1 = cp, spgeom2 = mask, byid = TRUE, 
                                id = row.names(cp))
  
  df <- data.frame(id = sapply(methods::slot(final, "polygons"), 
                               methods::slot, "ID"))
  row.names(df) <- df$id
  final <- sp::SpatialPolygonsDataFrame(Sr = final, data = df)
  
  (final@data)
  plot(final)
  
  contourStewart
  
  cp <- x
  x@proj4string
  b <- rgeos::gUnaryUnion(spgeom = b, id = NULL)
  x <- rgeos::gDifference(b,cp, byid = F )
  x <- sp::SpatialPolygonsDataFrame(Sr = x, 
                                    data = data.frame(levels = min(levels)))
  plot(x)
  aa <- gIntersection(cp, b, byid = T)
  plot(aa)
  aa
  cp
  plot(cp, add=T)
  ## restructure and make polygon number the ID
  polys <- list() 
  for(j in seq_along(cp@polygons[[1]]@Polygons)) {
    polys[[j]] <- Polygons(list(cp@polygons[[1]]@Polygons[[j]]),j)
  }
  cp <- SpatialPolygons(polys)
  cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)))
  
  
  cp@data
  
  ## cut the raster by levels
  rc <- cut(r, breaks=plevels, include.lowest = TRUE)
  ?carto.pal
  plot(rc, col = carto.pal("kaki.pal",8))
  display.carto.all(20)
  levels
  
  ## loop through each polygon, create internal buffer, select points and define overlap with raster
  cat("Adding attributes to polygons...\n")
  l <- character(length(cp))
  plot
  
  
  
  for(j in seq_along(cp)) {
    p <- cp[cp$id==j,] 
    bp <- gBuffer(p, width = -max(res(r)))
    bp <- p
    if(!is.null(bp)) {
      # xy <- SpatialPoints(coordinates(bp@polygons[[1]]@Polygons[[1]]))[1]
      l[j] <- llevels[extract(rc,bp, max, buffer= -max(res(r)))]
    } else { 
      xy <- coordinates(gCentroid(p)) 
      l[j] <- llevels[extract(rc,xy)]
    } 
  }
  
  
  cp$level <- l
  plot(cp, col = cp$level)
  
  
  
  ## assign level to each polygon
  cp$level <- 
    factor(l, levels=llevels)
  cp@min
  cp$min <- plevels[-length(plevels)][cp$level]
  cp$max <- plevels[-1][cp$level]  
  cp <- cp[!is.na(cp$level),] # discard small polygons that did not capture a raster point
  df <- unique(cp@data[,c("level","min","max")]) # to be used after holes are defined
  df <- df[order(df$min),]
  df$mean <- df$min + (df$max-df$min)/2
  row.names(df) <- 1:nrow(df)
  df
  
  llevels <- df$level
  
  plot(rc)
  plot(cp, add=T)
  class(cp)
  library(rgdal)
  writeOGR(cp, dsn = "/home/tg/Bureau", layer = "test", driver = "ESRI Shapefile")
  writeOGR(cx, dsn = "/home/tg/Bureau", layer = "test2", driver = "ESRI Shapefile")
  cp@data
  plot(cp, col = "#920000")
  
  row.names(cp@data)
  cp@plotOrder
  xx <- gUnaryUnion(spgeom = cp, id = as.character(cp$level))
  plot()
  plot(xx[8,], add=F)
  plot(rc, add=T)
  plot(xx[8,], add=T)
  plot(cp, add=T, border = 'blue')
  plot(xx)
  
  summary(cp@data)
  
  cellStats(ras, summary)
  
  class(xx)
  length(xx)
  
  
  x <- gUnion(cp, cp,id = "level")
  gUn
  plot(x)  
  plot(cp)
  x@data
  x <-   gIntersection(cp, cp, byid = T, id = level)
  xx <- gIntersection(cp,cp, byid = T)
  plot(xx)
  
  class(x)
  
  plot(cp)
  ?gIntersection()
  
  cp@data  
  ## define depressions in higher levels (ie holes)
  cat("Defining holes...\n")
  spolys <- list()
  p <- cp[cp$level==llevels[1],] # add deepest layer
  p <- gUnaryUnion(p)
  plot(p)
  spolys[[1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[1])
  
  
  i <- 1
  for(i in seq(length(llevels)-1)) {
    p1 <- cp[cp$level==llevels[i+1],] # upper layer
    p2 <- cp[cp$level==llevels[i],] # lower layer
    plot(p)
    plot(p1, add=T)
    plot(p2, add=T)
    p1@data
    (p2@data)
    x <- numeric(length(p2)) # grab one point from each of the deeper polygons
    y <- numeric(length(p2))
    id <- numeric(length(p2))
    for(j in seq_along(p2)) {
      xy <- coordinates(p2@polygons[[j]]@Polygons[[1]])[1,]
      x[j] <- xy[1]
      y[j] <- xy[2]
      id[j] <- as.numeric(p2@polygons[[j]]@ID)
    }
    plot(p1)
    plot(p2, add=T, col = "#92000010")
    xy <- SpatialPointsDataFrame(cbind(x,y), data.frame(id=id))
    holes <- over(xy, p1)$id
    holes <- xy$id[which(!is.na(holes))]
    if(length(holes) > 0) {
      p2 <- p2[p2$id %in% holes,] # keep the polygons over the shallower polygon
      p1 <- gUnaryUnion(p1) # simplify each group of polygons
      p2 <- gUnaryUnion(p2)
      p <- gDifference(p1, p2) # cut holes in p1      
    } else { p <- gUnaryUnion(p1) }
    plot(p)
    
    spolys[[i+1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[i+1]) # add level 
  }
  plot(cp, border = "green", lty=4, add=T)
  
  length(spolys)
  
  cp <- SpatialPolygons(spolys, pO=seq_along(llevels), 
                        proj4string=CRS(proj4string(r))) # compile into final object
  
  
  cp <- SpatialPolygonsDataFrame(cp, df)
  plot(cx)
  plot(cp, border = "red", lty = 4, add=T)
  
  
  cat("Done!")
  return(cp)
}
plot(cp[8,], col="#92000010")

choroLayer(spdf = cp[2:9,], df = cp@data, var = "mean", breaks = levels)
cp@data
library(rgeos)
?union








