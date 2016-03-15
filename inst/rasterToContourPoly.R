

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL) {
  
  # min and max raster
  rmax <- cellStats(r, max, na.rm = TRUE)
  rmin <- cellStats(r, min, na.rm = TRUE)
  
  # default breaks and nclass
  if(is.null(breaks)){
    levels <- seq(from = rmin, 
                  to = rmax, 
                  length.out = (nclass+1))
  }else{
    levels <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
    levels <- sort(levels)
    nclass <- length(breaks)-1
  }
  levels <- levels[-(nclass+1)]
  
  # zero level problem
  if(rmin == 0){
    levels[1] <-  0.1
  }
  
  # minizone problem
  mval <- sort(getValues(r))[3]
  if(levels[1] < mval){
    levels[1] <- mval
  }  
  levels <- unique(levels)
  
  # raster to SpatialLinesDataFrame contour
  cl <- rasterToContour(r, levels = levels)

  # levels actualy created
  alevels <- cl$level
  ilevels <- 1:nlevels(cl$level)
  dflevels <- data.frame(levelsVal = alevels, levelsIndex = ilevels)
  dflevels$levelsVal <- as.numeric(as.character(dflevels$levelsVal))
  dflevels <- rbind(dflevels, c(ceiling(rmax), nlevels(cl$level)+1))
  cl$level <- ilevels
  
  ## set mask
  mask <- masker(r = r)
  
  ## Create the contour spdf
  bcl <- gBuffer(cl, width = 0.001)
  cp <- gDifference(mask, bcl )
  cp <-  disaggregate(cp)
  df <- data.frame(id = seq_along(cp), min = NA, max = NA, lmin = NA, 
                   lmax = NA, center = NA)
  cp <- SpatialPolygonsDataFrame(cp, df, match.ID = F)
  

  # create a SpatialPointsDataFrame around the contouer zones
  bcl2 <- gBuffer(cl, width = 0.3, byid = TRUE) 
  df <- data.frame(x = numeric(length = 0), 
                   y = numeric(length = 0), 
                   level = numeric(length = 0))
  for (j in 1:length(bcl2)){
    x <- bcl2[j,]
    for (i in 1:length(x@polygons[[1]]@Polygons)){
      df <- rbind(df, data.frame(x@polygons[[1]]@Polygons[[i]]@coords, 
                                 level = x$level))
    }
  }
  pts <- SpatialPointsDataFrame(coords = df[,1:2], 
                                data = df, 
                                proj4string = cl@proj4string)
  
  # over spdf contour with the points
  x <- over(x = cp, y = pts, returnList = T)
  
  # loop over x to get max and min pts values
  for (i in 1:length(cp)){
    v <- x[[i]]$level
    if(length(v)>0){
      cp[i,"min"] <- min(v)
      cp[i,"max"] <- max(v)
    }
  }
  
  
  # ambiguous zones
  cpi <- cp[cp$min == cp$max,]


  rc <- cut(r, 
            breaks = as.numeric(as.character(dflevels$levelsVal)), 
            include.lowest = TRUE, right=TRUE)
  dflevels[1,1] <- rmin

  extr <- extract(x = rc, y = cpi,   df=TRUE)
  x <- aggregate(extr$layer, by = list(extr$ID), max)
  cp@data[cp$id %in% cpi$id,"min"] <- x[, 2]
  cp@data[cp$id %in% cpi$id,"max"] <- x[, 2] + 1
  
  dflevels[nrow(dflevels),1] <- rmax
  cp$lmin <- dflevels[cp$min, 1]
  cp$lmax <- dflevels[cp$max, 1]
  
  
  
  cp$center <- cp$lmin + (cp$lmax-cp$lmin)/2
  cp@data
  
  return(cp)
}  


masker <- function(r){
  
  xy <- coordinates(r)[which(!is.na(values(r))),]
  i <- chull(xy)
  b <- xy[c(i,i[1]),]
  mask <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), 
                                        ID = "1")), 
                          proj4string = CRS(proj4string(r)))
  mask <- gBuffer(spgeom = mask, width = -res(r)[1]/2)
  return(mask)
}