r = ras
breaks <- seq(0,120,length.out = 13)
mask = NULL
mystewart <- stewart(knownpts = spatPts, varname = "Capacite",
                     typefct = "exponential", span = 3000, beta = 3,
                     resolution = 50, longlat = FALSE, mask = spatMask)

r <- rasterStewart(x = mystewart)

r <- rasterStewart(x = mystewart)

extent(spatMask) > 

  spatMask  
gWithin(r, spatMask)

nclass = 8
breaks = NULL
mask  = NULL

mask = spatMask


plot(r)
plot(spatMask, add=T)

plot(cut(r,c(10, 1000,2000,3000,4000,5000,6500)))

cp <- rasterToContourPoly(r = r, mask  = nuts0.spdf)
, mask = NULL, breaks = c(100, 1000,2000,3000,4000,5000,6500))

plot(cp, add=T)
choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center",
           add=F, border = NA, breaks = sort(unique(c(cp$min, cp$max))),
           legend.values.rnd = 2)

r
summary(r)

plot(r)

r[is.na(r)] <- 0

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL){
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rgeos' %in% search()){
    attachNamespace('rgeos')
  }

  truemin <- cellStats(r, min, na.rm = TRUE)
  rmin <- cellStats(r, min, na.rm = TRUE)
  myres <- res(r)[1]
  
  if (is.null(mask)){
    mask <- masker(r)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- extend(r, maskbuff, value=-1)
  }else{
    
    mask <- gUnaryUnion(mask)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- mask(r, maskbuff, updatevalue = -1)
  }

  if(gWithin(masker(r), mask)){stop("mask should be smaller than r", 
                            call. = FALSE)}

  myproj <- CRS(proj4string(mask))
  
  # rmin <- cellStats(r, min, na.rm = TRUE)
  rmax <- cellStats(r, max, na.rm = TRUE)
  
  # default breaks and nclass
  if(is.null(breaks)){
    breaks <- seq(from = rmin, 
                  to = rmax, 
                  length.out = (nclass+1))
  }else{
    breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
    breaks <- sort(breaks)
    nclass <- length(breaks)-1
  }
  
  
  # zero level problem
  if(rmin <= 0){
    breaks[1] <-  0.1
  }
  

  # minizone problem
  mval <- sort(getValues(r))[3]
  if(breaks[1] < mval){
    breaks[1] <- mval
  }
  
  breaks <- unique(breaks)
  breaks <- breaks[-(nclass+1)]
  
  # test breaks
  if(length(breaks)<2){stop("breaks values do not fit the raster values", 
                            call. = FALSE)}
  
  # build the contour lines
  cl <- rasterToContour(r, levels = breaks)
  cl$level <- as.numeric (as.character(cl$level))
  cl$level[1] <- truemin
  breaks[1] <- truemin
  SPlist <- list()
  SPlevels <- character()
  for (i in cl$level){ 
    linex <- cl[cl@data$level == i,]
    linex <- linex@lines
    linex <- linex[[1]]
    linex <- linex@Lines
    Plist <- NULL
    Plist <- list()
    for (j in 1:length(linex)){
      x <- linex[[j]]@coords
      x <- sp::Polygon(coords =  x, hole = F)
      x <- sp::Polygons(srl = list(x), ID = j)
      Plist[j] <- x
    }  
    x <- sp::SpatialPolygons(Srl = Plist)
    x <- rgeos::union(x = x)
    if (class(x) != "SpatialPolygonsDataFrame"){
      x <- sp::SpatialPolygonsDataFrame(Sr = x, 
                                        data = data.frame(
                                          level = rep(i, length(x))))
    } else {
      x <- x[x@data$count < 2,]
      x@data <- data.frame(level = rep(i, dim(x)[1]))
    }
    SPlist <- c(SPlist , x@polygons  )
    SPlevels <- c(SPlevels,x@data$level)
  }
  for (i in 1:length(SPlist)){
    SPlist[[i]]@ID <- as.character(i)
  }
  x <- sp::SpatialPolygons(Srl = SPlist, proj4string = myproj)
  x <- sp::SpatialPolygonsDataFrame(Sr = x, 
                                    data = data.frame(levels = SPlevels))

  # manage attributes data of the contour spdf
  breaks <- c(breaks, rmax)

  x@data <- data.frame(id = paste("id_",row.names(x),sep=""),
                       min = breaks[match(x$levels, breaks)], 
                       max = breaks[match(x$levels, breaks)+1],
                       center = NA, 
                       stringsAsFactors = FALSE)
  x$center <- (x$min+x$max) / 2 
  row.names(x) <- x$id
  
  # clip the contour spdf with the mask
  final <- rgeos::gIntersection(spgeom1 = x, spgeom2 = mask, byid = TRUE, 
                                id = row.names(x))
  
  df <- data.frame(id = sapply(methods::slot(final, "polygons"), 
                               methods::slot, "ID"))
  row.names(df) <- df$id
  final <- sp::SpatialPolygonsDataFrame(Sr = final, data = df)
  final@data <- data.frame(id = final$id, x[match(final$id, x$id),2:4])
  final@plotOrder <- 1:nrow(final)
  return(final)
}

rmax

# rasterToContourPoly <- function(r, nclass = 8, breaks = NULL) {
#   
#   # min and max raster
#   rmax <- cellStats(r, max, na.rm = TRUE)
#   rmin <- cellStats(r, min, na.rm = TRUE)
#   
#   # default breaks and nclass
#   if(is.null(breaks)){
#     levels <- seq(from = rmin, 
#                   to = rmax, 
#                   length.out = (nclass+1))
#   }else{
#     levels <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
#     levels <- sort(levels)
#     nclass <- length(breaks)-1
#   }
#   levels <- levels[-(nclass+1)]
#   
#   # zero level problem
#   if(rmin == 0){
#     levels[1] <-  0.1
#   }
#   
#   # minizone problem
#   mval <- sort(getValues(r))[3]
#   if(levels[1] < mval){
#     levels[1] <- mval
#   }
#   levels <- unique(levels)
#   
#   
#   # raster to SpatialLinesDataFrame contour
#   cl <- rasterToContour(r, levels = levels)
# 
#   # levels actualy created
#   alevels <- cl$level
#   ilevels <- 1:nlevels(cl$level)
#   dflevels <- data.frame(levelsVal = alevels, levelsIndex = ilevels)
#   dflevels$levelsVal <- as.numeric(as.character(dflevels$levelsVal))
#   dflevels <- rbind(dflevels, c(ceiling(rmax), nlevels(cl$level)+1))
#   cl$level <- ilevels
#   
# 
#   
#   ## set mask
#   mask <- masker(r = r)
# 
#   
#   ## Create the contour spdf
#   bcl <- gBuffer(cl, width = res(r)[1]/1000000, capStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = 3)
# 
#   cp <- gDifference(mask, bcl )
#   cp <-  disaggregate(cp)
#   df <- data.frame(id = seq_along(cp), min = NA, max = NA, lmin = NA, 
#                    lmax = NA, center = NA)
#   cp <- SpatialPolygonsDataFrame(cp, df, match.ID = F)
#   
# 
#   # create a SpatialPointsDataFrame around the contouer zones
#   bcl2 <- gBuffer(cl, width = res(r)[1]/500000, byid = TRUE) 
#   df <- data.frame(x = numeric(length = 0), 
#                    y = numeric(length = 0), 
#                    level = numeric(length = 0))
#   for (j in 1:length(bcl2)){
#     x <- bcl2[j,]
#     for (i in 1:length(x@polygons[[1]]@Polygons)){
#       df <- rbind(df, data.frame(x@polygons[[1]]@Polygons[[i]]@coords, 
#                                  level = x$level))
#     }
#   }
#   pts <- SpatialPointsDataFrame(coords = df[,1:2], 
#                                 data = df, 
#                                 proj4string = cl@proj4string)
#   
#   # over spdf contour with the points
#   x <- over(x = cp, y = pts, returnList = T)
#   
#   # loop over x to get max and min pts values
#   for (i in 1:length(cp)){
#     v <- x[[i]]$level
#     if(length(v) > 0){
#       cp[i,"min"] <- min(v)
#       cp[i,"max"] <- max(v)
#     }
#   }
#   
#   
#   # ambiguous zones
#   cpi <- cp[cp$min == cp$max,]
# 
# 
#   rc <- cut(x = r, 
#             breaks = as.numeric(as.character(dflevels$levelsVal)), 
#             include.lowest = TRUE, 
#             right=TRUE)
# 
#   
#   extr <- extract(x = rc, y = cpi,   df=TRUE)
#   tx <- table(extr,useNA="ifany")
#   x <- apply(tx, 1, function(x) (as.numeric(names(which.max(x)))))
# 
#   cp@data[cp$id %in% cpi$id,"min"] <- x
#   cp@data[cp$id %in% cpi$id,"max"] <- x + 1
#   
#   dflevels[1,1] <- rmin
#   dflevels[1,1] <- rmin
#   dflevels[nrow(dflevels),1] <- rmax
#   cp$lmin <- dflevels[cp$min, 1]
#   cp$lmax <- dflevels[cp$max, 1]
#   
#   cp$center <- cp$lmin + (cp$lmax-cp$lmin)/2
#   
#   # choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center",
#   #            add=F, border = NA, breaks = sort(unique(c(cp$lmin, cp$lmax))),
#   #            legend.values.rnd = 2)
#   return(cp)
# }  


masker <- function(r){
  
  xy <- coordinates(r)[which(!is.na(values(r))),]
  i <- chull(xy)
  b <- xy[c(i,i[1]),]
  mask <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), 
                                        ID = "1")), 
                          proj4string = CRS(proj4string(r)))
  # mask <- gBuffer(spgeom = mask, width = -res(r)[1]/2 - res(r)[1]/1000)
  return(mask)
}





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
r <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,background= max(rpt$d)+1 )
breaks <- seq(0,120,length.out = 13)



cp <- rasterToContourPoly(r = ras)
plot(cp)


choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center",
           add=F, border = NA, breaks = sort(unique(c(cp$lmin, cp$lmax))),
           legend.values.rnd = 2)

