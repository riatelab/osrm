
################## FROM SpatialPosition PACKAGE ################################

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL){
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rgeos' %in% search()){
    attachNamespace('rgeos')
  }
  
  rmin <- raster::cellStats(r, min, na.rm = TRUE)
  rmax <- raster::cellStats(r, max, na.rm = TRUE)
  
  # default breaks and nclass
  if(is.null(breaks)){
    breaks <- seq(from = rmin,
                  to = rmax,
                  length.out = (nclass+1))
  }else{
    breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
    breaks <- unique(breaks)
    breaks <- sort(breaks)
    # nclass <- length(breaks)-1
  }
  
  myres <- raster::res(r)[1]
  myproj <- sp::CRS(sp::proj4string(r))
  
  if (is.null(mask)){
    mask <- masker(r)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::extend(r, maskbuff, value=-1)
  }else{
    mask <- rgeos::gUnaryUnion(mask)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::mask(r, maskbuff, updatevalue = -1)
    if(rgeos::gWithin(masker(r), mask)){stop("mask should be smaller than r",
                                             call. = FALSE)}
  }
  
  rmin <- min(r[r!=-1])
  rmax <- max(r[r!=-1])
  breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
  breaks <- unique(breaks)
  breaks <- sort(breaks)
  finalBreaks <- breaks
  # zero level problem
  if(breaks[1] <= 0){
    zv <- TRUE
    breaks <- breaks + 1
    r <- r + 1
  }else{
    zv <- FALSE
  }
  
  nclass <- length(breaks)-1
  breaks <- breaks[-(nclass+1)]
  
  r[is.na(r)] <- 0
  
  # test breaks
  if(length(breaks)<2){stop("breaks values do not fit the raster values",
                            call. = FALSE)}
  # build the contour lines
  cl <- rasterToContour(r, levels = breaks)
  cl$level <- as.numeric(as.character(cl$level))
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
  
  bks <- data.frame(b =c(breaks, rmax), t = finalBreaks)
  
  # # manage attributes data of the contour spdf
  # breaks <- c(breaks, rmax)
  x@data <- data.frame(id = paste("id_",row.names(x),sep=""),
                       min = bks[match(x$levels, bks[,1]),2],
                       max = bks[match(x$levels, bks[,1])+1,2],
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
  
  final@data
  return(final)
}


masker <- function(r){
  xy <- sp::coordinates(r)[which(!is.na(values(r))),]
  i <- grDevices::chull(xy)
  b <- xy[c(i,i[1]),]
  mask <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(b,
                                                                 hole = FALSE)),
                                                ID = "1")),
                              proj4string = sp::CRS(sp::proj4string(r)))
  return(mask)
}

################################################################################


################# osrm isolines stuff ##########################################
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
library(rgeos)
data("com")
pt <- src[5,]
tmax <- 240
speed <- 150
dmax <-  3 * 100000 * tmax/speed
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
library(raster)
r <- raster(spatGrid)
r <- rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,
               background= max(rpt$d)+1 )
breaks <- seq(0,240,length.out = 13)
cp <- rasterToContourPoly(r = r,nclass = 8)
################################################################################


################ End the rasterToContourPoly story #############################
library(SpatialPosition)

data("spatData")
# Compute a SpatialPolygonsDataFrame of potentials
cp <- quickStewart(spdf = spatPts, 
                   df = spatPts@data, 
                   var = "Capacite", 
                   span = 1000, 
                   beta = 3, mask = spatMask)
plot(cp, add=F)
df <- unique(cp@data[,2:4])
df$id <- 1:nrow(df)
df <- df[order(df$center, decreasing = T),]
z <- gIntersection(cp[cp$center==df[1,3],],cp[cp$center==df[1,3],], byid = F,
                   id = as.character(df[1,4]))
for(i in 2:nrow(df)){
  y <- gDifference(cp[cp$center==df[i,3],],cp[cp$center==df[i-1,3],], byid = F, 
                   id = as.character(df[i,4]))
  z <- rbind(z, y)
}
dfx <- data.frame(id = sapply(slot(z, "polygons"), slot, "ID"))
row.names(dfx) <- dfx$id
z <- SpatialPolygonsDataFrame(z, dfx)
plot(z, col = 10:18)

################################################################################

plot(r)
plot(z, col = "#92005050", add=T)

choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center", 
           add=T, border = NA, breaks = sort(unique(c(cp$min, cp$max))),
           legend.values.rnd = 2)

