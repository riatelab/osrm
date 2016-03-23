################## FROM SpatialPosition PACKAGE ################################

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL){
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rgeos' %in% search()){
    attachNamespace('rgeos')
  }
  r <- ra  
  nclass=8
  mask =NULL
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
  
  # ring correction
  df <- unique(final@data[,2:4])
  df$id <- 1:nrow(df)
  df <- df[order(df$center, decreasing = T),]
  
  z <- gIntersection(final[final$center==df[1,3],],final[final$center==df[1,3],], byid = F,
                     id = as.character(df[1,4]))
  for(i in 2:nrow(df)){
    y <- gDifference(final[final$center==df[i,3],],final[final$center==df[i-1,3],], byid = F, 
                     id = as.character(df[i,4]))
    z <- rbind(z, y)
  }
  dfx <- data.frame(id = sapply(slot(z, "polygons"), slot, "ID"))
  row.names(dfx) <- dfx$id
  z <- SpatialPolygonsDataFrame(z, dfx)
  z@data <- df[match(x=z@data$id, table = df$id),c(4,1:3)]
  return(z)
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
rgrid <- function(loc, dmax, res){
  boxCoordX <- seq(from = loc[1] - dmax,
                   to = loc[1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = loc[2] - dmax,
                   to = loc[2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                  data = sgrid,
                                  proj4string = CRS( "+init=epsg:3857"))
  return(sgrid)
}
################################################################################

