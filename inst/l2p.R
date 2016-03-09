contourlines2contourpoly <- function(cl){
  
  cl$level <- as.numeric (as.character(cl$level))
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
  x <- sp::SpatialPolygons(Srl = SPlist, proj4string = cl@proj4string)
  x <- sp::SpatialPolygonsDataFrame(Sr = x, 
                                    data = data.frame(levels = SPlevels))
  return(x)
}