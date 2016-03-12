# library(cartography)
# library(SpatialPosition)
# library(rgeos)
# 
# data(nuts2006)
# nuts3.spdf@data <- nuts3.df[match(nuts3.spdf$id, nuts3.df$id),]
# poppot <- stewart(knownpts = nuts3.spdf,
#                   varname = "pop2008",
#                   typefct = "exponential",
#                   span = 75000,
#                   beta = 2,
#                   resolution = 45000,
#                   mask = nuts0.spdf)
# 
# ras <- rasterStewart(poppot)
# 
# data(spatData)
# # Compute Stewart potentials from known points (spatPts) on a
# # grid defined by its resolution
# mystewart <- stewart(knownpts = spatPts, varname = "Capacite",
#                      typefct = "exponential", span = 1000, beta = 3,
#                      resolution = 50, longlat = FALSE, mask = spatMask)
# # Create a raster of potentials values
# ras <- rasterStewart(x = mystewart)
# 
# 
# plot(ras)
# source('/data/depot/osrm/inst/l2p.R')
# 
# rasterToContourPoly(r = ras)

rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL) {
  
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
  
  # from rater to contour
  cl <- rasterToContour(r, levels = levels)
  
  bcl <- gBuffer(cl, width = 0.0001) 
  cp <- gDifference(mask, bcl)
  
  ## restructure and make polygon number the ID
  polys <- list() 
  for(j in seq_along(cp@polygons[[1]]@Polygons)) {
    polys[[j]] <- Polygons(list(cp@polygons[[1]]@Polygons[[j]]),j)
  }
  cp <- SpatialPolygons(polys)
  cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)))
  
  # reclassify the raster
  rc <- cut(r, breaks = levels, include.lowest = TRUE)
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
             breaks = levels, add=F, border = NA)
}  
