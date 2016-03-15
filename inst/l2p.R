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

r <- rasterStewart(poppot)


mystewart <- stewart(knownpts = spatPts, varname = "Capacite",
                     typefct = "exponential", span = 3000, beta = 3,
                     resolution = 50, longlat = FALSE)
# Create a raster of potentials values
r <- rasterStewart(x = mystewart)
r <- rasterStewart(poppot)
nclass <- 8
breaks <- NULL
mask <- NULL
cellStats(r, min)
plot(r)
cp <- rasterToContourPoly(r = r)

choroLayer(cp, cp@data, spdfid = "id", dfid  = "id",var = "center",
         nclass=8, add=F, border = NA, 
          legend.values.rnd = 2)


plot(  cut(r, 
           breaks =  c(0,2000000,5000000,10000000,15000000), 
           include.lowest = TRUE, right=TRUE))


plot
