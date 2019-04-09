options(osrm.server = "http://router.project-osrm.org/", osrm.profile = "driving")


library(osrm)
# Load data
data("berlin")
# Travel time matrix
distA <- osrmTable(loc = apotheke.sf[1:5,])
knitr::kable(distA$durations)
|           | 440338666| 538057637| 977657079| 3770254015| 364363337|
|:----------|---------:|---------:|---------:|----------:|---------:|
|440338666  |       0.0|      24.2|      48.0|       26.9|      15.0|
|538057637  |      27.1|       0.0|      44.2|       16.4|      22.6|
|977657079  |      47.6|      41.7|       0.0|       35.0|      36.2|
|3770254015 |      28.7|      16.9|      34.6|        0.0|      17.0|
|364363337  |      15.4|      20.7|      34.4|       15.9|       0.0|
  
  
  
  
library(osrm)
library(sf)
library(cartography)
# Load data
data("berlin")
route <- osrmRoute(src = apotheke.sf[74,], dst = apotheke.sf[55,],
                   overview = "full", returnclass = "sf")
# Display the path
osm <- getTiles(x = route, crop = TRUE, type = "osm", zoom = 13)

png("img/route.png", width = 500, height = 225, res = 100)
par(mar= c(0,0,0,0))
tilesLayer(osm)
plot(st_geometry(route), lwd = 4, add = TRUE)
plot(st_geometry(route), lwd = 1, col = "white", add = TRUE)
plot(st_geometry(apotheke.sf[c(74,55),]), pch = 20, col = "red", add = TRUE)
dev.off()







library(osrm)
library(sf)
library(cartography)
# Load data
data("berlin")
# Get a trip with a SpatialPointsDataFrame
trips <- osrmTrip(loc = apotheke.sf[10:20,], returnclass="sf")

trip <- trips[[1]]$trip
# Map
osm2 <- getTiles(x = trip, crop = TRUE, type = "cartolight", zoom = 11)
png("img/trip.png", width = 500, height = 480, res = 100)
par(mar= c(0,0,0,0))
tilesLayer(x = osm)
plot(st_geometry(trip), col = "black", lwd = 4, add = TRUE )
plot(st_geometry(trip), col = c("red", "white"), lwd = 1, add=TRUE)
plot(st_geometry(apotheke.sf[10:20,]), pch = 21, bg = "red", cex = 1.5, add=TRUE)
dev.off()










library(osrm)
library(sf)
library(cartography)
# Load data
data("berlin")
iso <- osrmIsochrone(loc = apotheke.sf[87,], returnclass="sf",
                     breaks = seq(from = 0, to = 14, by = 2), res = 50)
osm <- getTiles(x = iso, crop = FALSE, type = "osm", zoom = 12)
png("img/iso.png", width = 500, height = 538, res = 100)
par(mar = c(0,0,0,0))
tilesLayer(x = osm)
bks <- sort(c(unique(iso$min), max(iso$max)))
cols <- paste0(carto.pal("turquoise.pal", n1 = length(bks)-1), 80)
choroLayer(x = iso, var = "center", breaks = bks,
           border = NA, col = cols,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)
plot(st_geometry(apotheke.sf[87,]), pch = 21, bg = "red", cex = 1.5, add=TRUE)
dev.off()