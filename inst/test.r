library(osrm)
library(sp)
library(cartography)

options(osrm.server = "http://router.project-osrm.org/", osrm.profile = "driving")

data("berlin")
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")




#### osrmRoute
route <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                   dst = apotheke.df[16, c("id", "lon","lat")])
route2 <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                    dst = apotheke.df[16, c("id", "lon","lat")], 
                    exclude = "motorway")
plot(route[,1:2], type = "l", lty = 2, asp =1)
points(route2[,1:2], type = "l", lty = 2, asp = 1, col = "red")
points(apotheke.df[c(1,16),2:3], col = "red", pch = 20, cex = 1.5)
text(apotheke.df[c(1,16),2:3], labels = c("A","B"), pos = c(1, 2))
route3 <- osrmRoute(src = c("A", 13.43853, 52.47728),
                    dst = c("B", 13.32247, 52.48882),
                    sp = TRUE, overview = "full")
route4 <- osrmRoute(src = c("A", 13.43853, 52.47728),
                    dst = c("B", 13.32247, 52.48882),
                    sp = TRUE, overview = "full", exclude = "motorway")
plot(route3, lty = 2, asp = 1)
plot(route4, lty = 2, asp = 1, col = "red", add = T)
points(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
       col = "red", pch = 20, cex = 1.5)
text(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
     labels = c("A","B"), pos = c(1, 2))
route5 <- osrmRoute(src = apotheke.sp[1,], dst = apotheke.sp[2,], sp = TRUE)
route5@data




#### osrmTrip
trips <- osrmTrip(loc = apotheke.df)
plot(trips[[1]]$trip, col = "black", lwd = 4)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)
trips_no_motorway <- osrmTrip(loc = apotheke.df, exclude = "motorway")
mapply(`/`, trips_no_motorway[[1]]$summary, trips[[1]]$summary)
plot(trips[[1]]$trip, col = "black", lwd = 3)
plot(trips_no_motorway[[1]]$trip, col = "green", lwd = 3, add = T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)
osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                type = "cartolight", zoom = 11)
tilesLayer(x = osm)
plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)
trips <- osrmTrip(loc = apotheke.sp[1:10,])
osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                type = "cartolight", zoom = 11)
tilesLayer(x = osm)
plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
plot(apotheke.sp[1:10,], pch = 21, bg = "red", cex = 1, add=T)



#### osrmTable
A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("distance", "duration"))
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               measure = c("distance", "duration"))
min(A$distances==B$distances)
min(A$durations==B$durations)
A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("distance"), exclude = "motorway")
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               exclude = "motorway", measure = "distance")
min(A$distances==B$distances)
A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("duration"), exclude = "motorway")
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               exclude = "motorway", measure = "duration")
min(A$durations==B$durations)


#### osrmIsochrone
iso <- osrmIsochrone(loc = c(13.43853,52.47728), breaks = seq(0,15,2), res = 30)
plot(iso, col = colorRampPalette(colors = c('grey80', 'grey20'))(14))
osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
tilesLayer(x = osm)
breaks <- sort(c(unique(iso$min), max(iso$max)))
choroLayer(spdf = iso,
           var = "center", breaks = breaks,
           col = paste0(rev(carto.pal("green.pal",
                                      length(breaks)+1)),"ff"),
           border = NA,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = F)





library(osrm)
# Load data
data("berlin")
# Inputs are data frames
# Travel time matrix
distA <- osrmTable(loc = apotheke.df[1:50, c("id","lon","lat")])
# First 5 rows and columns
distA$durations[1:5,1:5]


route <- osrmRoute(src = c("A", 13.23889, 52.54250),
                   dst = c("B", 13.45363, 52.42926),
                   sp = TRUE, overview = "full")
# Display the path
osm <- getTiles(x = route, crop = TRUE, type = "osm")
tilesLayer(osm)
plot(route, lty = 1,lwd = 4, asp = 1, add=TRUE)
plot(route, lty = 1, lwd = 1, col = "white", add=TRUE)
points(x = c(13.23889, 13.45363), y = c(52.54250,52.42926), 
       col = "red", pch = 20, cex = 1.5)
text(x = c(13.23889, 13.45363), y = c(52.54250,52.42926), 
     labels = c("A","B"), pos = c(1,2))



# Get a trip with a SpatialPointsDataFrame
trips <- osrmTrip(loc = apotheke.sp[10:20,])
# Map
osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                type = "cartolight", zoom = 11)
tilesLayer(x = osm)
plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
plot(apotheke.sp[10:20,], pch = 21, bg = "red", cex = 1.5, add=T)



# Get isochones with a SpatialPointsDataFrame, custom breaks
iso <- osrmIsochrone(loc = apotheke.sp[10,],
                     breaks = seq(from = 0, to = 14, by = 2), res = 50)
# Map
osm <- getTiles(x = iso, crop = FALSE, type = "osm", zoom = 13)
tilesLayer(x = osm)
bks <- sort(c(unique(iso$min), max(iso$max)))
cols <- paste0(carto.pal("turquoise.pal", n1 = length(bks)-1), 80)
choroLayer(spdf = iso,
           var = "center", breaks = bks,
           border = NA, col = cols,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)
plot(apotheke.sp[10,], add=TRUE, col ="red", pch = 20)
