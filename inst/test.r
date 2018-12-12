library(osrm)
library(sp)
library(cartography)
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
iso <- osrmIsochrone(loc = c(13.43853,52.47728), breaks = seq(0,15,1), res = 70)
plot(iso, col = colorRampPalette(colors = c('grey80', 'grey20'))(14))
osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
tilesLayer(x = osm)
breaks <- sort(c(unique(iso$min), max(iso$max)))
choroLayer(spdf = iso,
           var = "center", breaks = breaks,
           col = paste0(rev(carto.pal("green.pal",
                                      length(breaks)+1)),99),
           border = NA,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)
