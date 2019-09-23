library(osrm)
library(sp)
library(cartography)
options(osrm.server = "http://router.project-osrm.org/", osrm.profile = "driving")
data("berlin")
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")




#### osrmRoute
r1 <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                dst = apotheke.df[16, c("id", "lon","lat")], 
                returnclass = "sf")
r2 <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                dst = apotheke.df[16, c("id", "lon","lat")], 
                exclude = "motorway", 
                returnclass = "sf")
r3 <- osrmRoute(src = c("A", 13.43853, 52.47728),
                dst = c("B", 13.32247, 52.48882), 
                returnclass = "sf")
r4 <- osrmRoute(src = c(13.43853, 52.47728),
                dst = c(13.32247, 52.48882), 
                returnclass = "sf")
r5 <- osrmRoute(src = apotheke.sf[1,],
                dst = apotheke.sf[16,], 
                returnclass = "sf")
r6 <- osrmRoute(src = as(apotheke.sf[1,], "Spatial"),
                dst = as(apotheke.sf[16,], "Spatial"), 
                returnclass = "sf")
r7 <- osrmRoute(loc = apotheke.sf[c(85,18,32,77,3), ], 
                returnclass="sf", exclude = "motorway")
r8 <- osrmRoute(loc = apotheke.df[c(85,18,32,77,3),],
                returnclass = "sf")

plot(r1$geometry)
plot(r2$geometry)
plot(r3$geometry)
plot(r4$geometry)
plot(r5$geometry)
plot(r6$geometry)
plot(r7$geometry)
plot(r8$geometry)


#### osrmTrip
trips             <- osrmTrip(loc = apotheke.df)
trips_no_motorway <- osrmTrip(loc = apotheke.df, exclude = "motorway")
mapply(`/`, trips_no_motorway[[1]]$summary, trips[[1]]$summary)

par(mfrow = c(1,2))
plot(trips[[1]]$trip, col = "black", lwd = 4)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)

plot(trips_no_motorway[[1]]$trip, col = "black", lwd = 4)
plot(trips_no_motorway[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)

par(mfrow = c(1,1))
trips <- osrmTrip(loc = apotheke.sf[1:10,])
plot(trips[[1]]$trip, col = "black", lwd = 4)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
plot(apotheke.sp[1:10,], pch = 21, bg = "red", cex = 1, add=T)



#### osrmTable
A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("distance", "duration"))
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               measure = c("distance", "duration"))
all.equal(A$distance,B$distances)
all.equal(A$durations,B$durations)

A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("distance"), exclude = "motorway")
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               exclude = "motorway", measure = "distance")
all.equal(A$distances, B$distances)

A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("duration"), exclude = "motorway")
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               exclude = "motorway", measure = "duration")
all.equal(A$durations,B$durations)

A <- osrmTable(src = apotheke.sf[1:10, ],
               dst = apotheke.sf[1:10, ], 
               measure = c("distance", "duration"))
B <- osrmTable(loc = apotheke.sf[1:10, ], 
               measure = c("distance", "duration"))
all.equal(A$distance,B$distances)
all.equal(A$durations,B$durations)


#### osrmIsochrone
iso <- osrmIsochrone(loc = c(13.43853,52.47728), breaks = seq(0,15,2), res = 30, 
                     returnclass = "sf")
plot(iso)
iso <- osrmIsochrone(loc = apotheke.sf[20,], breaks = seq(0,30,5), res = 50, 
                     returnclass = "sf")
plot(iso$geometry)
