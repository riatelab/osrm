# options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")
# osrmRoute tests
library(osrm)
# Load data
data("berlin")

# Travel path between points
route <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                   dst = apotheke.df[16, c("id", "lon","lat")])
# Travel path between points excluding motorways
route2 <- osrmRoute(src = apotheke.df[1, c("id", "lon","lat")],
                    dst = apotheke.df[16, c("id", "lon","lat")], 
                    exclude = "motorway")
# Display paths
plot(route[,1:2], type = "l", lty = 2, asp =1)
points(route2[,1:2], type = "l", lty = 2, asp = 1, col = "red")
points(apotheke.df[c(1,16),2:3], col = "red", pch = 20, cex = 1.5)
text(apotheke.df[c(1,16),2:3], labels = c("A","B"), pos = c(1, 2))


# Travel path between points between points - output a SpatialLinesDataFrame
route3 <- osrmRoute(src = c("A", 13.43853, 52.47728),
                    dst = c("B", 13.32247, 52.48882),
                    sp = TRUE, overview = "full")
# Travel path between points between points - output a SpatialLinesDataFrame 
# excluding motorways
route4 <- osrmRoute(src = c("A", 13.43853, 52.47728),
                    dst = c("B", 13.32247, 52.48882),
                    sp = TRUE, overview = "full", exclude = "motorway")
# Display the path
library(sp)
plot(route3, lty = 2, asp = 1)
plot(route4, lty = 2, asp = 1, col = "red", add = T)
points(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
       col = "red", pch = 20, cex = 1.5)
text(x = c(13.43853,13.32247 ), y = c(52.47728, 52.48882), 
     labels = c("A","B"), pos = c(1, 2))


# Input is SpatialPointsDataFrames
route5 <- osrmRoute(src = apotheke.sp[1,], dst = apotheke.sp[2,], sp = TRUE)
route5@data




# osrmTrip tests
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")


## Not run: 
# Load data
data("berlin")

# Get a trip with a id lat lon data.frame
trips <- osrmTrip(loc = apotheke.df)

# Display the trip
library(sp)
plot(trips[[1]]$trip, col = "black", lwd = 4)
plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)

# Do not route through motorways
trips_no_motorway <- osrmTrip(loc = apotheke.df, exclude = "motorway")

# Looks like it may be convenient to avoid motorways...
mapply(`/`, trips_no_motorway[[1]]$summary, trips[[1]]$summary)

# Display the trips
plot(trips[[1]]$trip, col = "black", lwd = 3)
plot(trips_no_motorway[[1]]$trip, col = "green", lwd = 3, add = T)
points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)

# Map
if(require("cartography")){
  osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                  type = "cartolight", zoom = 11)

    tilesLayer(x = osm)
  plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
  plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
  points(apotheke.df[, 2:3], pch = 21, bg = "red", cex = 1)
}

# Get a trip with a SpatialPointsDataFrame
trips <- osrmTrip(loc = apotheke.sp[1:10,])

# Map
if(require("cartography")){
  osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                  type = "cartolight", zoom = 11)
  tilesLayer(x = osm)
  plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
  plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
  plot(apotheke.sp[1:10,], pch = 21, bg = "red", cex = 1, add=T)
}




library(osrm)
# options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")
data("berlin")

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


A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
               dst = apotheke.df[1:10,c("id","lon","lat")], 
               measure = c("duration"), exclude = "motorway")
B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
               exclude = "motorway", measure = "duration")



min(A$distances==B$distances)
min(A$durations==B$durations)

A$durations
B$durations





A
x <- osrmRoute(apotheke.df[1,c("id","lon","lat")], apotheke.df[11,c("id","lon","lat")], sp = T)
library(sp)
plot(x)
x@data

options(osrm.server = "http://0.0.0.0:5002/", osrm.profile = "driving")


# Inputs are data frames
# Travel time matrix
distB <- osrmTable(loc = apotheke.df[1:50, c("id","lon","lat")])
# First 5 rows and columns
distB$durations[1:5,1:5]


distB$durations==distA$durations









## Not run: 
# Load data
data("berlin")

# Get isochones with lon/lat coordinates, default breaks
iso <- osrmIsochrone(loc = c(13.43853,52.47728), breaks = seq(0,15,1), res = 70)
library(sp)
plot(iso, col = colorRampPalette(colors = c('grey80', 'grey20'))(14))

# Map
if(require("cartography")){
  osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
  tilesLayer(x = osm)
  breaks <- sort(c(unique(iso$min), max(iso$max)))
  cartography::choroLayer(spdf = iso,
                          var = "center", breaks = breaks,
                          col = paste0(rev(carto.pal("green.pal",
                                                     length(breaks)+1)),99),
                          border = NA,
                          legend.pos = "topleft",legend.frame = TRUE,
                          legend.title.txt = "Isochrones\n(min)",
                          add = TRUE)
}

