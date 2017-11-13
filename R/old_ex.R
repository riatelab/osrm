#' @title Communes Coordinates
#' @name com
#' @description Coordinates of a set of communes in France. Coordinates are in WGS84.
#' @source UMS RIATE
#' @docType data
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' ### osrmTable ###
#' # Inputs are data frames  
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50, c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,c("comm_id","lon","lat")],
#'                       dst = com[11:20,c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' # Inputs are SpatialPointsDataFrames
#' distCom <- osrmTable(loc = src)
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = src, dst = dst)
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' 
#' ### osrmRoute ###
#' # Travel path between points
#' route <- osrmRoute(src = com[1, c("comm_id", "lon","lat")],
#'                    dst = com[15, c("comm_id", "lon","lat")])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(route[,1:2], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' route2 <- osrmRoute(src=c("Bethune", 2.64781, 50.5199),
#'                     dst = c("Renescure", 2.369521, 50.72761),
#'                     sp = TRUE, overview = "full")
#' 
#' # Display the path
#' plot(com[c(1,4),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(route2, lty = 1,lwd = 4, add = TRUE)
#' plot(route2, lty = 1, lwd = 1, col = "white", add=TRUE)
#' text(com[c(1,4),3:4], labels = com[c(1,4),2], pos = 2)
#' 
#' # Input is SpatialPointsDataFrames
#' route3 <- osrmRoute(src = src[1,], dst = dst[1,], sp = TRUE)
#' route3@data
#' 
#' 
#' 
#' ### osrmTrip ###
#' # Get a trip with a id lat lon data.frame
#' trips <- osrmTrip(loc = com[1:9, c(1,3,4)])
#' 
#' # Display the trip
#' plot(trips[[1]]$trip , col = 1:5)
#' points(com[1:10, 3:4], pch = 20, col = "red", cex = 0.5)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE)
#'   points(com[1:9, 3:4], pch = 20, col = "red", cex = 2)
#' }
#' 
#' # Get a trip with a SpatialPointsDataFrame
#' trips <- osrmTrip(loc = src)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE, lwd=2)
#' }
#' 
#' 
#' ### osrmIsochrone
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(6.026875, 48.93447))
#' plot(iso, col = paste0(rep("grey", nrow(iso)), c(seq(80,20,length.out = nrow(iso)))))
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[1,], breaks = seq(from = 0, to = 40, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(x = iso2, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' }
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France
#' @name src
#' @description 8 communes in France. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' ### osrmTable ###
#' # Inputs are data frames  
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50, c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,c("comm_id","lon","lat")],
#'                       dst = com[11:20,c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' # Inputs are SpatialPointsDataFrames
#' distCom <- osrmTable(loc = src)
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = src, dst = dst)
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' 
#' ### osrmRoute ###
#' # Travel path between points
#' route <- osrmRoute(src = com[1, c("comm_id", "lon","lat")],
#'                    dst = com[15, c("comm_id", "lon","lat")])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(route[,1:2], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' route2 <- osrmRoute(src=c("Bethune", 2.64781, 50.5199),
#'                     dst = c("Renescure", 2.369521, 50.72761),
#'                     sp = TRUE, overview = "full")
#' 
#' # Display the path
#' plot(com[c(1,4),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(route2, lty = 1,lwd = 4, add = TRUE)
#' plot(route2, lty = 1, lwd = 1, col = "white", add=TRUE)
#' text(com[c(1,4),3:4], labels = com[c(1,4),2], pos = 2)
#' 
#' # Input is SpatialPointsDataFrames
#' route3 <- osrmRoute(src = src[1,], dst = dst[1,], sp = TRUE)
#' route3@data
#' 
#' 
#' 
#' ### osrmTrip ###
#' # Get a trip with a id lat lon data.frame
#' trips <- osrmTrip(loc = com[1:9, c(1,3,4)])
#' 
#' # Display the trip
#' plot(trips[[1]]$trip , col = 1:5)
#' points(com[1:10, 3:4], pch = 20, col = "red", cex = 0.5)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE)
#'   points(com[1:9, 3:4], pch = 20, col = "red", cex = 2)
#' }
#' 
#' # Get a trip with a SpatialPointsDataFrame
#' trips <- osrmTrip(loc = src)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE, lwd=2)
#' }
#' 
#' 
#' ### osrmIsochrone
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(6.026875, 48.93447))
#' plot(iso, col = paste0(rep("grey", nrow(iso)), c(seq(80,20,length.out = nrow(iso)))))
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[1,], breaks = seq(from = 0, to = 40, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(x = iso2, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' }
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France
#' @name dst
#' @description 10 communes in France. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' ### osrmTable ###
#' # Inputs are data frames  
#' # Travel time matrix
#' distCom <- osrmTable(loc = com[1:50, c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = com[1:10,c("comm_id","lon","lat")],
#'                       dst = com[11:20,c("comm_id","lon","lat")])
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' # Inputs are SpatialPointsDataFrames
#' distCom <- osrmTable(loc = src)
#' # First 5 rows and columns
#' distCom$durations[1:5,1:5]
#' 
#' # Travel time matrix with different sets of origins and destinations
#' distCom2 <- osrmTable(src = src, dst = dst)
#' # First 5 rows and columns
#' distCom2$durations[1:5,1:5]
#' 
#' 
#' ### osrmRoute ###
#' # Travel path between points
#' route <- osrmRoute(src = com[1, c("comm_id", "lon","lat")],
#'                    dst = com[15, c("comm_id", "lon","lat")])
#' # Display the path
#' plot(com[c(1,15),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' points(route[,1:2], type = "l", lty = 2)
#' text(com[c(1,15),3:4], labels = com[c(1,15),2], pos = 2)
#' 
#' # Travel path between points - output a SpatialLinesDataFrame
#' route2 <- osrmRoute(src=c("Bethune", 2.64781, 50.5199),
#'                     dst = c("Renescure", 2.369521, 50.72761),
#'                     sp = TRUE, overview = "full")
#' 
#' # Display the path
#' plot(com[c(1,4),3:4], asp =1, col = "red", pch = 20, cex = 1.5)
#' plot(route2, lty = 1,lwd = 4, add = TRUE)
#' plot(route2, lty = 1, lwd = 1, col = "white", add=TRUE)
#' text(com[c(1,4),3:4], labels = com[c(1,4),2], pos = 2)
#' 
#' # Input is SpatialPointsDataFrames
#' route3 <- osrmRoute(src = src[1,], dst = dst[1,], sp = TRUE)
#' route3@data
#' 
#' 
#' 
#' ### osrmTrip ###
#' # Get a trip with a id lat lon data.frame
#' trips <- osrmTrip(loc = com[1:9, c(1,3,4)])
#' 
#' # Display the trip
#' plot(trips[[1]]$trip , col = 1:5)
#' points(com[1:10, 3:4], pch = 20, col = "red", cex = 0.5)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE)
#'   points(com[1:9, 3:4], pch = 20, col = "red", cex = 2)
#' }
#' 
#' # Get a trip with a SpatialPointsDataFrame
#' trips <- osrmTrip(loc = src)
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = trips[[1]]$trip, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
#'   plot(trips[[1]]$trip, col = 1:5, add = TRUE, lwd=2)
#' }
#' 
#' 
#' ### osrmIsochrone
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(6.026875, 48.93447))
#' plot(iso, col = paste0(rep("grey", nrow(iso)), c(seq(80,20,length.out = nrow(iso)))))
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(x = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[1,], breaks = seq(from = 0, to = 40, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(x = iso2, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(x = osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' }
NULL