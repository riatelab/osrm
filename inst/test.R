communes_lau2_osm <- read.csv("/mnt/data/depot/osrm/inst/communes_lau2_osm.csv", stringsAsFactors=FALSE)
com <- communes_lau2_osm[1:50, c(1:2,7:8)]
x2 <- osrmTable(com, id = "comm_id", x =  "lon",y =  "lat")

x <- osrmViaroute(com[1,"lon"],com[1,"lat"],com[2,"lon"],com[2,"lat"] )



library(jsonlite)
myst <- "{\"hint_data\":{\"locations\":[\"7xREAoX4sgLRMBoAPgAAAHEAAAAAAAAAlwAAALhHOgIAAAAAtd4CA_JhKAAAABEA\",\"sf7jArT-4wLg3x0AAAAAAD4AAAAAAAAAAAAAAP____8AAAAAvDMDA031JwAAABEA\"],\"checksum\":1472105679},\"route_name\":[\"\",\"\"],\"via_indices\":[0,142],\"via_points\":[[50.519733,2.646514],[50.5415,2.618701]],\"found_alternative\":false,\"route_summary\":{\"end_point\":\"Allée des Arpents Verts\",\"start_point\":\"Rue de l'Horlogerie\",\"total_time\":321,\"total_distance\":4142},\"status_message\":\"Found route between points\",\"status\":0}"
y <- jsonlite::fromJSON(myst)

str(y)
y/600
x
matrix(y, nrow = 5)
class(y)
x*600




x <- osrmTableOD(dfo = com, ido = "comm_id",xo =  "lon",yo =  "lat",
            dfd = com, idd = "comm_id", xd = "lon",yd =  "lat",
            limit = 2000)


dfo = com
ido = "comm_id"
xo =  "lon"
yo =  "lat"
dfd = com
idd = "comm_id"
xd = "lon"
yd =  "lat"
limit = 10000000
communes_lau2_osm <- read.csv("/mnt/data/depot/osrm/inst/communes_lau2_osm.csv", stringsAsFactors=FALSE)
com <- communes_lau2_osm[1:2000, c(1:2,7:8)]

xOD1 <- osrmTableOD(dfo = com, ido = "comm_id",xo =  "lon",yo =  "lat",
                  dfd = com, idd = "comm_id", xd = "lon",yd =  "lat",
                  limit = 1000000)

xOD2 <- osrmTableOD(dfo = com, ido = "comm_id",xo =  "lon",yo =  "lat",
                    dfd = com, idd = "comm_id", xd = "lon",yd =  "lat",
                    limit = 100)



# xOD <- testF(com, id = "comm_id", x =  "lon",y =  "lat")




.ls.objects()


identical(xOD, x)


x <- matrix(data = 0, 
               nrow = 2000, ncol = 2000, 
               dimnames = list(com$id, com$id))
y <- matrix(data = NA, 
                 nrow = 2000, ncol = 2000, 
                 dimnames = list(com$id, com$id))
z <- matrix(data = 0.0, 
            nrow = 2000, ncol = 2000, 
            dimnames = list(com$id, com$id))



dim(xx)

head(com)
