# if(home){
#   suppressPackageStartupMessages(library(sf))
#   apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
#                          quiet = TRUE)
#   ss <- function(){Sys.sleep(1)}
#   
#   ######################## DEMO car ###########################
#   options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
#   ss()
#   iso <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2))
#   expect_true(methods::is(iso, "sf"))
#   ss()
#   iso2 <- osrmIsochrone(loc = apotheke.sf[10,],
#                         breaks = seq(from = 0, to = 16, by = 2))
#   expect_true(methods::is(iso2, "sf"))
#   iso3 <- osrmIsometric(loc = c(13.43,52.47), res = 4)
#   expect_true(methods::is(iso3, "sf"))
#   
#   
#   
#   
#   ################# DEMO BIKE #####################
#   options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
#   ss()
#   iso <- osrmIsochrone(loc = c(13.43,52.47))
#   expect_true(methods::is(iso, "sf"))
#  
#   
#   
#   
#   ############## DEMO FOOT #################"""""
#   options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
#   ss()
#   iso <- osrmIsochrone(loc = c(13.43,52.47))
#   expect_true(methods::is(iso, "sf"))
#   
#   
#   
#   ############# fun param ##################""
#   ss()
#   iso <- osrmIsochrone(loc = c(13.43,52.47), 
#                        osrm.server = "http://router.project-osrm.org/", 
#                        osrm.profile = "driving", res = 5)
#   expect_true(methods::is(iso, "sf"))
# 
#   ############## ONLY LOCAL ############################################
#   if(localtest){
#     options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "bike")
#     iso <- osrmIsochrone(loc = c(13.43,52.47))
#     expect_true(methods::is(iso, "sf"))
#     
#   }
# }
