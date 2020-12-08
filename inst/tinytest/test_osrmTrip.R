home <- length(unclass(packageVersion("osrm"))[[1]]) == 4
localtest <- F

if(home){
  suppressPackageStartupMessages(library(sf))
  data("berlin")
  ss <- function(){Sys.sleep(1)}
  
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  ss()
  trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf")
  expect_true(methods::is(trips[[1]]$trip, "sf"))
  
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  ss()
  trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf")
  expect_true(methods::is(trips[[1]]$trip, "sf"))
  
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  ss()
  trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf")
  expect_true(methods::is(trips[[1]]$trip, "sf"))
  
  
  ############# fun param ##################""
  ss()
  trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf",
                    osrm.server = "http://router.project-osrm.org/", 
                    osrm.profile = "driving")
  expect_true(methods::is(trips[[1]]$trip, "sf"))
  
  ############## ONLY LOCAL ############################################
  if(localtest){
    options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "bike")
    trips <- osrmTrip(loc = apotheke.sf, returnclass = "sf")
    expect_true(methods::is(trips[[1]]$trip, "sf"))
  }
}
