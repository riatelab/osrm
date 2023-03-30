if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  r <- osrmTrip(loc = x_sf[1:16, ])
  wait()
  trip <- r[[1]]$trip
  trip_summary <- r[[1]]$summary
  expect_true(inherits(trip, "sf"))
  expect_identical(st_crs(trip), st_crs(x_sf))
  expect_true(nrow(trip) == 16)
  expect_identical(colnames(trip), 
                   c("start", "end", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(trip, by_geometry = FALSE) == "LINESTRING")
  expect_identical(names(trip_summary),c('duration', 'distance'))
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  r <- osrmTrip(loc = x_sf[1:16, ])
  wait()
  trip <- r[[1]]$trip
  trip_summary <- r[[1]]$summary
  expect_true(inherits(trip, "sf"))
  expect_identical(st_crs(trip), st_crs(x_sf))
  expect_true(nrow(trip) == 16)
  expect_identical(colnames(trip), 
                   c("start", "end", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(trip, by_geometry = FALSE) == "LINESTRING")
  expect_identical(names(trip_summary),c('duration', 'distance'))
  
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  r <- osrmTrip(loc = x_sf[1:16, ])
  wait()
  trip <- r[[1]]$trip
  trip_summary <- r[[1]]$summary
  expect_true(inherits(trip, "sf"))
  expect_identical(st_crs(trip), st_crs(x_sf))
  expect_true(nrow(trip) == 16)
  expect_identical(colnames(trip), 
                   c("start", "end", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(trip, by_geometry = FALSE) == "LINESTRING")
  expect_identical(names(trip_summary),c('duration', 'distance'))
  
  ############# server param ##################""
  r <- osrmTrip(loc = x_sf[1:5,], 
                osrm.server = "http://router.project-osrm.org/", 
                osrm.profile = "driving")
  wait()
  trip <- r[[1]]$trip
  trip_summary <- r[[1]]$summary
  expect_true(inherits(trip, "sf"))
  expect_identical(st_crs(trip), st_crs(x_sf))
  expect_true(nrow(trip) == 5)
  expect_identical(colnames(trip), 
                   c("start", "end", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(trip, by_geometry = FALSE) == "LINESTRING")
  expect_identical(names(trip_summary),c('duration', 'distance'))  
  # server error
  expect_error(osrmTrip(loc = x_sf[1:5, ], 
                        osrm.server = "https://router.project-osrm.orgS/", 
                        osrm.profile = "driving"))
  wait()
  expect_error(osrmTrip(loc = x_sf[1:5, ], 
                        exclude = "motorway",
                        osrm.server = "https://router.project-osrm.org/", 
                        osrm.profile = "driving"))
  wait()
}


# ############## ONLY LOCAL ############################################
if(local_server){
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "test")
  r <- osrmTrip(loc = x_sf[1:16, ])
  trip <- r[[1]]$trip
  trip_summary <- r[[1]]$summary
  expect_true(inherits(trip, "sf"))
  expect_identical(st_crs(trip), st_crs(x_sf))
  expect_true(nrow(trip) == 16)
  expect_identical(colnames(trip), 
                   c("start", "end", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(trip, by_geometry = FALSE) == "LINESTRING")
  expect_identical(names(trip_summary),c('duration', 'distance'))
  
  # server error
  expect_error(osrmTrip(loc = x_sf[1:5, ], 
                        osrm.server = "http://0.0.0.0:5100/", 
                        osrm.profile = "driving"))
  expect_error(osrmTrip(loc = x_sf[1:5, ], 
                        exclude = "autoroute",
                        osrm.server = "http://0.0.0.0:5000/", 
                        osrm.profile = "driving"))
}
