if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  r <- osrmNearest(loc = x_sf[1, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), c("id", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "POINT")
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  r <- osrmNearest(loc = x_sf[1, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), c("id", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "POINT")
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  r <- osrmNearest(loc = x_sf[1, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), c("id", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "POINT")
  
  ############# server param ##################""
  r <-  osrmNearest(loc = x_sf[1, ], 
                    osrm.server = "https://router.project-osrm.org/", 
                    osrm.profile = "driving")
  wait()
  expect_true(inherits(r, "sf"))
  # server error
  expect_error(osrmNearest(loc = x_sf[1, ], 
                           osrm.server = "https://router.project-osrm.orgS/", 
                           osrm.profile = "driving"))
  wait()
  expect_error(osrmNearest(loc = x_sf[1, ], 
                           exclude = "motorway",
                           osrm.server = "https://router.project-osrm.org/", 
                           osrm.profile = "driving"))
  wait()
}



# ############## ONLY LOCAL ############################################
if(local_server){
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "test")
  r <- osrmNearest(loc = x_sf[1, ])
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), c("id", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "POINT")
  
  # server error
  expect_error(osrmNearest(loc = x_sf[1, ], 
                           osrm.server = "http://0.0.0.0:5100/", 
                           osrm.profile = "driving"))
  expect_error(osrmNearest(loc = x_sf[1, ], 
                           exclude = "autoroute",
                           osrm.server = "http://0.0.0.0:5000/", 
                           osrm.profile = "driving"))
}

