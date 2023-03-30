if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  r <- osrmRoute(src = x_sf[1, ], dst = x_sf[16, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  r <- osrmRoute(loc = x_sf[1:3, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  # Return only duration and distance
  r <- osrmRoute(loc = x_sf[1:3, ], overview = FALSE)
  wait()
  expect_true(is.numeric(r))
  expect_true(length(r) == 2)
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  r <- osrmRoute(src = x_sf[1, ], dst = x_sf[16, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  r <- osrmRoute(loc = x_sf[1:3, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  # Return only duration and distance
  r <- osrmRoute(loc = x_sf[1:3, ], overview = FALSE)
  wait()
  expect_true(is.numeric(r))
  expect_true(length(r) == 2)
  
  
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  r <- osrmRoute(src = x_sf[1, ], dst = x_sf[16, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")

  r <- osrmRoute(loc = x_sf[1:3, ])
  wait()
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  # Return only duration and distance
  r <- osrmRoute(loc = x_sf[1:3, ], overview = FALSE)
  wait()
  expect_true(is.numeric(r))
  expect_true(length(r) == 2)
  
  ############# server param ##################""
  r <- osrmRoute(src = x_sf[1, ], dst = x_sf[16, ], 
                 osrm.server = "https://router.project-osrm.org/", 
                 osrm.profile = "driving")
  wait()
  expect_true(inherits(r, "sf"))
  
  # server error
  expect_error(osrmRoute(src = x_sf[1, ], dst = x_sf[16, ], 
                         osrm.server = "https://router.project-osrm.orgS/", 
                         osrm.profile = "driving"))
  wait()
  expect_error(osrmRoute(src = x_sf[1, ], dst = x_sf[16, ], 
                         exclude = "motorway",
                         osrm.server = "https://router.project-osrm.org/", 
                         osrm.profile = "driving"))
  wait()
}



# ############## ONLY LOCAL ############################################
if(local_server){
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "test")
  r <- osrmRoute(src = x_sf[1, ], dst = x_sf[16, ])
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  r <- osrmRoute(loc = x_sf[1:3, ])
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_true(nrow(r) == 1)
  expect_identical(colnames(r), 
                   c("src", "dst", "duration", "distance", "geometry"))
  expect_true(st_geometry_type(r) == "LINESTRING")
  
  # Return only duration and distance
  r <- osrmRoute(loc = x_sf[1:3, ], overview = FALSE)
  expect_true(is.numeric(r))
  expect_true(length(r) == 2)
  
  # server error
  expect_error(osrmRoute(src = x_sf[1, ], dst = x_sf[16, ], 
                         osrm.server = "http://0.0.0.0:5100/", 
                         osrm.profile = "driving"))
  expect_error(osrmRoute(src = x_sf[1, ], dst = x_sf[16, ], 
                         exclude = "autoroute",
                         osrm.server = "http://0.0.0.0:5000/", 
                         osrm.profile = "driving"))
}

