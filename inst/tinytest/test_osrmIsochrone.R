if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  wait()
  r <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  r <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  r <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ############# server param ##################""
  wait()
  r <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), res = 10, 
                     osrm.server = "https://router.project-osrm.org/", 
                     osrm.profile = "driving")
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  
  # server error
  wait()
  expect_error(osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), 
                             res = 10, 
                             osrm.server = "https://router.project-osrm.orgS/", 
                             osrm.profile = "driving"))
  wait()
  expect_error(osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), 
                             res = 10,  
                             exclude = "motorway",
                             osrm.server = "https://router.project-osrm.org/", 
                             osrm.profile = "driving"))
}

# ############## ONLY LOCAL ############################################
if(local_server){
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")
  r <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  r <- osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  
  # point too far
  expect_warning(osrmIsochrone(loc = c(10, 10), breaks = seq(0,14,2), 
                               res = 10,
                               osrm.server = "http://0.0.0.0:5000/", 
                               osrm.profile = "driving"))
  
  
  # server error
  expect_error(osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10,
                             osrm.server = "http://0.0.0.0:5100/", 
                             osrm.profile = "driving"))
  expect_error(osrmIsochrone(loc = x_sf[10, ], breaks = seq(0,14,2), res = 10,
                             exclude = "autoroute",
                             osrm.server = "http://0.0.0.0:5000/", 
                             osrm.profile = "driving"))
}
