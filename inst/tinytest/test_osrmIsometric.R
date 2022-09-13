if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  wait()
  r <- osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsometric(loc = x_sf[11, ], breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  r <- osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsometric(loc = x_sf[11, ], breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  r <- osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  r <- osrmIsometric(loc = x_sf[11, ], breaks = seq(0,500,100), res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  wait()
  
  ############# server param ##################""
  wait()
  r <- osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), res = 10,
                     osrm.server = "https://router.project-osrm.org/", 
                     osrm.profile = "driving")
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  
  # server error
  wait()
  expect_error(osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100),
                             res = 10, 
                             osrm.server = "https://router.project-osrm.orgS/", 
                             osrm.profile = "driving"))
  wait()
  expect_error(osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100),
                             res = 10,  
                             exclude = "motorway",
                             osrm.server = "https://router.project-osrm.org/", 
                             osrm.profile = "driving"))
}

# ############## ONLY LOCAL ############################################
if(local_server){
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")
  r <- osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), res = 10)
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs("EPSG:4326"))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  r <- osrmIsometric(loc = x_sf[11, ], breaks = seq(0,500,100),res = 10 )
  expect_true(inherits(r, "sf"))
  expect_identical(st_crs(r), st_crs(x_sf))
  expect_identical(colnames(r), 
                   c("id", "isomin", "isomax", "geometry"))
  
  # server error
  expect_error(osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100), 
                             res = 10,
                             osrm.server = "http://0.0.0.0:5100/", 
                             osrm.profile = "driving"))
  expect_error(osrmIsometric(loc = c(13.43,52.47), breaks = seq(0,500,100),
                             res = 10,
                             exclude = "autoroute",
                             osrm.server = "http://0.0.0.0:5000/", 
                             osrm.profile = "driving"))
}
