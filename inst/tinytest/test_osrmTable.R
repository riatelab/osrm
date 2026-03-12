if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  

  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "bike")

  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "foot")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
    wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))

  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############# server param ##################
  C <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving")
  wait()
  expect_equal(dim(A$durations), c(10,10))
  
  ############# server error #####################"
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                 measure = c("wrong_measure"), 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving"))
  wait()
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         osrm.server = "https://rosuter.psroject-ossrm.org/", 
                         osrm.profile = "driving"))
  wait()

}


if(local_server){
  ############## ONLY LOCAL ############################################
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  ######### exclude
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 exclude = "motorway")
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 exclude = "motorway")
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  ##### server error
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         measure = c("wrong_measure")))
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         osrm.server = "http://0.0.0.0:5100/", 
                         osrm.profile = "car"))
}