if(home){
  suppressPackageStartupMessages(library(sf))
  apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
                         quiet = TRUE)
  apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
  
  ss <- function(){Sys.sleep(1)}
  
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  ss()
  A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                 dst = apotheke.df[1:10,c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(dim(A$distances), c(10,10))
  ss()
  A <- osrmTable(src = apotheke.sf[1:10, ],
                 dst = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  #### osrmTable
  ss()
  A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                 dst = apotheke.df[1:10,c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(dim(A$distances), c(10,10))
  ss()
  A <- osrmTable(src = apotheke.sf[1:10, ],
                 dst = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  #### osrmTable
  ss()
  A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                 dst = apotheke.df[1:10,c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(dim(A$distances), c(10,10))
  ss()
  A <- osrmTable(src = apotheke.sf[1:10, ],
                 dst = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  ss()
  B <- osrmTable(loc = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############# fun param ##################""
  C <- osrmTable(loc = apotheke.sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving")
  expect_equal(dim(A$durations), c(10,10))
  
  
  
  
  
  ################### GEPAF
  ss()
  A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                 dst = apotheke.df[1:10,c("id","lon","lat")], 
                 measure = c("distance", "duration"), gepaf = TRUE)
  expect_equal(dim(A$distances), c(10,10))
  
  
  ############## ONLY LOCAL ############################################
  if(localtest){
    options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "test")
    A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                   dst = apotheke.df[1:10,c("id","lon","lat")],
                   measure = c("distance", "duration"))
    B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")],
                   measure = c("distance", "duration"))
    expect_equal(A$distances,B$distances)
    expect_equal(dim(A$distances), c(10,10))
    A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                   dst = apotheke.df[1:10,c("id","lon","lat")],
                   measure = c("distance"), exclude = "motorway")
    B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")],
                   exclude = "motorway", measure = "distance")
    expect_equal(A$distances,B$distances)
    expect_equal(dim(A$distances), c(10,10))
    A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                   dst = apotheke.df[1:10,c("id","lon","lat")],
                   measure = c("duration"), exclude = "motorway")
    B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")],
                   exclude = "motorway", measure = "duration")
    expect_equal(A$durations,B$durations)
    expect_equal(dim(A$durations), c(10,10))
    A <- osrmTable(src = apotheke.sf[1:10, ],
                   dst = apotheke.sf[1:10, ],
                   measure = c("distance", "duration"))
    B <- osrmTable(loc = apotheke.sf[1:10, ],
                   measure = c("distance", "duration"))
    expect_equal(A$distances,B$distances)
    expect_equal(A$durations,B$durations)
    expect_equal(dim(A$durations), c(10,10))
    expect_equal(dim(A$distances), c(10,10))
    A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                   dst = apotheke.df[1:10,c("id","lon","lat")], 
                   measure = c("distance"), exclude = "motorway")
    B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
                   measure = c("distance"), exclude = "motorway")
    expect_equal(A$distances,B$distances)
    expect_equal(dim(A$distances), c(10,10))
    A <- osrmTable(src = apotheke.df[1:10,c("id","lon","lat")],
                   dst = apotheke.df[1:10,c("id","lon","lat")], 
                   measure = c("duration"), exclude = "motorway")
    B <- osrmTable(loc = apotheke.df[1:10, c("id","lon","lat")], 
                   exclude = "motorway", measure = "duration")
    expect_equal(A$durations,B$durations)
    expect_equal(dim(A$durations), c(10,10))
  }
}
