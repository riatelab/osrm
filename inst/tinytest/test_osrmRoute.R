if(home){
  suppressPackageStartupMessages(library(sf))
  apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
                         quiet = TRUE)
  apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
  
  ss <- function(){Sys.sleep(1)}
  
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  ss()
  # Travel path between points
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ])
  expect_true(methods::is(r, "sf"))
  ss()

  # Return only duration and distance
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
                 overview = FALSE)
  expect_equal(length(r), 2)
  ss()
  # Using only coordinates
  r <-  osrmRoute(src = c(13.412, 52.502), 
                  dst = c(13.454, 52.592))
  expect_true(methods::is(r, "sf"))
  ss()
  # Using via points
  pts <- structure(
    list(x = c(13.32500, 13.30688, 13.30519, 13.31025, 
               13.4721, 13.56651, 13.55303, 13.37263, 13.50919, 13.5682), 
         y = c(52.40566, 52.44491, 52.52084, 52.59318, 52.61063, 52.55317, 
               52.50186, 52.49468, 52.46441, 52.39669)), 
    class = "data.frame", row.names = c(NA, -10L))
  r <- osrmRoute(loc = pts)
  expect_true(methods::is(r, "sf"))
  ss()
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "bike")
  ss()
  # Travel path between points
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ])
  expect_true(methods::is(r, "sf"))
  ss()
  # Return only duration and distance
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
                 overview = FALSE)
  expect_equal(length(r), 2)
  ss()
  # Using only coordinates
  r <-  osrmRoute(src = c(13.412, 52.502), 
                  dst = c(13.454, 52.592))
  expect_true(methods::is(r, "sf"))
  ss()
  # Using via points
  pts <- structure(
    list(x = c(13.32500, 13.30688, 13.30519, 13.31025, 
               13.4721, 13.56651, 13.55303, 13.37263, 13.50919, 13.5682), 
         y = c(52.40566, 52.44491, 52.52084, 52.59318, 52.61063, 52.55317, 
               52.50186, 52.49468, 52.46441, 52.39669)), 
    class = "data.frame", row.names = c(NA, -10L))
  r <- osrmRoute(loc = pts)
  expect_true(methods::is(r, "sf"))
  ss()
  
  
  
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "foot")
  ss()
  # Travel path between points
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ])
  expect_true(methods::is(r, "sf"))
  ss()
  # Return only duration and distance
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
                 overview = FALSE)
  expect_equal(length(r), 2)
  ss()
  # Using only coordinates
  r <-  osrmRoute(src = c(13.412, 52.502), 
                  dst = c(13.454, 52.592))
  expect_true(methods::is(r, "sf"))
  ss()
  # Using via points
  pts <- structure(
    list(x = c(13.32500, 13.30688, 13.30519, 13.31025, 
               13.4721, 13.56651, 13.55303, 13.37263, 13.50919, 13.5682), 
         y = c(52.40566, 52.44491, 52.52084, 52.59318, 52.61063, 52.55317, 
               52.50186, 52.49468, 52.46441, 52.39669)), 
    class = "data.frame", row.names = c(NA, -10L))
  r <- osrmRoute(loc = pts)
  expect_true(methods::is(r, "sf"))
  ss()
  
  
  ############# fun param ##################""
  ss()
  # Travel path between points
  r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ], 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving")
  expect_true(methods::is(r, "sf"))
  
  
  # 
  # ############## ONLY LOCAL ############################################
  if(localtest){
    options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "test")
    # Travel path between points
    r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ])
    expect_true(methods::is(r, "sf"))
    # Travel path between points excluding motorways
    r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ],
                   exclude = "motorway")
    expect_true(methods::is(r, "sf"))
    # Return only duration and distance
    r <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.df[16, ],
                   overview = FALSE)
    expect_equal(length(r), 2)
    # Using only coordinates
    r <-  osrmRoute(src = c(13.412, 52.502),
                    dst = c(13.454, 52.592))
    expect_true(methods::is(r, "sf"))
    # Using via points
    pts <- structure(
      list(x = c(13.32500, 13.30688, 13.30519, 13.31025,
                 13.4721, 13.56651, 13.55303, 13.37263, 13.50919, 13.5682),
           y = c(52.40566, 52.44491, 52.52084, 52.59318, 52.61063, 52.55317,
                 52.50186, 52.49468, 52.46441, 52.39669)),
      class = "data.frame", row.names = c(NA, -10L))
    r <- osrmRoute(loc = pts)
    expect_true(methods::is(r, "sf"))

  }
}
