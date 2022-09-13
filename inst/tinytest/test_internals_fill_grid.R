# Test fill_grid()
# l <- osrmIsochrone(loc = x_sf[10,], 
#                    breaks = seq(from = 0, to = 10, by = 2))
# saveRDS(l, "inst/tinytest/fill_grid_in.rds")
# fill_grid_out <- fill_grid(destinations = l$destinations, 
#                            measure = l$measure, sgrid = l$sgrid, 
#                            res = l$res, tmax = l$tmax)
# saveRDS(fill_grid_out, "inst/tinytest/fill_grid_out.rds")
fill_grid_in <- readRDS("fill_grid_in.rds")
st_crs(fill_grid_in$sgrid) <- st_crs(fill_grid_in$sgrid)
target <- readRDS("fill_grid_out.rds")
st_crs(target) <- st_crs(target)
fill_grid_current <- osrm:::fill_grid(destinations = fill_grid_in$destinations, 
                                      measure = fill_grid_in$measure, 
                                      sgrid = fill_grid_in$sgrid, 
                                      res = fill_grid_in$res, 
                                      tmax = fill_grid_in$tmax)
expect_identical(fill_grid_current, target)
