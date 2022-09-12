# Test tab_format()
# tab_format_in <- osrmTable(src = x_sf[1:3,], dst = x_sf[4:6, ],
#                            measure = c("distance", "duration"))
# saveRDS(tab_format_in, file = "inst/tinytest/tab_format_in.rds")
# tab_format_out_dist <- osrm:::tab_format(res = tab_format_in$res,
#                                     src = tab_format_in$src,
#                                     dst = tab_format_in$dst,
#                                     type = "distance")
# tab_format_out_dur <- osrm:::tab_format(res = tab_format_in$res,
#                                          src = tab_format_in$src,
#                                          dst = tab_format_in$dst,
#                                          type = "duration")
# saveRDS(tab_format_out_dist, file = "inst/tinytest/tab_format_out_dist.rds")
# saveRDS(tab_format_out_dur, file = "inst/tinytest/tab_format_out_dur.rds")
tab_format_in <- readRDS("tab_format_in.rds")
expect_identical(osrm:::tab_format(res = tab_format_in$res, 
                                   src = tab_format_in$src, 
                                   dst = tab_format_in$dst, 
                                   type = "duration"), 
                 readRDS("tab_format_out_dur.rds"))
expect_identical(osrm:::tab_format(res = tab_format_in$res, 
                                   src = tab_format_in$src, 
                                   dst = tab_format_in$dst, 
                                   type = "distance"),
                 readRDS("tab_format_out_dist.rds"))

