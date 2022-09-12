# Test coord_format()
# coord_format_in <- readRDS("inst/tinytest/tab_format_in.rds")
# coord_format_out <- osrm:::coord_format(res = coord_format_in$res,
#                                     src = coord_format_in$src,
#                                     dst = coord_format_in$dst)
# saveRDS(coord_format_out, file = "inst/tinytest/coord_format_out.rds")
coord_format_in <- readRDS("tab_format_in.rds")
expect_identical(osrm:::coord_format(res = coord_format_in$res,
                                     src = coord_format_in$src,
                                     dst = coord_format_in$dst), 
                 readRDS("coord_format_out.rds"))

