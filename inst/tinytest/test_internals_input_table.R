# input_table()
# empty objects
expect_error(osrm:::input_table(x = st_sf(st_sfc()), id = "src"))
expect_error(osrm:::input_table(x = st_sfc(), id = "src"))
expect_error(osrm:::input_table(x = data.frame(), id = "src"))
expect_error(osrm:::input_table(x = matrix(), id = "src"))

# x not a point
expect_error(osrm:::input_table(x = st_cast(x_sf, "MULTIPOINT"), id = "src"))

# x does not have coordinates
expect_error(osrm:::input_table(x = st_drop_geometry(x_sf), id = "src"))  

# x is not the correct type
expect_error(osrm:::input_table(x = st_crs(x_sf), id = "src"))  

# correct input
# input_table_out_df <- osrm:::input_table(x = x_df, id = "src")
# input_table_out_m <- osrm:::input_table(x = x_m, id = "src")
# input_table_out_sfc <- osrm:::input_table(x = x_sfc, id = "src")
# input_table_out_sf <- osrm:::input_table(x = x_sf, id = "src")
# saveRDS(input_table_out_df, 'inst/tinytest/input_table_out_df.rds')
# saveRDS(input_table_out_m, 'inst/tinytest/input_table_out_m.rds')
# saveRDS(input_table_out_sfc, 'inst/tinytest/input_table_out_sfc.rds')
# saveRDS(input_table_out_sf, 'inst/tinytest/input_table_out_sf.rds')

expect_identical(osrm:::input_table(x = x_df, id = "src"), 
                 readRDS('input_table_out_df.rds'))
expect_identical(osrm:::input_table(x = x_m, id = "src"), 
                 readRDS('input_table_out_m.rds'))
expect_identical(osrm:::input_table(x = x_sfc, id = "src"), 
                 readRDS('input_table_out_sfc.rds'))
expect_identical(osrm:::input_table(x = x_sf, id = "src"), 
                 readRDS('input_table_out_sf.rds'))
























# points with missing (NA/NaN) coordinates are skipped with a warning
x_nan <- data.frame(
  lon = c(13.26, NaN, 13.41, 13.45),
  lat = c(52.48, NaN, 52.52, NA),
  row.names = c("a", "b", "c", "d")
)
expect_warning(
  res <- osrm:::input_table(x = x_nan, id = "src"),
  "2 point\\(s\\) with missing \\(NA/NaN\\) or non-finite coordinates"
)
expect_identical(res$id, c("a", "c"))
expect_identical(res, suppressWarnings(osrm:::input_table(x = x_nan[c(1, 3), ], id = "src")))
expect_warning(osrm:::input_table(x = as.matrix(x_nan), id = "src"))

# error if no point with valid coordinates remains
expect_error(
  suppressWarnings(osrm:::input_table(x = data.frame(lon = NaN, lat = NaN), id = "src"))
)
