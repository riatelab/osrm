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

# contains NA / Inf / NaN coordinates from matrix, data.frame, sf
expect_error(
  osrm:::input_table(x = matrix(c(NA, 1, 50, 50), ncol = 2), id = "loc"),
  '"loc" contains missing \\(NA\\), infinite \\(Inf/-Inf\\) or invalid \\(NaN\\) coordinates.'
)
expect_error(
  osrm:::input_table(x = data.frame(x = c(NaN, 1), y = c(50, 50)), id = "loc"),
  '"loc" contains missing \\(NA\\), infinite \\(Inf/-Inf\\) or invalid \\(NaN\\) coordinates.'
)
expect_error(
  osrm:::input_table(x = st_as_sf(data.frame(x = c(Inf, 1), y = c(50, 50)), coords = c("x", "y"), crs = "EPSG:4326"), id = "loc"),
  '"loc" contains missing \\(NA\\), infinite \\(Inf/-Inf\\) or invalid \\(NaN\\) coordinates.'
)










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























