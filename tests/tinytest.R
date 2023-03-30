if (requireNamespace("tinytest", quietly = TRUE)) {
  local_server <- FALSE
  demo_server <- TRUE
  suppressPackageStartupMessages(library(sf))
  x_sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
    quiet = TRUE
  )
  x_df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
  x_v <- c(13.26403, 52.48707)
  x_df <- x_df[, -1]
  x_sfc <- st_geometry(x_sf)
  x_m <- as.matrix(x_df)
  row.names(x_sf) <- paste0("sf", row.names(x_sf))
  wait <- function() {
    Sys.sleep(1)
  }
  tinytest::test_package("osrm")
}
