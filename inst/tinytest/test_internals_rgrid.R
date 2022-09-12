# Test rgrid()
loc <- x_sf[1,]
dmax <- 2000
res <- 30
# rgrid_out <- osrm:::rgrid(loc, dmax, res)
# saveRDS(rgrid_out, 'inst/tinytest/rgrid_out.rds')
rgrid_out <- osrm:::rgrid(loc, dmax, res)
expect_equivalent(rgrid_out, readRDS("rgrid_out.rds"))
