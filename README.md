
# osrm <img src="man/figures/logo.png" align="right" width="140"/>

[![CRAN](https://www.r-pkg.org/badges/version/osrm)](https://cran.r-project.org/package=osrm)
[![downloads](https://cranlogs.r-pkg.org/badges/osrm?color=brightgreen)](https://cran.r-project.org/package=osrm)
[![R build
status](https://github.com/riatelab/osrm/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/riatelab/osrm/actions)
[![codecov](https://codecov.io/gh/riatelab/osrm/branch/master/graph/badge.svg?token=JOJNuBCH9M)](https://app.codecov.io/gh/riatelab/osrm)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04574/status.svg)](https://doi.org/10.21105/joss.04574)

***Interface Between R and the OpenStreetMap-Based Routing Service
[OSRM](http://project-osrm.org/)***

![](https://raw.githubusercontent.com/riatelab/osrm/master/img/cover.png)

## Description

OSRM is a routing service based on OpenStreetMap data. See
<http://project-osrm.org/> for more information. This package enables
the computation of routes, trips, isochrones and travel distances
matrices (travel time and kilometric distance).

This package relies on the usage of a running OSRM service (tested with
v5.27.0 of OSRM).

You can run your own instance of OSRM following guidelines provided
[here](https://github.com/Project-OSRM/osrm-backend). The simplest
solution is probably the one based on [docker
containers](https://github.com/Project-OSRM/osrm-backend#using-docker).

:warning: **You must be careful using the OSRM demo server and read the
[*about* page](https://routing.openstreetmap.de/about.html) of the
service**:

> [One request per second max. No scraping, no heavy
> usage.](https://routing.openstreetmap.de/about.html)

## Features

- `osrmTable()` uses the *table* service to query time/distance
  matrices,
- `osrmRoute()` uses the *route* service to query routes,
- `osrmTrip()` uses the *trip* service to query trips,
- `osrmIsochrone()` and `osrmIsodistance()` use multiple `osrmTable()`
  calls to create isochrones or isodistances polygons.

## Demo

This is a short overview of the main features of `osrm`. The dataset
used here is shipped with the package, it is a sample of 100 random
pharmacies in Berlin ([© OpenStreetMap
contributors](https://www.openstreetmap.org/copyright/en)) stored in a
[geopackage](https://www.geopackage.org/) file.

- `osrmTable()` gives access to the *table* OSRM service. In this
  example we use this function to get the median time needed to access
  any pharmacy from any other pharmacy.

``` r
library(osrm)
```

    ## Data: (c) OpenStreetMap contributors, ODbL 1.0 - http://www.openstreetmap.org/copyright

    ## Routing: OSRM - http://project-osrm.org/

``` r
library(sf)
```

    ## Linking to GEOS 3.9.0, GDAL 3.2.2, PROJ 7.2.1; sf_use_s2() is TRUE

``` r
pharmacy <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"), 
                    quiet = TRUE)
travel_time <- osrmTable(loc = pharmacy)
travel_time$durations[1:5,1:5]
```

    ##      1    2    3    4    5
    ## 1  0.0 21.1 33.4 21.2 12.6
    ## 2 22.1  0.0 42.3 16.1 20.2
    ## 3 33.0 43.0  0.0 30.5 27.4
    ## 4 20.1 15.3 29.7  0.0 12.7
    ## 5 10.2 20.3 26.8 12.3  0.0

``` r
diag(travel_time$durations) <- NA
median(travel_time$durations, na.rm = TRUE)
```

    ## [1] 21.4

The median time needed to access any pharmacy from any other pharmacy is
21.4 minutes.

- `osrmRoute()` is used to compute the shortest route between two
  points. Here we compute the shortest route between the two first
  pharmacies.

``` r
(route <- osrmRoute(src = pharmacy[1, ], dst = pharmacy[2, ]))
```

    ## Simple feature collection with 1 feature and 4 fields
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: -13170.51 ymin: 5837172 xmax: -3875.771 ymax: 5841047
    ## Projected CRS: WGS 84 / UTM zone 34N
    ##     src dst duration distance                       geometry
    ## 1_2   1   2 21.11667   12.348 LINESTRING (-13170.51 58410...

This route is 12.3 kilometers long and it takes 21.1 minutes to drive
through it.

``` r
plot(st_geometry(route))
plot(st_geometry(pharmacy[1:2,]), pch = 20, add = T, cex = 1.5)
```

![](route.png)

- `osrmTrip()` can be used to resolve the travelling salesman problem,
  it gives the shortest trip between a set of unordered points. In this
  example we want to obtain the shortest trip between the first five
  pharmacies.

``` r
(trips <- osrmTrip(loc = pharmacy[1:5, ], overview = "full"))
```

    ## [[1]]
    ## [[1]]$trip
    ## Simple feature collection with 5 features and 4 fields
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: -13431.24 ymin: 5837172 xmax: -3875.582 ymax: 5856332
    ## Projected CRS: WGS 84 / UTM zone 34N
    ##   start end duration distance                       geometry
    ## 1     1   2 21.11667  12.3480 LINESTRING (-13170.77 58410...
    ## 2     2   4 16.10833   8.4273 LINESTRING (-3875.582 58379...
    ## 3     4   3 29.69000  18.1448 LINESTRING (-7444.513 58427...
    ## 4     3   5 27.39833  16.4265 LINESTRING (-8027.41 585621...
    ## 5     5   1 10.15333   4.2289 LINESTRING (-11716.36 58435...
    ## 
    ## [[1]]$summary
    ## [[1]]$summary$duration
    ## [1] 104.4667
    ## 
    ## [[1]]$summary$distance
    ## [1] 59.5755

The shortest trip between these pharmacies takes 104.5 minutes and is
59.6 kilometers long. The steps of the trip are described in the “trip”
sf object (point 1 \> point 2 \> point 4 \> point 3 \> point 5 \> point
1).

``` r
mytrip <- trips[[1]]$trip
# Display the trip
plot(st_geometry(mytrip), col = c("black", "grey"), lwd = 2)
plot(st_geometry(pharmacy[1:5, ]), cex = 1.5, pch = 21, add = TRUE)
text(st_coordinates(pharmacy[1:5,]), labels = row.names(pharmacy[1:5,]), 
     pos = 2)
```

![](trip.png)

- `osrmIsochrone()` computes areas that are reachable within a given
  time span from a point and returns the reachable regions as polygons.
  These areas of equal travel time are called isochrones. Here we
  compute the isochrones from a specific point defined by its longitude
  and latitude.

``` r
(iso <- osrmIsochrone(loc = c(13.43,52.47), breaks = seq(0,12,2)))
```

    ## Simple feature collection with 5 features and 3 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 13.34397 ymin: 52.41642 xmax: 13.50187 ymax: 52.51548
    ## Geodetic CRS:  WGS 84
    ##   id isomin isomax                       geometry
    ## 1  1      0      4 MULTIPOLYGON (((13.43743 52...
    ## 2  2      4      6 MULTIPOLYGON (((13.42356 52...
    ## 3  3      6      8 MULTIPOLYGON (((13.40345 52...
    ## 4  4      8     10 MULTIPOLYGON (((13.4077 52....
    ## 5  5     10     12 MULTIPOLYGON (((13.42257 52...

``` r
bks <-  sort(unique(c(iso$isomin, iso$isomax)))
pals <- hcl.colors(n = length(bks) - 1, palette = "Light Grays", rev = TRUE)
plot(iso["isomax"], breaks = bks, pal = pals, 
     main = "Isochrones (in minutes)", reset = FALSE)
points(x = 13.43, y = 52.47, pch = 4, lwd = 2, cex = 1.5)
```

![](iso.png)

## Installation

- Development version on GitHub

``` r
remotes::install_github("riatelab/osrm")
```

- Stable version on [CRAN](https://CRAN.R-project.org/package=osrm/)

``` r
install.packages("osrm")
```

## Community Guidelines

One can contribute to the package through [pull
requests](https://github.com/riatelab/osrm/pulls) and report issues or
ask questions [here](https://github.com/riatelab/osrm/issues). See the
[CONTRIBUTING.md](https://github.com/riatelab/osrm/blob/master/CONTRIBUTING.md)
file for detailed instructions.
