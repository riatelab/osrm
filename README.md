# osrm R package

[![Version](http://www.r-pkg.org/badges/version/osrm)](https://CRAN.R-project.org/package=osrm/)
![](http://cranlogs.r-pkg.org/badges/osrm?color=brightgreen)
[![R build status](https://github.com/rCarto/osrm/workflows/R-CMD-check/badge.svg)](https://github.com/rCarto/osrm/actions)
[![codecov](https://codecov.io/gh/rCarto/osrm/branch/master/graph/badge.svg?token=JOJNuBCH9M)](https://codecov.io/gh/rCarto/osrm)




***Interface Between R and the OpenStreetMap-Based Routing Service [OSRM](http://project-osrm.org/)***

![](https://raw.githubusercontent.com/rCarto/osrm/master/img/cover.png)

## Description
OSRM is a routing service based on OpenStreetMap data. See <http://project-osrm.org/> for more information. This package allows to compute distances (travel time and kilometric distance) between points and travel time matrices.   

This package relies on the usage of a running OSRM service (tested with v5.23.0 of OSRM).    

You can run your own instance of OSRM following guidelines provided here:    [https://github.com/Project-OSRM/osrm-backend](https://github.com/Project-OSRM/osrm-backend).     
The simplest solution is probably the one based on [docker containers](https://github.com/Project-OSRM/osrm-backend#using-docker).    

To set the OSRM server, use the `osrm.server` option: `options(osrm.server = "http://address.of.the.server/")`.     
To set the profile, use the `osrm.profile` option: `options(osrm.profile = "name.of.the.profile")`.  
The "car" profile is set by default. Other possible profiles are "bike" and "foot".  
A typical local setup, corresponding to the Docker example, would be:  
`options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")`


:warning: **You must be careful using the OSRM demo server**:    
> [One request per second max. No scraping, no heavy usage.](https://routing.openstreetmap.de/about.html)



:heavy_exclamation_mark: **OSRM Demoserver - call for hosting volunteers**:  
> ["This is a call to the community to see if someone else is willing to take 
up the mantle for running the demo server.  If nobody steps up, then the service 
will stop working on Mar 09, 2020.""](https://lists.openstreetmap.org/pipermail/osrm-talk/2020-January/001834.html)


:heavy_exclamation_mark: **To consider when using OSRM**:    
> ["Most of the previously active core devs have either moved on to new roles, or are simply busy on different projects (...)"](https://github.com/Project-OSRM/osrm-backend/issues/5463)




## Features

* `osrmTable` Get travel time matrices between points.

* `osrmRoute` Get the shortest path between two points.

* `osrmTrip` Get the travel geometry between multiple unordered points.

* `osrmIsochrone` Get polygons of isochrones.


## Demo

### `osrmTable`

```r
library(osrm)
data("berlin")
# Travel time matrix
distA <- osrmTable(loc = apotheke.sf[1:5,])
distA$durations
```
|           | 440338666| 538057637| 977657079| 3770254015| 364363337|
|:----------|---------:|---------:|---------:|----------:|---------:|
|440338666  |       0.0|      24.2|      48.0|       26.9|      15.0|
|538057637  |      27.1|       0.0|      44.2|       16.4|      22.6|
|977657079  |      47.6|      41.7|       0.0|       35.0|      36.2|
|3770254015 |      28.7|      16.9|      34.6|        0.0|      17.0|
|364363337  |      15.4|      20.7|      34.4|       15.9|       0.0|

### `osrmRoute`

```r
library(osrm)
library(sf)
library(cartography)
data("berlin")
route <- osrmRoute(src = apotheke.sf[74,], dst = apotheke.sf[55,],
                   overview = "full", returnclass = "sf")
# Display the path
osm <- getTiles(x = route, crop = TRUE, type = "osm", zoom = 13)
tilesLayer(osm)
plot(st_geometry(route), lwd = 4, add = TRUE)
plot(st_geometry(route), lwd = 1, col = "white", add = TRUE)
plot(st_geometry(apotheke.sf[c(74,55),]), pch = 20, col = "red", add = TRUE)
```
![](https://raw.githubusercontent.com/rCarto/osrm/master/img/route.png)


### `osrmTrip`

```r
library(osrm)
library(sf)
library(cartography)
data("berlin")
# Get a trip with a SpatialPointsDataFrame
trips <- osrmTrip(loc = apotheke.sf[10:20,], returnclass="sf")
trip <- trips[[1]]$trip
osm2 <- getTiles(x = trip, crop = TRUE, type = "cartolight", zoom = 11)
tilesLayer(x = osm2)
plot(st_geometry(trip), col = "black", lwd = 4, add = TRUE )
plot(st_geometry(trip), col = c("red", "white"), lwd = 1, add=TRUE)
plot(st_geometry(apotheke.sf[10:20,]), pch = 21, bg = "red", cex = 1.5, add=TRUE)
```

![](https://raw.githubusercontent.com/rCarto/osrm/master/img/trip.png)

### `osrmIsochrone`

```r
library(osrm)
library(sf)
library(cartography)
data("berlin")
iso <- osrmIsochrone(loc = apotheke.sf[87,], returnclass="sf",
                     breaks = seq(from = 0, to = 14, by = 2), res = 50)
osm3 <- getTiles(x = iso, crop = FALSE, type = "osm", zoom = 12)
tilesLayer(x = osm3)
bks <- sort(c(unique(iso$min), max(iso$max)))
cols <- paste0(carto.pal("turquoise.pal", n1 = length(bks)-1), 80)
choroLayer(x = iso, var = "center", breaks = bks,
           border = NA, col = cols,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)
plot(st_geometry(apotheke.sf[87,]), pch = 21, bg = "red", 
     cex = 1.5, add=TRUE)

```
![](https://raw.githubusercontent.com/rCarto/osrm/master/img/iso.png)


## Installation

* Development version on GitHub
```{r}
remotes::install_github("rCarto/osrm")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=osrm/)
```{r}
install.packages("osrm")
```

## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/rCarto/osrm/pulls) and report issues or ask questions [here](https://github.com/rCarto/osrm/issues).
