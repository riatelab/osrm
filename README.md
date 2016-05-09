# osrm R package

[![Version](http://www.r-pkg.org/badges/version/osrm)](https://CRAN.R-project.org/package=osrm/)
![](http://cranlogs.r-pkg.org/badges/osrm?color=brightgreen)
[![Travis-CI Build Status](https://travis-ci.org/rCarto/osrm.svg?branch=master)](https://travis-ci.org/rCarto/osrm)
![](https://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)  

***Interface Between R and the OpenStreetMap-Based Routing Service [OSRM](http://project-osrm.org/)***

![](https://f.hypotheses.org/wp-content/blogs.dir/1909/files/2016/02/Rroads.png)

## Description
OSRM is a routing service based on OpenStreetMap data. See <http://project-osrm.org/> for more information. A public API exists but one can run its own instance. This package allows to compute distance (travel time and kilometric distance) between points and travel time matrices.

This package relies on the usage of a running OSRM service (tested with version 5.0.0 of the OSRM API). 
By default this service is the OSRM public API (http://router.project-osrm.org/). To change the OSRM server, change the osrm.server option:
`options(osrm.server = "http://address.of.the.server/")`

## Features

* `osrmTable` Get travel time matrices between points.

* `osrmRoute` Get the shortest path between two points.

* `osrmTrip` Get the travel geometry between multiple unordered points.

* `osrmIsochrone` Get a SpatialPolygonsDataFrame of isochrones.


## Demo

### `osrmTable`

```r
library(osrm)
# Load data
data("com")
# Travel time matrix
distCom <- osrmTable(loc = com[1:50, c("comm_id","lon","lat")])
# First 5 rows and columns
distCom$durattion[1:5,1:5]
```
<table border=1>
<tr> <th>  </th> <th> FR3162292119 </th> <th> FR3162212035 </th> <th> BE241011 </th> <th> BE241024 </th> <th> FR3162270520 </th>  </tr>
  <tr> <td align="right"> FR3162292119 </td> <td align="right"> 0.0 </td> <td align="right"> 5.4 </td> <td align="right"> 95.1 </td> <td align="right"> 91.6 </td> <td align="right"> 7.5 </td> </tr>
  <tr> <td align="right"> FR3162212035 </td> <td align="right"> 4.9 </td> <td align="right"> 0.0 </td> <td align="right"> 98.3 </td> <td align="right"> 94.7 </td> <td align="right"> 7.1 </td> </tr>
  <tr> <td align="right"> BE241011 </td> <td align="right"> 94.2 </td> <td align="right"> 97.3 </td> <td align="right"> 0.0 </td> <td align="right"> 10.4 </td> <td align="right"> 93.5 </td> </tr>
  <tr> <td align="right"> BE241024 </td> <td align="right"> 90.8 </td> <td align="right"> 93.8 </td> <td align="right"> 10.4 </td> <td align="right"> 0.0 </td> <td align="right"> 90.0 </td> </tr>
  <tr> <td align="right"> FR3162270520 </td> <td align="right"> 7.0 </td> <td align="right"> 6.9 </td> <td align="right"> 93.5 </td> <td align="right"> 90.0 </td> <td align="right"> 0.0 </td> </tr>
</table>


### `osrmRoute`

```r
library(osrm)
# Load data
data("com")

# Travel path between SpatialPointsDataFrame
route <- osrmRoute(src = src[1,], 
                   dst = dst[1,],
                   sp = TRUE)
plot(route, lty = 2, lwd = 2)
plot(src[1,], pch = 20, col = "green", cex = 3, add = TRUE)             
plot(dst[1,], pch = 20, col = "red", cex = 3, add = TRUE) 
```
![](http://rgeomatic.hypotheses.org/files/2016/04/viageom.png)


### `osrmTrip`

```r
library(osrm)
# Load data
data("com")

# Get a trip with a SpatialPointsDataFrame
trips <- osrmTripGeom(loc = src)

# Map
if(require("cartography")){
  osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE)
  tilesLayer(osm)
  plot(src, pch = 20, col = "red", cex = 2, add = TRUE)
  plot(trips[[1]]$trip, add = TRUE, lwd=2)
}

```

![](http://rgeomatic.hypotheses.org/files/2016/04/trips.png)

### `osrmIsochrone`

```r
library(osrm)
# Load data
data("com")

# Get isochones with a SpatialPointsDataFrame, custom breaks
iso <- osrmIsochrone(loc = src[6,], breaks = seq(from = 0,to = 30, by = 5))

# Map
if(require("cartography")){
  osm <- getTiles(spdf = iso, crop = TRUE)
  tilesLayer(osm)
  breaks <- sort(c(unique(iso$min), max(iso$max)))
  pal <- paste(carto.pal("taupe.pal", length(breaks)-1), "95", sep="")
  
  cartography::choroLayer(spdf = iso, df = iso@data,
                          var = "center", breaks = breaks,
                          border = "grey50", col = pal,
                          legend.pos = "topleft",legend.frame = TRUE, 
                          legend.title.txt = "Isochrones\n(min)", 
                          add = TRUE)
  plot(src[6,], add=T)
}
```
![](http://rgeomatic.hypotheses.org/files/2016/04/iso.png)


## Installation

* Development version on GitHub
```{r}
require(devtools)
devtools::install_github("rCarto/osrm")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=osrm/)
```{r}
install.packages("osrm")
```

