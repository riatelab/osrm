# osrm R package

[![Version](http://www.r-pkg.org/badges/version/osrm)](https://CRAN.R-project.org/package=osrm/)
![](http://cranlogs.r-pkg.org/badges/osrm?color=brightgreen)
[![Travis-CI Build Status](https://travis-ci.org/rCarto/osrm.svg?branch=master)](https://travis-ci.org/rCarto/osrm)

***Interface Between R and the OpenStreetMap-Based Routing Service [OSRM](http://project-osrm.org/)***

![](https://f.hypotheses.org/wp-content/blogs.dir/1909/files/2016/02/Rroads.png)

## Description
OSRM is a routing service based on OpenStreetMap data. See <http://project-osrm.org/> for more information. This package allows to compute distances (travel time and kilometric distance) between points and travel time matrices.   

This package relies on the usage of a running OSRM service (tested with v5.14.0 of OSRM).    

You can run your own instance of OSRM following guidelines provided here:    [https://github.com/Project-OSRM/osrm-backend](https://github.com/Project-OSRM/osrm-backend).     
The simplest solution is probably the one based on [docker containers](https://github.com/Project-OSRM/osrm-backend#using-docker).    

To set the OSRM server, use the `osrm.server` option: `options(osrm.server = "http://address.of.the.server/")`.     
To set the profile (driving is set by default), use the `osrm.profile` option: `options(osrm.profile = "name.of.the.profile")`.    



:warning: **You should be careful using the OSRM demo server, it is not always very stable** :bangbang: 




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
distCom <- osrmTable(loc = com[1:50, c("name","lon","lat")])
# First 5 rows and columns
distCom$duration[1:5,1:5]
```
<table border=1>
<tr> <th>  </th> <th> Bethune </th> <th> Annezin </th> <th> Denderleeuw </th> <th> Haaltert </th> <th> Locon </th>  </tr>
  <tr> <td align="right"> Bethune </td> <td align="right"> 0.00 </td> <td align="right"> 5.40 </td> <td align="right"> 95.10 </td> <td align="right"> 91.60 </td> <td align="right"> 7.50 </td> </tr>
  <tr> <td align="right"> Annezin </td> <td align="right"> 4.90 </td> <td align="right"> 0.00 </td> <td align="right"> 98.30 </td> <td align="right"> 94.70 </td> <td align="right"> 7.10 </td> </tr>
  <tr> <td align="right"> Denderleeuw </td> <td align="right"> 94.20 </td> <td align="right"> 97.30 </td> <td align="right"> 0.00 </td> <td align="right"> 10.40 </td> <td align="right"> 93.50 </td> </tr>
  <tr> <td align="right"> Haaltert </td> <td align="right"> 90.80 </td> <td align="right"> 93.80 </td> <td align="right"> 10.40 </td> <td align="right"> 0.00 </td> <td align="right"> 90.00 </td> </tr>
  <tr> <td align="right"> Locon </td> <td align="right"> 7.00 </td> <td align="right"> 6.90 </td> <td align="right"> 93.50 </td> <td align="right"> 90.00 </td> <td align="right"> 0.00 </td> </tr>
   </table>


### `osrmRoute`

```r
library(osrm)
# Load data
data("com")

# Travel path between SpatialPointsDataFrame
route <- osrmRoute(src = src[1,], dst = dst[1,], sp = TRUE)
if(require("cartography")){
  osm <- getTiles(spdf = route, crop = TRUE, type = "osmtransport")
  tilesLayer(osm)
  plot(route, lwd = 5, col = "blue", add = TRUE)
  plot(src[1,], pch = 20, col = "green", cex = 5, add = TRUE)             
  plot(dst[1,], pch = 20, col = "red", cex = 5, add = TRUE) 
  dev.off()
}
```
![](http://rgeomatic.hypotheses.org/files/2016/05/osrmRoute.png)


### `osrmTrip`

```r
library(osrm)
# Load data
data("com")

# Get a trip with a SpatialPointsDataFrame
trips <- osrmTrip(loc = src)

# Map
if(require("cartography")){
  osm <- getTiles(spdf = trips[[1]]$trip, crop = TRUE, type = "osmtransport")
  tilesLayer(osm)
  plot(trips[[1]]$trip, add = TRUE, col = 1:5, lwd = 5)
  plot(src, pch = 21, bg = "red", cex = 2, col = "black", add = TRUE)
}

```

![](http://rgeomatic.hypotheses.org/files/2016/05/osrmTrip.png)

### `osrmIsochrone`

```r
library(osrm)
# Load data
data("com")

# Get isochones with a SpatialPointsDataFrame, custom breaks
iso <- osrmIsochrone(loc = src[6,], breaks = seq(from = 0,to = 30, by = 5))

# Map
if(require("cartography")){
  osm <- getTiles(spdf = iso, crop = TRUE, type = "osmtransport")
  tilesLayer(osm)
  breaks <- sort(c(unique(iso$min), max(iso$max)))
  pal <- paste(carto.pal("taupe.pal", length(breaks)-1), "95", sep="")
  cartography::choroLayer(spdf = iso, df = iso@data,
                          var = "center", breaks = breaks,
                          border = "grey50", lwd = 0.5, col = pal,
                          legend.pos = "topleft",legend.frame = TRUE, 
                          legend.title.txt = "Driving Time\nto Renescure\n(min)", 
                          add = TRUE)
  plot(src[6,], cex = 2, pch = 20, col ="red", add=T)
  text(src[6,], label = "Renescure", pos = 3)
}
```
![](http://rgeomatic.hypotheses.org/files/2016/05/osrmIsochrone.png)


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

## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/rCarto/osrm/pulls) and report issues or ask questions [here](https://github.com/rCarto/osrm/issues).
