# osrm R package

[![Version](http://www.r-pkg.org/badges/version/osrm)](https://CRAN.R-project.org/package=osrm/)
![](http://cranlogs.r-pkg.org/badges/osrm?color=brightgreen)
[![Travis-CI Build Status](https://travis-ci.org/rCarto/osrm.svg?branch=master)](https://travis-ci.org/rCarto/osrm)  

***Interface Between R and the OpenStreetMap-Based Routing Service [OSRM](http://project-osrm.org/)***

![](http://f.hypotheses.org/wp-content/blogs.dir/1909/files/2015/10/route1.png)

## Description
OSRM is a routing service based on OpenStreetMap data. See <http://project-osrm.org/> for more information. A public API exists but one can run its own instance. This package allows to compute distance (travel time and kilometric distance) between points and travel time matrices.

This package relies on the usage of a running OSRM service (tested with version 4.9.0 of the OSRM API). 
By default this service is the OSRM public API (http://router.project-osrm.org/). To change the OSRM server, change the osrm.server option:
`options(osrm.server = "http://address.of.the.server/")`

## Features

* `osrmTable` Get travel time matrices between points.

* `osrmViaRoute` Get travel time and travel distance between two points.

* `osrmViaRouteGeom` Get the travel geometry between two points.

* `osrmTripGeom` Get the travel geometry between multiple unordered points.

* `osrmIsochrone` Get a SpatialPolygonsDataFrame of isochrones.


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




