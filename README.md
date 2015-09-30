# osrm R package
***Shortest Paths and Travel Time from OpenStreetMap via an OSRM API / Temps de trajet et matrices de distances entre points avec OpenStreetMap via une [API OSRM](http://project-osrm.org/)***

## Description
OSRM is a routing service based on OpenStreetMap data. See <http://project-osrm.org/> for more information. A public API exists but one can run its own instance. This package allows to compute distance (travel time and kilometric distance) between points and travel time matrices.

This package relies on the usage of a running OSRM service. By default this service is the OSRM public API (http://router.project-osrm.org/). To change the OSRM server, change the osrm.server option:
`options(osrm.server = "http://address.of.the.server/")`

## Features

* `osrmTable` Get travel time matrices between points.

* `osrmTableOD` Get travel time matrices between set of origin points and set of destination points.

* `osrmTableErrors` Detect errors in distance matrices.

* `osrmViaRoute` Get travel time and travel distance between two points.

* `osrmViaRouteGeom` Get the travel geometry between two points.

## Installation

### CRAN version
```{r}
install.packages("osrm")
```

### GitHub version :
```{r}
require(devtools)
devtools::install_github("rCarto/osrm")
```


