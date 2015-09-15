#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description An interface between R and the OSRM API. OSRM is a routing service 
#' based on OpenStreetMap data. A public API exists but one can run its own instance. 
#' The package allow to compute distance (travel time and kilometric distance) 
#' between points and travel time matrices.
#' The package relies on the usage of a running OSRM service.\cr
#'  
#' By default this service is the OSRM public API (\url{http://router.project-osrm.org/}).
#' 
#' To change the OSRM server, change the \code{osrm.server} option:\cr 
#' \code{options(osrm.server = "http://address.of.the.server/")}
#' @docType package
NULL



options(osrm.server = "http://router.project-osrm.org/")
t0 <- Sys.time()
route <- osrmViarouteGeom(yo=48.849697, xo=2.344238, yd=43.597095, xd=1.427691)
Sys.time()-t0

options(osrm.server = "http://0.0.0.0:5000/")
t0 <- Sys.time()
route <- osrmViarouteGeom(yo=48.849697, xo=2.344238, yd=43.597095, xd=1.427691)
Sys.time() - t0

plot(route[,2:1], asp = 1)

