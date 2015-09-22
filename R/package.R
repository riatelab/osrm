#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description An interface between R and the OSRM API. OSRM is a routing service 
#' based on OpenStreetMap data. A public API exists but one can run its own instance. 
#' The package allow to compute distance (travel time and kilometric distance) 
#' between points and travel time matrices.\cr
#' The package relies on the usage of a running OSRM service.
#' By default this service is the OSRM public API (\url{http://router.project-osrm.org/}).
#' To change the OSRM server, change the \code{osrm.server} option:\cr 
#' \code{options(osrm.server = "http://address.of.the.server/")}
#' @docType package
NULL






