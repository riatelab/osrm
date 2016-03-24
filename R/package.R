#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description An interface between R and the OSRM API. OSRM is a routing
#' service based on OpenStreetMap data. See <http://project-osrm.org/> for more
#' information. A public API exists but one can run its own instance. This package
#' allows to compute distances (travel time and kilometric distance) between points
#' and travel time matrices.\cr
#' This package relies on the usage of a running OSRM service (tested with 
#' version 4.9.0 of the OSRM API).\cr
#' By default this service is the OSRM public API (http://router.project-osrm.org/).
#' To change the OSRM server, change the \code{osrm.server} option:\cr 
#' \code{options(osrm.server = "http://address.of.the.server/")}. \cr
#' \itemize{
#' \item{\code{\link{osrmTable}} Get travel time matrices between points.}
#' \item{\code{\link{osrmViaroute}} Get travel time and travel distance between two points.}
#' \item{\code{\link{osrmViarouteGeom}} Get the travel geometry between two points.}
#' \item{\code{\link{osrmTripGeom}} Get the travel geometry between multiple unordered points.}
#' \item{\code{\link{osrmIsochrone}} Get a SpatialPolygonsDataFrame of isochrones.}
#' }
#' 
#' @docType package
NULL


#' @title Communes Coordinates
#' @name com
#' @description Coordinates of a set of communes in France, Belgium and Luxembourg. Coordinates are in WGS84.
#' @source UMS RIATE
#' @docType data
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France And Belgium
#' @name src
#' @description 10 communes in France & Belgium. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France
#' @name dst
#' @description 10 communes in France. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
NULL





