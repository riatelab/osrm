#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description An interface between R and the OSRM API.\cr
#' OSRM is a routing
#' service based on OpenStreetMap data. See <http://project-osrm.org/> for more
#' information. This package
#' allows to compute distances (travel time and kilometric distance) between points
#' and travel time matrices.\cr
#' \itemize{
#' \item{\code{\link{osrmTable}} Get travel time matrices between points.}
#' \item{\code{\link{osrmRoute}} Get the shortest path between two points.}
#' \item{\code{\link{osrmTrip}} Get the travel geometry between multiple unordered points.}
#' \item{\code{\link{osrmIsochrone}} Get a SpatialPolygonsDataFrame of isochrones.}
#' }
#' 
#' @note
#' This package relies on the usage of a running OSRM service (tested with 
#' version 5.12.0 of the OSRM API).\cr
#' To change the OSRM server, change the \code{osrm.server} option:\cr 
#' \code{options(osrm.server = "http://address.of.the.server/")}. \cr\cr
#' To change the profile (driving is set by default), use the \code{osrm.profile} 
#' option:\cr
#' \code{options(osrm.profile = "name.of.the.profile")}
#' @docType package
NULL


#' @title Communes Coordinates
#' @name com
#' @description Coordinates of a set of communes in France. Coordinates are in WGS84.
#' @source UMS RIATE
#' @docType data
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France
#' @name src
#' @description 8 communes in France. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
NULL


#' @title SpatialPointsDataFrame of 10 Communes in France
#' @name dst
#' @description 10 communes in France. The projection is RGF93 / Lambert-93.
#' @source UMS RIATE
#' @docType data
NULL





