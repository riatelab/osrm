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
#' version 5.17.0 of the OSRM API).\cr
#' To change the OSRM server, change the \code{osrm.server} option:\cr 
#' \code{options(osrm.server = "http://address.of.the.server/")}. \cr\cr
#' To change the profile (driving is set by default), use the \code{osrm.profile} 
#' option:\cr
#' \code{options(osrm.profile = "name.of.the.profile")}
#' @docType package
NULL





#' @title SpatialPointsDataFrame of 100 Random Pharmacies in Berlin
#' @name apotheke.sp
#' @description 100 random pharmacies in Berlin. 
#' The projection is WGS 84 / UTM zone 34N.
#' @source © OpenStreetMap contributors - \url{https://www.openstreetmap.org/copyright/en}.
#' @docType data
NULL

#' @title Coordinates of 100 Random Pharmacies in Berlin
#' @name apotheke.df
#' @description A data.frame of coordinates of 100 random pharmacies in Berlin. 
#' The projection is WGS 84.
#' @source © OpenStreetMap contributors - \url{https://www.openstreetmap.org/copyright/en}.
#' @docType data
NULL
