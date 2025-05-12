#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description An interface between R and the OSRM API.\cr
#' OSRM is a routing
#' service based on OpenStreetMap data. See <http://project-osrm.org/> for more
#' information. This package enables the computation of routes, trips, isochrones and
#' travel distances matrices (travel time and kilometric distance).\cr
#' \itemize{
#' \item{\code{\link{osrmTable}}: Build and send OSRM API queries to get travel
#' time matrices between points. This function interfaces the \emph{table}
#' OSRM service.}
#' \item{\code{\link{osrmRoute}}: Build and send an OSRM API query to get the
#' travel geometry between two points. This function interfaces with the
#' \emph{route} OSRM service.}
#' \item{\code{\link{osrmTrip}}: Build and send an OSRM API query to get the
#' shortest travel geometry between multiple unordered points. This function
#' interfaces the \emph{trip} OSRM service. Use this function to resolve the
#' travelling salesman problem.}
#' \item{\code{\link{osrmNearest}}: Build and send an OSRM API query to get the
#' nearest point on the street network. This function interfaces the
#' \emph{nearest} OSRM service.}
#' \item{\code{\link{osrmIsochrone}}: This function computes areas that are
#' reachable within a given time span from a point and returns the reachable
#' regions as polygons. These areas of equal travel time are called isochrones.}
#' \item{\code{\link{osrmIsodistance}}: This function computes areas that are
#' reachable within a given road distance from a point and returns the reachable
#' regions as polygons. These areas of equal travel distance are called
#' isodistances.}
#' }
#'
#'
#'
#'
#' @note
#' This package relies on the usage of a running OSRM service (tested with
#' version 6.0.0 of the OSRM API).\cr
#'
#'
#' To set the OSRM server, change the \code{osrm.server} option:\cr
#' \code{options(osrm.server = "http://address.of.the.server/")} \cr\cr
#' To set the profile, use the \code{osrm.profile} option:\cr
#' \code{options(osrm.profile = "name.of.the.profile")}\cr
#' The "car" profile is set by default. Other possible profiles are "foot" and "bike".\cr\cr
#'
#' A typical setup, corresponding to the Docker example, would be:\cr
#' \code{options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")}
#'
#'
#'
#'
#' The package ships a sample dataset of 100 random pharmacies in Berlin
#' (© OpenStreetMap contributors - \url{https://www.openstreetmap.org/copyright/en}).\cr
#' The sf dataset uses the projection WGS 84 / UTM zone 34N (EPSG:32634).\cr
#' The csv dataset uses WGS 84 (EPSG:4326).\cr
"_PACKAGE"
