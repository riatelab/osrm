---
title: 'osrm: Interface Between R and the OpenStreetMap-Based Routing Service OSRM'
tags:
  - R
  - OpenStreetMap
  - routing
  - road distance
  - spatial
authors:
  - name: Timothée Giraud^[Corresponding author] 
    orcid: 0000-0002-1932-3323
    affiliation: 1
affiliations:
 - name: Centre National de la Recherche Scientifique, France
   index: 1
date: 14 June 2022
bibliography: paper.bib
---

# Summary

`osrm` is an interface between R and the Open Source Routing Machine (OSRM) API. OSRM [@luxen-vetter-2011] is a routing service based on OpenStreetMap (OSM) data. This package allows to compute routes, trips, isochrones and travel distances matrices (travel times and kilometric distances) based on the OSM road network. 
It contains five functions that interface OSRM services:

- `osrmTable()` uses the *table* service to query time/distance matrices,
- `osrmRoute()` uses the *route* service to query routes,
- `osrmTrip()` uses the *trip* service to query trips,
- `osrmIsochone()` and `osrmIsometric()` use multiple `osrmTable()` calls to create isochrones or isometric polygons.

An instance of OSRM can be installed on a local or remote server, allowing free and heavy usage of the routing engine.



# Statement of need

Distance based computations and models are at the core of many spatial analysis operations in various scientific fields.
The simplest distance metric is the Euclidean distance (or distance as the crow flies), it is easy and inexpensive to compute. The use of this simple metric may be well fitted to study some phenomenon, such as species distribution or pollution diffusion. 
But whenever research aims at studying human activities (transport of persons or goods for example) it is common to use more realistic metrics based on road distance or travel time.

To compute these metrics with R one has to use packages that interfaces routing engines. Most routing engines are commercial, use tokens to limit the number of requests, or heavily restrict the usage of derived datasets. 
The use of open source software based on open source data enables a high level of transparency useful to research works that aim at reproducibility. 

We argue that `osrm` offers such a level of transparency by relying on the open source software OSRM, which itself uses the open data source OSM.  

`osrm` is already used in various fields such as
transport [@Danesi:2020; @Barroso_2021; @SAVARIA2021102964], 
education [@doi:10.1080/00131881.2017.1339285], 
health [@ijerph18073813; @CHEN2021126; @wisch2022naturalistic; @SNYDER2022102526], 
applied geography [@doi:10.1080/23754931.2018.1519458; @Kandlbinder; @doi:10.1080/23754931.2021.1895875; @Oberst2021hedonischer], 
environmental science [@https://doi.org/10.1111/cobi.13326; @WALKER2021127097], 
urban planning [@XU2019103452; @dey2022urban; @doi:10.1177/23998083211040519] 
and linguistics [@ijgi8090400].  

The use of `osrm` is also suggested by the package for sustainable transport planning `stplanr` [@stplanr]


# State of the field

Several packages exist to compute routes, trips or distance matrices. Most of them rely on commercial and non-free software and use non-free data. See for example `hereR` [@hereR] that uses here services, `gmapsdistance` [@gmapsdistance], `googleway` [@googleway] and `mapsapi` [@mapsapi] that use Google Maps Platform or `mapboxapi` [@mapboxapi] that relies on Mapbox. Using these packages imposes many restrictions on data extraction, exploitation and sharing. Other packages use open source routing engines and open data: `graphhopper` [@graphhopper] uses GraphHopper, `opentripplanner`[@openttripplanner] uses OpenTripPlanner, `valhallr` [@val] uses Valhalla. Among these packages `osrm` has the advantage of using OSRM which is easy to install and to run on a local or remote server.


# References
