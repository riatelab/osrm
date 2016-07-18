.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Data (c) OpenStreetMap contributors, ODbL 1.0. http://www.openstreetmap.org/copyright")
  packageStartupMessage("Routes: OSRM. http://project-osrm.org/")
  packageStartupMessage("If you plan to use the OSRM public API, read the OSRM API Usage Policy:\nhttps://github.com/Project-OSRM/osrm-backend/wiki/Api-usage-policy")
  # options(osrm.server = "http://0.0.0.0:5000/")
  options(osrm.server = "http://router.project-osrm.org/")
  options(osrm.profile = "driving")
  # options(osrm.server = "http://api-osrm-routed-production.tilestream.net/")
}
