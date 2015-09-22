.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Data (c) OpenStreetMap contributors, ODbL 1.0. http://www.openstreetmap.org/copyright")
  packageStartupMessage("If you plan to use the OSRM public API, read the OSRM API Usage Policy:\nhttps://github.com/Project-OSRM/osrm-backend/wiki/Api-usage-policy")
  # options(osrm.server = "http://0.0.0.0:5000/")
  options(osrm.server = "http://router.project-osrm.org/")
  options(osrm.delay = 1)
  
}
