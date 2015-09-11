.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Data (c) OpenStreetMap contributors, ODbL 1.0. http://www.openstreetmap.org/copyright")
#   packageStartupMessage("Nominatim Usage Policy: http://wiki.openstreetmap.org/wiki/Nominatim_usage_policy")
#   packageStartupMessage("MapQuest Nominatim Terms of Use: http://info.mapquest.com/terms-of-use/")
  options(osrm.delay = 5)
  options(osrm.server = "http://0.0.0.0:5000/")
  options(osrm.delay = 0.5)
}
