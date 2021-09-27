if ( requireNamespace("tinytest", quietly=TRUE) ){
  localtest <- FALSE
  home <- TRUE
  tinytest::test_package("osrm")
}
