if ( requireNamespace("tinytest", quietly=TRUE) ){
  localtest <- FALSE
  home <- FALSE
  tinytest::test_package("osrm")
}
