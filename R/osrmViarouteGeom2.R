DecodeLineR <- function(encoded) {
  require(bitops)
  require(stringr)
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
  
  while(index <= len) {
    
    shift <- result <- 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift <- result <- b <- 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    
    array[df.index,] <- c(lat = lat * 1e-06, lng = lng * 1e-6)
    df.index <- df.index + 1
  }
  
  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}

osrmViarouteGeom2 <- function(xo, yo, xd, yd, sp = FALSE, ido = "start", idd = "end"){

  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "viaroute?loc=", sep = "")
    
    tab <- paste(tab, yo, ",", xo, "&loc=",yd,",",xd, 
                 "&alt=false&geometry=true",sep="")
    
    # Sending the query
    routgeom <- jsonlite::fromJSON(tab)$route_geometry

    geodf <- DecodeLineR(routgeom)
    if (sp==TRUE){
      if(!'package:sp' %in% search()){
        attachNamespace('sp')
      }
      routeLines <- sp::Lines(slinelist = sp::Line(geodf[,2:1]), 
                              ID = "x")
      routeSL <- sp::SpatialLines(LinesList = list(routeLines), 
                                  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(ido = ido, idd = idd)
      geodf <- sp::SpatialLinesDataFrame(routeSL, 
                                         data = df, 
                                         match.ID = FALSE)   
      row.names(geodf) <- paste(ido,idd,sep="_")
      
    }
    
    return(geodf)
  }, error=function(e) { message("osrmViarouteGeom function returns an error: \n", e)})
  return(NULL)
}