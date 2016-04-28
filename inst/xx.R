require(bitops)

enc_coord <- function(coord){
  output <- ""
  pt <- round(coord * 1e5, digits = 0) * 2
  if(coord < 0) pt <- bitops::bitFlip(pt)
  while(pt >= 0x20){
    output <- paste0(output, 
                     intToUtf8(bitops::bitOr(0x20, bitops::bitAnd(pt, 0x1f)) + 63))
    pt <- bitops::bitShiftR(pt, 5)
  }
  return(paste0(output, intToUtf8(pt + 63)))
}

encodeToPolyline <- function(df_coords){
  dims <- dim(df_coords)[1]
  output_string <- paste0(enc_coord(df_coords[1, 1]), enc_coord(df_coords[1, 2]))
  for(i in seq(2, dims)){
    output_string <- paste0(output_string,
                            enc_coord(df_coords[i, 1] - df_coords[i-1, 1]),
                            enc_coord(df_coords[i, 2] - df_coords[i-1, 2]))
  }
  return(output_string)
}

decodeFromPolyline <- function(encoded_polyline, factor = 1e5) {
  if(class(encoded_polyline) != "character") stop("Wrong encoded string format")
  
  len <- nchar(encoded_polyline)
  encoded_polyline <- strsplit(encoded_polyline, '')[[1]]
  
  idx <- 1
  res_idx <- 0
  lat = lon = 0
  changes <- list(NULL, NULL)
  pairs_points <- matrix(nrow = 0, ncol = 2)
  
  while(idx <= len) {
    for(u in c(1, 2)){
      shift = result = 0
      while(TRUE){
        byte <- as.integer(charToRaw(encoded_polyline[idx])) - 63
        result <- bitops::bitOr(result,
                                bitops::bitShiftL(bitops::bitAnd(byte, 0x1f), shift))
        idx <- idx + 1
        shift <- shift + 5
        if(byte < 0x20) break
      }
      changes[u] <- ifelse(bitops::bitAnd(result, 1),
                           -(result - (bitops::bitShiftR(result, 1))),
                           bitops::bitShiftR(result, 1))
    }
    lat <- lat + changes[[1]]
    lon <- lon + changes[[2]]
    res_idx <- res_idx + 1
    pairs_points <- rbind(pairs_points, c(lat, lon)/factor)
  }
  coords <- data.frame(pairs_points)
  names(coords) <- c('lat', 'lon')
  return(coords)
}




