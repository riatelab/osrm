#' @name osrmTableOD
#' @title Query OSRM to Get a Travel Time Matrix Between Origins and Destination
#' @description Query the OSRM API to get travel time matrices between a set of 
#' origin points and a set of destination points
#' @param dfo data frame containing origin points identifiers, longitudes and latitudes 
#' (WGS84 projection).
#' @param ido identifier field in dfo.
#' @param xo longitude field in dfo.
#' @param yo latitude field in dfo.
#' @param dfd data frame containing destination points identifiers, longitudes and latitudes 
#' (WGS84 projection).
#' @param idd identifier field in dfd.
#' @param xd longitude field in dfd.
#' @param yd latitude field in dfd.
#' @param limit table query limit of osrm API in use. The default value (100) is 
#' the public API limit.
#' @return A matrix of time distances (in minutes) between origins and
#' destinations is returned.
#' @note This function is a wraper around osrmTable. Multiple calls allow to 
#' obtain a matrix between different sets of origins and destinations. 
#' @export
osrmTableOD <- function(dfo, ido, xo, yo, dfd, idd, xd, yd, limit = 100){
  k <- limit 
  dfo <- dfo[,c(ido, xo, yo)]
  names(dfo) <- c("id", "x", "y")
  dfd <- dfd[,c(idd, xd, yd)]
  names(dfd) <- c("id", "x", "y")
  no <- nrow(dfo)
  nd <- nrow(dfd)
  mati <- matrix(data = 0, nrow = no, ncol = nd, dimnames = list(dfo$id, dfd$id))
  df <- unique(rbind(dfo,dfd))
  n <- nrow(df)
  # Within limits
  if(n <= k){
    xx <- osrmTable(df = df, id = "id", x = "x", y = "y" )
    mati <- xx[row.names(mati), colnames(mati)]
    return(mati)
  }else{
    # indice matrix
    k2 <- floor(k/2)
    seqi <- seq(1, (n %/% k2)*k2, (k2))
    seqi <- unique(c(seqi, max(seqi)+k2, (max(seqi)+k2) + (n %% k2)))
    x <- data.frame(i = seqi, bidon = 1)
    y <- merge(x, x, by = 'bidon')
    z <- reshape2::dcast(data = y,formula = i.x ~ i.y, mean, value.var = "bidon")
    row.names(z) <- z[,1]
    z <- z[,-1]
    z <- upper.tri(z, diag = FALSE)*z
    # queries
    for (i in (1:(nrow(z)-1))){
      for (j in 1:(ncol(z)-1)){
        if(z[i,j]==1){
          xx <- osrmTable(df = df[c(row.names(z[i,]):(as.numeric(row.names(z[i+1,]))-1), 
                                    (colnames(z)[j]):(as.numeric(colnames(z)[j+1])-1)), ], 
                          id = "id", x = "x", y = "y" )
          mati[rownames(mati) %in% rownames(xx),colnames(mati) %in% colnames(xx)] <- 
            xx[rownames(xx) %in% rownames(mati),colnames(xx) %in% colnames(mati)] 
        }
      }
    }
    return(mati)
  }
}