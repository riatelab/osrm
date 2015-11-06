#' @name osrmTableOD
#' @title Get Travel Time Matrices Between Origins and Destinations
#' @description Build and send OSRM API queries to get travel time matrices between set of origin points and set of destination points
#' @param dfo data frame containing origin points identifiers, longitudes and latitudes 
#' (WGS84).
#' @param ido identifier field in dfo.
#' @param xo longitude field in dfo.
#' @param yo latitude field in dfo.
#' @param dfd data frame containing destination points identifiers, longitudes and latitudes 
#' (WGS84).
#' @param idd identifier field in dfd.
#' @param xd longitude field in dfd.
#' @param yd latitude field in dfd.
#' @param limit table query limit of osrm API in use. The default value (100) is 
#' the public API limit.
#' @return A matrix of time distances (in minutes) between origins and
#' destinations is returned.
#' @note This function is a wraper around osrmTable. Multiple calls allow to 
#' obtain a matrix between different sets of origins and destinations. A default delay is set between 
#' calls (1 sec) to be gentle with the public API. You can modify this delay if you use your own server: 
#' \code{options(osrm.delay = your_delay_value)}.  
#' @seealso \link{osrmTable}, \link{osrmTableErrors}
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' # Travel time matrix
#' distcom <- osrmTableOD(dfo = com[1:50,], ido = "comm_id", xo =  "lon", yo =  "lat", 
#'                        dfd = com[51:100,], idd = "comm_id", xd = "lon", yd = "lat")
#' # First 5 rows and columns
#' distcom[1:5,1:5]
#' }
#' @export
osrmTableOD <- function(dfo, ido, xo, yo, dfd, idd, xd, yd, limit = 100){
  osrmcourtesy <- getOption("osrm.delay")
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
          Sys.sleep(osrmcourtesy)
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