#' @name osrmTableOD
#' @title Query OSRM to Get Time Distances Between Origines and Destinations
#' @description Query the OSRM API to get time distances marices between Origine
#'  and Destination points.
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
#' @param limit table query limit of osrm API in use.
#' @return A matrix of time distances (in minutes) between origins and
#' destinations is returned.
#' @export
osrmTableOD <- function(dfo, ido, xo, yo, dfd, idd, xd, yd, limit = 100){
#   luz_coords <- read.csv("/mnt/data/depot/osrm/inst/luz_coords.csv", stringsAsFactors=FALSE)
#   dfo = luz_coords[1:3,2:4]
#   dfd = luz_coords[16:30,2:4]
#   ido = "id" 
#   xo = "x"
#   yo = "y"
#   idd = "id"
#   xd = "x"
#   yd = "y"
#   limit = 4
#   k <- limit 
#   
  
  
  
  
  names(dfo[,c(ido, xo, yo)]) <- c("id", "x", "y")
  names(dfd[,c(idd, xd, yd)]) <- c("id", "x", "y")
  no <- nrow(dfo)
  nd <- nrow(dfd)
  mati <- matrix(data = NA, 
                 nrow = no, ncol = nd, 
                 dimnames = list(dfo[,ido], dfd[,idd]))
  
  # Within limits
  if((no + nd) <= limit){
    df <- rbind(dfo[,c(ido, xo, yo)], dfd[, c(idd, xd, yd)])
    xx <- osrmTable(df = df, id = "id", x = "x", y = "y" )  
    
    return(xx[row.names(mati), colnames(mati)])
  }
  

  ### Not many origins
  if((no <= k)){
    seqj <- seq(1, (nd %/% k)*k, k)
    for (j in seqj){

      df <- rbind(dfo[,c(ido, xo, yo)], dfd[j:(j + (k-1)), c(idd, xd, yd)])
      nrow(df)>k
      x <- osrmTable(df = df, id = "id", x = "x", y = "y" )  
      mati[rownames(mati) %in% rownames(x),colnames(mati) %in% colnames(x)] <- 
        x[rownames(x) %in% rownames(mati),colnames(x) %in% colnames(mati)] 
    }
    if (nd %% k != 0){
      df <- rbind(dfo[,c(ido, xo, yo)], dfd[(max(seqj) + k):nd,c(idd, xd, yd)])
      x <- osrmTable(df = df, id = "id", x = "x", y = "y" )  
      mati[rownames(mati) %in% rownames(x),colnames(mati) %in% colnames(x)] <- 
        x[rownames(x) %in% rownames(mati),colnames(x) %in% colnames(mati)] 
      
    }
    return(mati)
  }
  
  ### not many destinations
  if((nd <= k)){
    seqi <- seq(1, (no %/% k)*k, k)
    for (i in seqi){
      df <- rbind(dfo[i:(i + (k - 1)),c(ido, xo, yo)], dfd[,c(idd, xd, yd)])
      x <- osrmTable(df = df, id = "id", x = "x", y = "y" )  
      mati[rownames(mati) %in% rownames(x),colnames(mati) %in% colnames(x)] <- 
        x[rownames(x) %in% rownames(mati),colnames(x) %in% colnames(mati)] 
    }
    ## 3
    if (no %% k != 0 ){
      pts <- rbind(dfo[(max(seqi)+k) : no,], dfd[,c(idd, xd, yd)])
      x <- osrmTable(df = df, id = "id", x = "x", y = "y" )  
      mati[rownames(mati) %in% rownames(x),colnames(mati) %in% colnames(x)] <- 
        x[rownames(x) %in% rownames(mati),colnames(x) %in% colnames(mati)] 
    }
    return(mati)
  }
  
#   ### many origins and destinations
#   if((nd > k) & (no > k)){
#     seqi <- seq(1, (no %/% k)*k, k)
#     seqj <- seq(1, (nd %/% k)*k, k)
# 
#     ### 1
#     for (i in seqi){
#       for (j in seqj){
#         pts <- rbind(ptso[i:(i + (k - 1)),], ptsd[j:(j + (k-1)),])
#         x <- OSMDist(pts = pts, id = 'ID')
#         mati[i:(i + (k - 1)), j:(j + (k - 1))] <- x[1:k,(k+1):(2*k)]
#       }
# 
#     }
#     
#     ### 2
#     if (nd %% k != 0){
#       
#       for (i in seqi){
#         pts <- rbind(ptso[i:(i + (k - 1)),],
#                      ptsd[(max(seqj) + k):(max(seqj) + k + (nd %% k) - 1),])
#         x <- OSMDist(pts = pts, id = 'ID')
#         mati[i:(i + (k - 1)),
#              (max(seqj) + k):(max(seqj) + k + (nd %% k) - 1)] <-
#           x[1:k,(k+1):(k + (nd %% k))]
#       }
#     }
#     
#     ### 3
#     if (no %% k != 0 ){
#       for (j in seqj){
#         pts <- rbind(ptso[(max(seqi) + k):(max(seqi) + k + (no %% k) - 1),
#                           ],
#                      ptsd[j:(j + (k-1)),])
#         x <- OSMDist(pts = pts, id = 'ID')
#         mati[(max(seqi) + k):(max(seqi) + k + (no %% k) - 1),
#              j:(j + (k - 1))] <- x[1:(no %% k),((no %% k)+1):(((no %% k))+k) ]
#       }
#     }
#     
#     if ((no %% k != 0) & (no %% k != 0) ){
#       ### 4
#       pts <- rbind(ptso[(max(seqi) + k):(max(seqi) + k + (no %% k) - 1),
#                         ],
#                    ptsd[(max(seqj) + k):(max(seqj) + k + (nd %% k) - 1),])
#       x <- OSMDist(pts = pts, id = 'ID')
#       mati[(max(seqi) + k):(max(seqi) + k + (no %% k) - 1),
#            (max(seqj) + k):(max(seqj) + k + (nd %% k) - 1)] <-
#         x[1:(no %% k),((no %% k)+1):(((no %% k)+1) +(nd %% k) - 1) ]
#     }
# 
#   }
# 
#   return(mati)
}