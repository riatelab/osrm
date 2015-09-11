# #' @title OSMDistanceMatrix Package
# #' @name OSMDistanceMatrix
# #' @description Query OSRM to obtain time distance matrices in minutes. \cr
# #' The package relies on the usage of a running OSRM instance
# #' (located here: http://0.0.0.0:5000/)
# #' @docType package
# NULL
# 
# #' @name OSMDist
# #' @title Query OSRM to Get Time Distances
# #' @description Query the OSRM API to get time distances matrices between
# #' points.
# #' @param pts SpatialPointsDataFrame, WGS84 projection.
# #' @param id character, name of the id field in pts.
# #' @return A matrix of time distances (in minutes) is returned.
# #' @import sp
# #' @import RCurl
# #' @import RJSONIO
# #' @export
# OSMDist <- function(pts, id){
#   # coordinates extraction
#   origxy <- sp::coordinates(pts)
# 
#   # ids extraction
#   idpts <- pts@data[, id]
# 
#   # Query build
#   tab <- "http://0.0.0.0:5000/table?loc="
#   for (i in 1:nrow(origxy)){
#     if (i != nrow(origxy)){
#       tab <- paste(tab, origxy[i,2], ",", origxy[i,1], "&loc=", sep="")
#     }else{
#       tab <- paste(tab, origxy[i,2], ",", origxy[i,1], "&z=18", sep="")
#     }
#   }
# 
#   # Sending the query
#   tab2 <- RCurl::getURL(URLencode(tab))
# 
#   # JSON parsing
#   tab3 <- RJSONIO::fromJSON(tab2)
#   x <- unlist(tab3[[1]])
# 
#   # Conversion to matrix
#   mat <- matrix(data = unlist(tab3[[1]]),
#                 nrow = nrow(origxy),
#                 ncol = nrow(origxy),
#                 byrow = T,
#                 dimnames =  list(idpts, idpts))
# 
#   # From millisec to minutes
#   mat <- round(mat/(10*60), 1)
#   mat[mat==3579139.4] <- NA
#   return(mat)
# }
# 
# 
# 
# 
# 
# #' @name OSMDistOD
# #' @title Query OSRM to Get Time Distances Between Origines and Destinations
# #' @description Query the OSRM API to get time distances marices between Origine
# #'  and Destination points.
# #' @param ptso SpatialPointsDataFrame, origin points, WGS84 projection.
# #' @param ido character, origin points ids.
# #' @param ptsd SpatialPointsDataFrame, destination points, WGS84 projection.
# #' @param idd character, destination points ids.
# #' @param k numeric, dimension of the query (internal use mostly, see Details).
# #' @details OSRM server authorizes a certain query size for matrices
# #' (devel https://github.com/Project-OSRM). The default value, 1000, implies
# #' that the server is start with --max-table-size=1000 (or more than 1000).
# #'
# #' WGS84 pro4  : "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# #' @return A matrix of time distances (in minutes) between origins and
# #' destinations is returned.
# #' @export
# OSMDistOD <- function(ptso, ido, ptsd, idd, k){
# 
#   t1 <- Sys.time()
#   ptso <- ptso[, ido]
#   ptsd <- ptsd[, idd]
#   names(ptso) <- "ID"
#   names(ptsd) <- "ID"
#   no <- nrow(ptso)
#   nd <- nrow(ptsd)
# 
#   mati <- matrix(data = 0, nrow = no, ncol = nd, dimnames = list(ptso$ID,
#                                                                  ptsd$ID))
#   ### Small Matrix
#   if ((no <= k) & (nd <= k)){
#     pts <- rbind(ptso[], ptsd[])
#     x <- OSMDist(pts = pts, id = 'ID')
#     mati <- x[1:no,(no+1):(no+nd)]
#     cat(Sys.time()-t1)
#     return(mati)
#   }
# 
#   ### Not many origins
#   if((no <= k)){
#     seqj <- seq(1, (nd %/% k)*k, k)
#     for (j in seqj){
#       pts <- rbind(ptso, ptsd[j:(j + (k-1)),])
#       x <- OSMDist(pts = pts, id = 'ID')
#       mati[, j:(j + (k - 1))] <- x[1:(no %% k),((no %% k)+1):(((no %% k))+k) ]
#     }
#     ## 2
#     if (nd %% k != 0){
#       pts <- rbind(ptso, ptsd[(max(seqj) + k):nd,])
#       x <- OSMDist(pts = pts, id = 'ID')
#       mati[, (max(seqj) + k):
#              (max(seqj) + k + (nd %% k) - 1)] <-
#         x[1:(no %% k),
#           ((no %% k)+1):((no %% k)+(nd %% k))]
#     }
#   }
# 
#   ### not many destinations
#   if((nd <= k)){
#     seqi <- seq(1, (no %/% k)*k, k)
#     for (i in seqi){
#       pts <- rbind(ptso[i:(i + (k - 1)),], ptsd)
#       x <- OSMDist(pts = pts, id = 'ID')
#       mati[i:(i + (k - 1)),] <- x[1:k,(k+1):(k+nd)]
#     }
#     ## 3
#     if (no %% k != 0 ){
#       pts <- rbind(ptso[(max(seqi)+k) : no,], ptsd)
#       x <- OSMDist(pts = pts, id = 'ID')
#       mati[(max(seqi)+k) : no,] <-
#         x[1 : (no - (max(seqi)+k) + 1),
#           (no - (max(seqi)+k) + 2) : ((no - (max(seqi)+k) + 1) + nd)]
#     }
#   }
# 
#   ### many origins and destinations
#   if((nd > k) & (no > k)){
#     seqi <- seq(1, (no %/% k)*k, k)
#     seqj <- seq(1, (nd %/% k)*k, k)
#     pb <- txtProgressBar(min=0, max=max(seqi), style=3)
#     ### 1
#     for (i in seqi){
#       for (j in seqj){
#         pts <- rbind(ptso[i:(i + (k - 1)),], ptsd[j:(j + (k-1)),])
#         x <- OSMDist(pts = pts, id = 'ID')
#         mati[i:(i + (k - 1)), j:(j + (k - 1))] <- x[1:k,(k+1):(2*k)]
#       }
#       setTxtProgressBar(pb, i)
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
#     close(pb)
#   }
#   cat(Sys.time()-t1)
#   return(mati)
# }
# 
# 
# 
# #' @name OSMDistErrors
# #' @title Query A Distance Matrix and Extract Errors
# #' @description Query a distance matrix and extracts errors in origins and destination
# #' @param mat A distance matrix.
# #' @details An error is detected when at least half of distances are not found for an element.
# #' @return A list of two vectors of identifier: originErrors and destinationErrors.
# #' @export
# OSMDistErrors <- function(mat){
#   matna <- apply(X = mat, c(1,2), FUN = function(x){if(is.na(x)){1}else{0}})
#   x <- rowSums(matna)
#   originErrors <- names(x[x>(ncol(matna)/2)])
#   x <- colSums(matna)
#   destinationErrors <- names(x[x>(nrow(matna)/2)])
#   return(list(originErrors = originErrors,
#               destinationErrors = destinationErrors))
# }
