#' #' @name osrmTableErrors
#' #' @title Detect Errors in Distance Matrices
#' #' @description Detect errors in distance matrices. 
#' #' @param mat A distance matrix.
#' #' @note An error is detected when at least half of distances are not found for an element.
#' #' @return A list of two vectors of identifier: originErrors and destinationErrors.
#' #' @seealso \link{osrmTable}
#' #' @examples 
#' #' \dontrun{
#' #' # Load data
#' #' data("com")
#' #' # Travel time matrix
#' #' distcom <- osrmTable(com[1:50,], id = "comm_id", x =  "lon",y =  "lat")
#' #' # First 5 rows and columns
#' #' distcom[1:5,1:5]
#' #' # Get errors
#' #' osrmTableErrors(distcom)
#' #' }
#' #' @export
#' osrmTableErrors <- function(mat){
#'   mat[!is.na(mat)] <- 0
#'   mat[is.na(mat)] <- 1a
#'   x <- rowSums(mat)
#'   originErrors <- names(x[x>(ncol(mat)/2)])
#'   x <- colSums(mat)
#'   destinationErrors <- names(x[x>(nrow(mat)/2)])
#'   return(list(originErrors = originErrors,
#'               destinationErrors = destinationErrors))
#' }
