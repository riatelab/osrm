#' @name osrmTableErrors
#' @title Query A Distance Matrix and Extract Errors
#' @description Query a distance matrix and extracts errors in origins and destination. 
#' @param mat A distance matrix.
#' @note An error is detected when at least half of distances are not found for an element.
#' @return A list of two vectors of identifier: originErrors and destinationErrors.
#' @export
osrmTableErrors <- function(mat){
  mat[!is.na(mat)] <- 0
  mat[is.na(mat)] <- 1
  x <- rowSums(mat)
  originErrors <- names(x[x>(ncol(mat)/2)])
  x <- colSums(mat)
  destinationErrors <- names(x[x>(nrow(mat)/2)])
  return(list(originErrors = originErrors,
              destinationErrors = destinationErrors))
}
