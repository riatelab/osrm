#' @name osrmTable
#' @title Get Travel Time Matrices Between Points
#' @description Build and send OSRM API queries to get travel time matrices between points. 
#' This function interface the \emph{table} OSRM service. 
#' @param df data frame containing points identifiers, longitudes and latitudes 
#' (WGS84).
#' @param id identifier field in df.
#' @param x longitude field in df.
#' @param y latitude field in df.
#' @return x A square matrix of time travel (in minutes) is returned.
#' @note The public OSRM API does not allow more than 100 
#' locations in distance table query. \cr
#' If you use an other OSRM API service, make sure that 
#' more than 100 locations are allowed in table queries 
#' (i.e. the "max-table-size" argument, Max. locations supported in distance 
#' table query).
#' @seealso \link{osrmTableOD}, \link{osrmTableErrors}
#' @examples
#' \dontrun{ 
#' # Load data
#' data("com")
#' # Travel time matrix
#' distcom <- osrmTable(com[1:50,], id = "comm_id", x =  "lon",y =  "lat")
#' # First 5 rows and columns
#' distcom[1:5,1:5]
#' }
#' @export
osrmTable <- function(df, id, x, y){
  e <- simpleError("The public OSRM API does not allow more than 100 locations 
                   in a distance table query.")
  
  if(getOption("osrm.server")=="http://router.project-osrm.org/" & nrow(df)>100){
    stop(e)
  }
  
  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "table?loc=", sep = "")
    
    n <- nrow(df)
    for (i in 1:n){
      tab <- paste(tab, df[i,y], ",", df[i,x], "&loc=", sep="")
    }
    tab <- substr(x = tab, start = 1, stop = (nchar(tab)-5))

    # Sending the query
    tab2 <- RCurl::getURL(utils::URLencode(tab), useragent = "'osrm' R package")

    # JSON parsing
    # tab3 <- RJSONIO::fromJSON(content= tab2,default.size = 10)
    # Conversion to matrix
#     mat <- matrix(data = unlist( RJSONIO::fromJSON(content= tab2)[[1]]),
#                   nrow = n,
#                   ncol = n,
#                   byrow = T,
#                   dimnames =  list(df[,id], df[,id]))
    mat <- jsonlite::fromJSON(tab2)$distance_table

    dimnames(mat) <- list(df[,id], df[,id]) 
    
    # From millisec to minutes
    mat <- round(mat/(600), 1)

    # NA management
    mat[mat == 3579139.4] <- NA

    return(mat)
  }, error=function(e) { message("osrmTable function returns an error: \n", e)})
  return(NULL)
}


