#' @title Shortest Paths and Travel Time from OpenStreetMap via an OSRM API
#' @name  osrm
#' @description Query an OSRM service to obtain travel time between points in minutes. \cr
#' The package relies on the usage of a running OSRM instance
#' (located here: http://0.0.0.0:5000/)
#' @docType package
NULL


#' @name osrmTable
#' @title Query OSRM to Get a Travel Time Matrix
#' @description Query the OSRM API to get a travel time matrix between
#' points. This function interface the OSRM `table` service. 
#' @param df data frame containing points identifiers, longitudes and latitudes 
#' (WGS84 projection).
#' @param id identifier field in df.
#' @param x longitude field in df.
#' @param y latitude field in df.
#' @return x A square matrix of time travel (in minutes) is returned.
#' @note The public OSRM API does not allow more than 100 
#' locations in distance table query. If you use an other API, make sure that 
#' more than 100 locations are allowed (--max-table-size arg)   
#' -m [ --max-table-size ] arg (=100)  Max. locations supported in distance table query
#' @export
osrmTable <- function(df, id, x, y){
  e <- simpleError("
The public OSRM API does not allow more than 100 locations in a distance table query.")

  if(getOption("osrm.server")=="http://router.project-osrm.org/" & nrow(df)>100){
    stop(e)
  }
  
  tryCatch({
    # Query build
    tab <- paste(getOption("osrm.server"), "table?loc=", sep = "")
    tab
    n <- nrow(df)
    for (i in 1:n){
      tab <- paste(tab, df[i,y], ",", df[i,x], "&loc=", sep="")
    }
    tab <- substr(x = tab, start = 1, stop = (nchar(tab)-5))
    
    # Sending the query
    tab2 <- RCurl::getURL(URLencode(tab), useragent = "'osmdistance' R package")
    
    # JSON parsing
    tab3 <- RJSONIO::fromJSON(tab2)
    
    # Conversion to matrix
    mat <- matrix(data = unlist(tab3[[1]]),
                  nrow = n,
                  ncol = n,
                  byrow = T,
                  dimnames =  list(df[,id], df[,id]))
    
    # From millisec to minutes
    mat <- round(mat/(600), 1)
    
    # NA management
    mat[mat == 3579139.4] <- NA
    
    return(mat)
  }, error=function(e) { message("osrmTable function returns an error: \n", e)})
  return(NULL)
}


