#' A wrapper around the read.csv function allowing for creating factors on the fly
#'
#' @param pathname The path to your csv file.
#' @param sep The column separator, default is `,`.
#' @param dec the decimal separator, default is `.`.
#' @param create_factors If `TRUE`, all columns will be returned as factorial, default is `TRUE`.
#' @param show_structure Shows the structure of the final data via `str()`.
#'
#' @return A data frame with factors if the input data was non-numbers (integers or floats).
#'
#' @export
#'
#' @examples
#'
#' # no example yet
#'


read_data <- function(pathname,
                      sep = ",",
                      dec = ".",
                      create_factors = T,
                      show_structure = F){

  ds <- read.csv(pathname, sep = sep, dec = dec)

  if(create_factors == TRUE){
    for(i in 1:ncol(ds)){
      if(class(ds[,i]) == "character"){
        ds[,i] <- as.factor(ds[,i])
      }
    }
  }

  if(show_structure == TRUE) str(ds)

  return(ds)
}
