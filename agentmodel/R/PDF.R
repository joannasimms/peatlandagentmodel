#' Beta PDF function for a range greater than 0 to 1
#' 
#' y
#' a is alpha
#' b is beta
#' q is the 
#' p is 
#' y is the possible values
#'
#' This is used in the pop_and_geog function for farm type.
#' 
#' @export


PDF <- function(y, a, b, p, q) {
  PDF = ( (y-p)^(a-1) * (q - y)^(b-1) ) / ( (q - p)^(a+b-1) * beta(a,b) )
  return(PDF)
}
