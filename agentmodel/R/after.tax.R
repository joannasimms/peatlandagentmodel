#' After Tax
#' 
#' works out the money let after tax. According to OED tax is 20%.
#' 
#' @export


after.tax <- function(a) {
  tax = 0.8*(a)
  return(tax)
}
