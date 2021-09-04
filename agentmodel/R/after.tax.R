#' After Tax
#' 
#' works out the money let after tax.
#' TODO: check that the tax rate is right and what it should be applied to
#' 
#' @export


after.tax <- function(a) {
  # income tax in Finland is 20%, I think this is for businesses
  # https://www.oecd-ilibrary.org/sites/e9c6c9d1-en/index.html?itemId=/content/component/e9c6c9d1-en
  tax = 0.8*(a)
  return(tax)
}
