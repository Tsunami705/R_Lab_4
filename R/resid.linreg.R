#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param x the list
#' @returns residuals
#' @export
#' 
resid.linreg <-
function(x){
  return(x$residuals)
}
