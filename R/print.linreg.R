#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param x the list
#' @returns coefficients
#' @export
#' 
print.linreg <-
function(x){
  cat("\nCall:\n",deparse(x$call),sep = "")
  cat("\n\nCoefficients:\n",sep = "")
  print.default(x$coefficients)
}
