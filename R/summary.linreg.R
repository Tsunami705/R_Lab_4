#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param x the list
#' @returns summary
#' @export
#' 
summary.linreg <-
function(x){
  standarError=sqrt(x$variance_rc)
  sumn=data.frame(x$regressionsCoefficients,standarError,x$t_value,x$p_value)
  colnames(sumn)<-c("coefficients","standard error","t-value","p-value")
  print(sumn)
  cat("\n\nResidual standard error:",sqrt(x$residualVariance),"\nDegrees of freedom:",x$df,sep = "")
}
