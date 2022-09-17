#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param formula the formula
#' @param data dataframe
#' @returns a list including Regressions coefficients,The fitted values,The residuals,The degrees of freedom,The residual variance,The variance of the regression coefficients,The t-values for each coefficient and The p-values for each coefficient
#' @export
#' 
linreg <-
function(formula,data){
  
  X=model.matrix(formula,data)
  y=as.matrix(data[,all.vars(formula)[1]])
  
  cal<-match.call()
  calist<-list()
  class(calist)<-"linreg"
  
  calist$regressionsCoefficients=solve(t(X) %*% X) %*% t(X) %*% y
  calist$fittedValues=X %*% calist$regressionsCoefficients
  calist$residuals=y - calist$fittedValues
  n=nrow(X)
  p=ncol(X)
  calist$df=n-p
  calist$residualVariance=(t(calist$residuals)%*%calist$residuals)/calist$df
  calist$variance_rc=calist$residualVariance * (solve(t(X) %*% X))
  calist$t_value=calist$regressionsCoefficients/sqrt(calist$variance_rc)
  calist$p_value=pt(abs(calist$t_value),calist$df,lower.tail=FALSE)
  calist$call=cal
  
  return(calist)
}
