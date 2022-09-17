#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param x the list
#' @returns ncoef
#' @export
#' 
coef.linreg <-
function(x){
  ncoef<-vector(length=length(x$regressionsCoefficients))
  nameCoef<-vector(length=length(x$regressionsCoefficients))
  for(i in 1:nrow(x$regressionsCoefficients)){
    ncoef[i]=x$regressionsCoefficients[i,1]
    nameCoef[i]=names(x$regressionsCoefficients[i,1])
  }
  names(ncoef)=nameCoef
  return(ncoef)
}
