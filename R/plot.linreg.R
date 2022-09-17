#' linreg
#'
#' `linreg()`  return an object with of class linreg.
#'
#' @param x the list
#' @returns plotList
#' @export
#' 
plot.linreg <-
function(x){
  theme_set(theme_bw())
  data1<-as.data.frame(cbind(x$fittedValues,x$residuals))
  colnames(data1)=c("fittedValues","residuals")
  standardizedResiduals=sqrt(abs(x$residuals)/sqrt(x$residualVariance[1,1]))
  data2<-as.data.frame(cbind(x$fittedValues,standardizedResiduals))
  colnames(data2)=c("fittedValues","standardizedResiduals")
  
  p1<-ggplot(data=data1,aes(x=data1[,1],y=data1[,2])  +
    geom_point(shape=1,size=3) +
    geom_smooth(aes(x=data1[,1],y=data1[,2]),
      formula=y~x,method="lm",color="red",se=FALSE)) +
    xlab(paste("Fitted values\n","lm(",format(formula),")")) +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
    
  p2<-ggplot(data=data2,aes(x=data2[,1],y=data2[,2])  +
    geom_point(shape=1,size=3) +
    geom_smooth(aes(x=data1[,1],y=data1[,2]),
                  formula=y~x,method="lm",color="red",se=FALSE)) +
    xlab(paste("Fitted values\n","lm(",format(formula),")")) +
    ylab(expression(sqrt("|Standardized residuals|"))) +
    ggtitle("Scale-Location")
  
  plotList=list(p1,p2)
  grid.arrange(grobs = plotList)
}
