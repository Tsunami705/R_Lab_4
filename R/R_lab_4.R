linreg<-function(formula,data){
  
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

print.linreg<-function(x){
  cat("\nCall:\n",deparse(x$call),sep = "")
  cat("\n\nCoefficients:\n",sep = "")
  print.default(x$coefficients)
}

plot.linreg<-function(x){
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

resid.linreg<-function(x){
  return(x$residuals)
}

pred.linreg<-function(x){
  return(x$fittedValues)
}

coef.linreg<-function(x){
  ncoef<-vector(length=length(x$regressionsCoefficients))
  nameCoef<-vector(length=length(x$regressionsCoefficients))
  for(i in 1:nrow(x$regressionsCoefficients)){
    ncoef[i]=x$regressionsCoefficients[i,1]
    nameCoef[i]=names(x$regressionsCoefficients[i,1])
  }
  names(ncoef)=nameCoef
  return(ncoef)
}

summary.linreg<-function(x){
  standarError=sqrt(x$variance_rc)
  sumn=data.frame(x$regressionsCoefficients,standarError,x$t_value,x$p_value)
  colnames(sumn)<-c("coefficients","standard error","t-value","p-value")
  print(sumn)
  cat("\n\nResidual standard error:",sqrt(x$residualVariance),"\nDegrees of freedom:",x$df,sep = "")
}

