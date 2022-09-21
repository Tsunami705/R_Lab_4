#'linreg
#'
#' @param formula formula
#' @param data a dataframe
#'
#' @return Returns an object of the class linreg
#' @examples  linreg_mod =linreg(Petal.Length~Sepal.Width, data=iris)
#'  linreg_mod$summary()
#' @export
#'

linreg=setRefClass("linreg",
                   fields=list(
                     formula="formula",
                     data="data.frame",
                     dataName="character",
                     regressionsCoefficients="matrix",
                     fittedValues="matrix",
                     residuals="matrix",
                     df="numeric",
                     residualVariance="matrix",
                     variance_rc="matrix",
                     t_value="matrix",
                     p_value="matrix",
                     call="language",
                     sumn="data.frame"
                   ),
                   methods=list(
                      initialize=function(formula,data){
                        X=model.matrix(formula,data)
                        y=as.matrix(data[,all.vars(formula)[1]])
                        .self$dataName=deparse(substitute(data))
                        
                        .self$formula=formula
                        cal<-match.call()
                        
                        .self$regressionsCoefficients=solve(t(X) %*% X) %*% t(X) %*% y
                        .self$fittedValues=X %*% .self$regressionsCoefficients
                        .self$residuals=y - .self$fittedValues
                        .self$df=nrow(X)-ncol(X)
                        .self$residualVariance=(t(.self$residuals)%*%.self$residuals)/.self$df
                        .self$variance_rc=.self$residualVariance[1]*(solve(t(X) %*% X))
                        .self$t_value=.self$regressionsCoefficients/sqrt(diag(abs(.self$variance_rc)))
                        .self$p_value=pt(abs(.self$t_value),.self$df,lower.tail=FALSE)
                        .self$call=cal
                        .self$sumn=data.frame(.self$regressionsCoefficients,sqrt(diag(abs(.self$variance_rc))),.self$t_value,.self$p_value)
                        colnames(.self$sumn)<-c("coefficients","standard error","t-value","p-value")
                      },
                      #print the coefficients
                      print=function(){
                        cat("Call:\nlinreg(formula = ",format(.self$formula),", data = ",.self$dataName,")",sep = "")
                        cat("\n\nCoefficients:\n",sep = "")
                        print.default(format(coef()))
                      },
                      #plot the scatter plot of fitted values and residuals
                      plot=function(){
                        theme_set(theme_bw())
                        data1<-as.data.frame(cbind(.self$fittedValues,.self$residuals))
                        colnames(data1)=c("fittedValues","residuals")
                        standardizedResiduals=sqrt(abs(.self$residuals)/sqrt(.self$residualVariance[1,1]))
                        data2<-as.data.frame(cbind(.self$fittedValues,standardizedResiduals))
                        colnames(data2)=c("fittedValues","standardizedResiduals")
                        
                        p1<-ggplot(data=data1,aes(x=data1[,1],y=data1[,2]))  +
                          geom_point(size=3,shape=1) +
                          geom_smooth(aes(x=data1[,1],y=data1[,2]),
                                      method="lm",color="red",se=FALSE) +
                          xlab(paste("Fitted values\n","lm(",format(.self$formula),")")) +
                          ylab("Residuals") +
                          ggtitle("Residuals vs Fitted")
                        
                        p2<-ggplot(data=data2,aes(x=data2[,1],y=data2[,2]))  +
                          geom_point(size=3,shape=1) +
                          geom_smooth(aes(x=data2[,1],y=data2[,2]),
                                      method="lm",color="red",se=FALSE) +
                          xlab(paste("Fitted values\n","lm(",format(.self$formula),")")) +
                          ylab(expression(sqrt("|Standardized residuals|"))) +
                          ggtitle("Scale-Location")
                        
                        plotList=list(p1,p2)
                        grid.arrange(grobs = plotList)
                      },
                      #return the residual
                      resid=function(){
                        return(.self$residuals)
                      },
                      #return the fitted values
                      pred=function(){
                        return(.self$fittedValues)
                      },
                      coef=function(){
                        ncoef<-vector(length=length(.self$regressionsCoefficients))
                        nameCoef<-vector(length=length(.self$regressionsCoefficients))
                        lenn=nrow(.self$regressionsCoefficients)
                        for(i in 1:lenn){
                          ncoef[i]=.self$regressionsCoefficients[i,1]
                          nameCoef[i]=names(.self$regressionsCoefficients[i,1])
                        }
                        names(ncoef)=nameCoef
                        return(ncoef)
                      },
                      #Calculate the Summary and print it
                      summary=function(){
                        .self$sumn[,4]="***"
                        print.data.frame(.self$sumn,digits=3)
                        cat("\n\nResidual standard error: ",sqrt(.self$residualVariance)," on ",.self$df," degrees of freedom",sep = "")
                      }
                   )
)

#1
