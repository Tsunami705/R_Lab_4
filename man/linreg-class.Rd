% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R_lab_4_RC.R
\docType{class}
\name{linreg-class}
\alias{linreg-class}
\alias{linreg}
\title{linreg}
\arguments{
\item{formula}{formula}

\item{data}{a dataframe}
}
\value{
Returns an object of the class linreg
}
\description{
linreg
}

\examples{
 linreg_mod =linreg(Petal.Length~Sepal.Width, data=iris)
 linreg_mod$summary()
}
