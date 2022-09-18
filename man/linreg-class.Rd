\name{linreg-class}
\Rdversion{1.1}
\docType{class}
\alias{linreg-class}
\alias{linreg}

\title{Class \code{"linreg"}}

\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\description{
just an R package lol

}
\examples{
showClass("linreg")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{formula}:}{Object of class \code{formula} ~~ }
    \item{\code{data}:}{Object of class \code{data.frame} ~~ }
    \item{\code{regressionsCoefficients}:}{Object of class \code{matrix} ~~ }
    \item{\code{fittedValues}:}{Object of class \code{matrix} ~~ }
    \item{\code{residuals}:}{Object of class \code{matrix} ~~ }
    \item{\code{df}:}{Object of class \code{numeric} ~~ }
    \item{\code{residualVariance}:}{Object of class \code{matrix} ~~ }
    \item{\code{variance_rc}:}{Object of class \code{matrix} ~~ }
    \item{\code{t_value}:}{Object of class \code{matrix} ~~ }
    \item{\code{p_value}:}{Object of class \code{matrix} ~~ }
    \item{\code{call}:}{Object of class \code{language} ~~ }
    \item{\code{sumn}:}{Object of class \code{data.frame} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{summary()}:}{ ~~ }
    \item{\code{coef()}:}{ ~~ }
    \item{\code{pred()}:}{ ~~ }
    \item{\code{resid()}:}{ ~~ }
    \item{\code{plot()}:}{ ~~ }
    \item{\code{print()}:}{ ~~ }
    \item{\code{initialize(formula, data)}:}{ ~~ }
  }
}
