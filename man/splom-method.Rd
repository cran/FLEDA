\name{splom-method}
\docType{methods}
\alias{splom-method}
\alias{splom,formula-method}
\alias{splom,formula,FLQuant-method}
\alias{splom,formula,FLCohort-method}
\title{Splom plots for FLQuants and FLCohorts}
\description{
Implements the \code{splom} method for \code{"FLQuant"} and \code{"FLCohort"} objects.
}

\section{Generic Function}{See \code{\link{splom}}.}

\section{Methods}{
\describe{
\item{x="formula", data="FLQuant"}{Usual splom method.}
\item{x="formula", data="FLCohort"}{Usual splom method.}
}
}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link{splom}}, \code{\link{FLCohort-class}}
}
\keyword{methods}
\examples{
data(ple4)
# fine tune plot
ttl <- list("BTS age pairwise plot for plaice in IV", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
splom(~data, data=catch.n(ple4), panel=pfun, pscales=0, main=ttl,
      xlab=xttl, ylab=yttl, pch=19, cex=0.3)
 
}
