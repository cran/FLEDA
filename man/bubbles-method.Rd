\name{bubbles-method}
\docType{methods}
\alias{bubbles-method}
\alias{bubbles,formula,spay-method}
\alias{bubbles,formula,FLCohort-method}
\title{Bubbles plots for "spay" and "FLCohort" objects}
\description{
Implements \code{bubbles} plots for \code{"spay"} and \code{"FLCohort"} classes.
}
\section{Generic Function}{See \code{\link[FLCore]{bubbles-methods}}.}
\section{Methods}{
\describe{
\item{x="formula", data="spay"}{This method differs from the usual \code{bubbles} methods by using different colors for positive and negative values.}
\item{x="formula", data="FLCohort"}{This method implements the usual \code{bubbles} plot to a \code{FLCohort} object.}
}}
\seealso{
\code{\link{spay-class}}, \code{\link[FLCore]{bubbles-methods}}, \code{\link{lattice}}, \code{\link{FLCohort-class}}
}
\keyword{methods}
\examples{
data(ple4sex)
# compute standardized catch proportion at age
ple4sex.spay <- spay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Standardized catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.spay,  main=ttl, 
        ylab=yttl, xlab=xttl, scales=ax)
}
