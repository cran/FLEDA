\name{flc2flq-methods}
\docType{methods}
\alias{flc2flq-methods}
\alias{flc2flq}
\alias{flc2flq,FLCohort-method}
\title{Coerce "FLCohort" to "FLQuant"}

\description{
This method coerces \code{"FLCohort"} objects into \code{"FLQuant"} objects.
}

\section{Generic Function}{
\describe{
\item{Usage}{flc2flq(object, \dots)}
\item{Arguments:}{\ }
\describe{
	\item{\ }{\code{object}: \code{"FLCohort"} object}
	\item{\ }{\code{\dots}: additional, optional arguments if requested}
}
\item{Value}{\code{"FLQuant"} object}
}}

\section{Methods}{
\describe{
\item{object="FLCohort"}{Coerces \code{"FLCohort"} into \code{"FLQuant"}.}
}
}

\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link{FLCohort-class}}
}
\examples{
data(ple4)
flc <- FLCohort(catch.n(ple4))
flq <- flc2flq(flc)
all.equal(flq, catch.n(ple4))
}
\keyword{methods}
