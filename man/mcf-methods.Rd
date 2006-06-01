\name{mcf-methods}
\docType{methods}
\alias{mcf-methods}
\alias{mcf}
\alias{mcf,FLQuant-method}
\alias{mcf,list-method}
\title{Make compatible "FLQuant" objects}

\description{
This method gets a list of \code{"FLQuant"} and transforms their dims so that all of them will have the same dimensions, including \code{"NA"} for missing values, and outputs a \code{"FLQuants"} object.
}

\section{Generic Function}{
\describe{
\item{Usage}{mcf(object, \dots)}
\item{Arguments:}{\ }
\describe{
	\item{\ }{\code{object}: list of \code{"FLQuant"} objects}
	\item{\ }{\code{\dots}: additional, optional arguments if requested}
}
\item{Value}{\code{"FLQuants"} object}
}}

\section{Methods}{
\describe{
\item{object="list"}{creates a \code{"FLQuants"} object from a list of \code{"FLQuant"} objects}
}
}

\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{FLQuants-class}}
}
\examples{
data(ple4)
mcf(list(catch.n(ple4), landings.n(ple4), discards.n(ple4)))
}
\keyword{methods}
