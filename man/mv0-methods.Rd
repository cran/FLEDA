\name{mv0-methods}
\docType{methods}
\alias{mv0-methods}
\alias{mv0}
\alias{mv0,FLQuant-method}
\alias{mv0,FLQuants-method}
\title{Compute missing values and 0}

\description{This computes the number of missing observations \code{NA} and \code{0} values by year.}


\section{Generic Function}{
\describe{
\item{Usage}{mv0(object, \dots)}
\item{Arguments:}{\ }
\describe{
	\item{\ }{\code{object}: \code{"FLQuant"} or \code{"FLQuants"}}
	\item{\ }{\code{\dots}: additional, optional arguments if requested}
}
\item{Value}{\code{"FLQuant"} or \code{"FLQuants"}}
}}
\section{Methods}{
\describe{
\item{object="FLQuant"}{creates a \code{"FLQuant"} with 2 rows with the number of \code{NA} and number of \code{0}.}
\item{object="FLQuants"}{creates a \code{"FLQuants"} with several of the above \code{"FLQuant"}.}
}
}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{FLQuants-class}}
}
\keyword{methods}

\examples{
data(ple4.index)
mv0(index(ple4.index))
}
