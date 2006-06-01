\name{plot-method}
\docType{methods}
\alias{plot-method}
\alias{plot,FLCohort-method}
\title{Plot for class "FLCohort"}

\description{
Implements a \code{plot} method for class \code{"FLCohort"}.
}

\section{Methods}{
\describe{
	\item{x="FLCohort"}{Plots \code{FLCohort} objects in a lattice of age by year.}
}}

\seealso{
  \code{\link{FLCohort-class}}, \code{\link{lattice}}
}

\keyword{methods}

\examples{
data(ple4)
flc <- FLCohort(catch.n(ple4))
plot(flc)
}
