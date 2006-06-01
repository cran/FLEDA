\name{as.data.frame-method}
\docType{methods}
\alias{cor-method}
\alias{cor,FLQuant,ANY-method}
\alias{cor,FLQuant,FLQuant-method}
\title{Correlation for class "FLQuant"}

\description{
Implements the \code{cor} method for class \code{"FLQuant"}.
}

\section{Methods}{
\describe{
	\item{x="FLQuant"}{This method computes correlation between ages (or whatever \code{quant} is).}
	\item{x="FLQuant",y="FLQuant"}{This method computes cross correlation of 2 \code{FLQuant} between ages (or whatever \code{quant} is).}
}}

\section{Details}{
The \code{cor} method for one \code{"FLQuant"} can be computed between the complete vector of observed values-at-quant or can be computed between the cohort adjusted vector. The choice is made by the argument \code{use}. The first method (defined by \code{use="all.obs"}) uses all observations of each two quant rows to compute \code{cor}. The second (\code{use="complete.obs"}, the default) uses only the observations of each two quant rows that have a matching cohort.

This kind of analysis is trying to give a measure of the data consistency along cohorts and if the observations that have no matching cohort are left in, the correlation coeficient is contaminated by them and is no longer a good indicator. 
}

\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link{cor}}
}

\keyword{methods}

\examples{
data(ple4)
c1 <- cor(catch.n(ple4), use="complete.obs", method="spearman")
round(c1,2)
c2 <- cor(catch.n(ple4), use="all.obs", method="spearman")
round(c2,2)
all.equal(c1,c2)
}
