\name{catch-method}
\docType{methods}
\alias{catch,FLIndex-method}
\title{Cacth method for FLIndex}
\description{
Computes total catch in weight using the mean weight at age and the catch at age. 
}

\section{Generic Function}{See \code{\link[FLCore]{catch}}.}

\section{Methods}{
\describe{
\item{object="FLIndex"}{Total catch in weight based on weights and numbers at age.  Yield would be a better name but to be consistent with "FLCore" we use catch.}
}
}
\seealso{
  \code{\link[FLCore]{FLIndex-class}}
}
\keyword{methods}
\examples{
data(ple4.index)
catch(ple4.index)
}
