\name{logr-class}
\docType{class}
\alias{logr-class}
\alias{logr}
\alias{logr-methods}
\alias{logr,FLQuant-method}
\title{Class "logr" for log ratio at age per cohort}

\description{This class encapsulates results of a log ratio at age per method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{logr(object, ...)}, where \code{object} must be an object of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion. 
}

\section{Slots}{
\describe{
	\item{logr}{Object of class FLQuant.}
}}

\section{Extends}{
Class \code{"FLQuant"}, directly.
}

\section{Methods}{
Inherits from \code{"FLQuant"}.
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"} object, the computation of \code{logr} \eqn{L_{ay}} is:

\deqn{L_{ay}=log{\frac{C_{ay}}{C_{a+1,y+1}}}}

}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}
}
\examples{
require(FLEDA)
data(ple4)
myp <- logr(catch.n(ple4))
plot(myp)
# now trimming years
myp <- logr(catch.n(ple4), year=c(1990:2000))
plot(myp)
}
\keyword{classes}
