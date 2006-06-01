\name{pay-class}
\docType{class}
\alias{pay-class}
\alias{pay}
\alias{pay-methods}
\alias{pay,FLQuant-method}
\title{Class "pay" for proportion at age per year}

\description{This class encapsulates results of a \code{pay} method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{pay(object, ...)}, where \code{object} must be an object of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{pay}{object of class \code{"FLQuant"}.}
}}

\section{Extends}{
Class \code{"FLQuant"}, directly.
}

\section{Methods}{
Inherits from \code{"FLQuant"}.
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"} object, the computation of \code{pay} \eqn{P_{ay}} is:

\deqn{P_{ay}=\frac{C_{ay}}{\sum_a{C_{ay}}}}
}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}
}
\examples{
data(ple4sex)
# compute catch proportions at age
ple4sex.pay <- pay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.pay,  main=ttl, 
        ylab=yttl, xlab=xttl, scales=ax)
}
\keyword{classes}
