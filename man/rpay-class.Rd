\name{rpay-class}
\docType{class}
\alias{rpay-class}
\alias{rpay}
\alias{rpay-methods}
\alias{rpay,FLQuant-method}
\title{Class "rpay" for relative proportion at age per year}

\description{This class encapsulates results of a \code{rpay} method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{rpay(object, ...)}, where \code{object} must be an object of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{rpay}{Object of class \code{"FLQuant"}.}
}}

\section{Extends}{
Class \code{"FLQuant"}, directly.
}

\section{Methods}{
Inherits from \code{"FLQuant"}.
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"} object, the computation of \code{rpay} \eqn{P_{ay}^r} is:

Consider:

\deqn{P_{ay}=\frac{C_{ay}}{\sum_a{C_{ay}}}}

and 

\deqn{\bar{P}_{a}=\frac{\sum_y{P_{ay}}}{Y}}

\deqn{P_{ay}^r=\frac{P_{ay}}{\bar{P}_{a}}}

}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}, \code{\link{pay-class}}}
}
\examples{
data(ple4sex)
# compute relative catch proportion at age
ple4sex.rpay <- rpay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Relative catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
bubbles(age~year|unit, ple4sex.rpay,  main=ttl, 
        ylab=yttl, xlab=xttl, scales=ax)
}
\keyword{classes}
