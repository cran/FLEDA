\name{spay-class}
\docType{class}
\alias{spay-class}
\alias{spay}
\alias{spay-methods}
\alias{spay,FLQuant-method}
\title{Class "spay" for standardised proportion at age per year}

\description{This class encapsulates results of a \code{spay} method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{spay(object, ...)}, where \code{object} must be an object of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{spay}{Object of class \code{"FLQuant"}.}
}}

\section{Extends}{
Class \code{"FLQuant"}, directly.
}

\section{Methods}{
Inherits from \code{"FLQuant"}.
\describe{
    \item{bubbles}{\code{signature(x="formula", data="spay")}: provides the bubbles plot with different colors for positive and negative values.}
}
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"} object, the computation of \code{spay} \eqn{P_{ay}^s} is:

Consider: 
\deqn{P_{ay}=\frac{C_{ay}}{\sum_a{C_{ay}}}}
and
\deqn{s_a = \sqrt{\frac{\sum{(P_{ay}-\bar{P}_a)^2}}{Y-1}}}
\deqn{P_{ay}^s=\frac{P_{ay}-\bar{P}_a}{s_a}}

}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}, \code{\link{pay-class}}
}
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
\keyword{classes}
