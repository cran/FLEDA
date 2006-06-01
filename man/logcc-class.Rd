\name{logcc-class}
\docType{class}
\alias{logcc-class}
\alias{logcc}
\alias{logcc-methods}
\alias{logcc,FLQuant-method}
\title{Class "logcc" for log catch curves}

\description{This class encapsulates results of a log ratio at age per cohort method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{logcc(object, ...)}, where \code{object} must be an object of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{logcc}{Object of class FLCohort.}
}}

\section{Extends}{
Class \code{"FLCohort"}, directly.
}

\section{Methods}{
Inherits from \code{"FLCohort"}.
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"}, the computation of \code{logr} \eqn{L_{ay}} is:

\deqn{L_{ay}=log{\frac{C_{ay}}{C_{a+1,y+1}}}}

}

\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}, \code{\link{FLCohort-class}}
}

\examples{
data(ple4sex)
# compute logcc
ple4sex.cc <- logcc(ple4sex@catch.n)
# fine tune plot
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
ccplot(data~age|unit, data=ple4sex.cc, type="l", main=ttl, 
       ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1)
}

\keyword{classes}
