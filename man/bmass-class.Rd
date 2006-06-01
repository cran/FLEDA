\name{bmass-class}
\docType{class}
\alias{bmass-class}
\alias{bmass}
\alias{bmass-methods}
\alias{bmass,FLStock-method}
\title{Class "bmass" for mature and immature biomass}

\description{
This class encapsulates results of a mature (\code{mb}) and immature (\code{ib}) biomass method.
}

\section{Creating objects from the class}{
Objects of this classe should be created with a call to the creator function \code{bmass(object, ...)}, where \code{object} must be of class \code{"FLStock"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{bmass}{
		Object of class \code{"FLQuants"} with a \code{mb} component and a \code{ib} component.
	}
}}

\section{Extends}{
Class \code{"FLQuants"}, directly.
}

\section{Methods}{
Inherits from \code{"FLQuants"}.
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers, \eqn{W_{ay}}, the mean weight and \eqn{M_{ay}}, the proportion of mature fish, at age \eqn{a=1,\dots,A} and year \eqn{y=1,\dots,Y} obtained e.g. from a \code{"FLStock"} object, the computations of \code{bmass} are:

\code{mb}:
\deqn{MB_{y}=\sum_{a}{C_{ay}*W_{ay}*M_{ay}}}

\code{ib}:
\deqn{IB_{y}=\sum_{a}{C_{ay}*W_{ay}*(1-M_{ay})}}

}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}, \code{\link{logr-class}}
}
\examples{
data(ple4)
# compute mature and immature biomass
ple4.bmass <- bmass(ple4)
# tune plot
ttl <- list(label="Trends in biomass for mature and immature plaice in IV", cex=1)
yttl <- list(label="relative biomass", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.8)
akey <- simpleKey(text=c("mature", "immature"), points=FALSE, lines=TRUE)
# plot
xyplot(data~year, data=ple4.bmass, type="l", main=ttl,
       key=akey, ylab=yttl, xlab=xttl, scales=ax)
}
\keyword{classes}
