\name{z-class}
\docType{class}
\alias{z-class}
\alias{z}
\alias{z-methods}
\alias{z,FLQuant-method}
\alias{summary,z-method}
\alias{t.test,z-method}
\title{Class "z" for total mortality}

\description{This class encapsulates results of a \code{z} method.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{z(object, agerng, ...)}, where \code{object} must be an object of class \code{"FLQuant"}, \code{agerng} is the age range of fully expoited ages and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{zy}{Object of class \code{"FLQuant"}.}
	\item{za}{Object of class \code{"FLQuant"}.}
	\item{zc}{Object of class \code{"FLQuant"}.}
	\item{agerng}{Object of class \code{"numeric"}.}
}}

\section{Methods}{
\describe{
    \item{show}{\code{signature(object = "z")}: Display object briefly.}
    \item{summary}{\code{signature(object = "z")}: Displays the average total mortality computed by age and cohort.}
	\item{t.test}{\code{signature(object = "z")}: Tests the similarities between the estimates by year and by cohort.}
	\item{xyplot}{\code{signature(object = "z")}: Lattice \code{xyplot} method.}
}
}

\section{Details}{
Considering \eqn{C_{ay}}, the catch in numbers at age \eqn{a=1,\dots,A} per year \eqn{y=1,\dots,Y}, obtained e.g. from the catch.n slot of a \code{"FLStock"} object, the computation of \code{z} is performed by year \eqn{Z_{y}}, by age \eqn{Z_{a}} and by cohorts \eqn{Z_{c}}, for a defined age range (\code{agerng}):

Consider \eqn{a:a \in agerng}, \eqn{n=\#a} and

\deqn{L_{ay}=log{\frac{C_{ay}}{C_{a+1,y+1}}}}

than

\deqn{Z_{y}=\frac{\sum_{a}{L_{ay}}}{n}}

\deqn{Z_{a}=\frac{\sum_{y}{L_{ay}}}{Y}}

\deqn{Z_{c}=\frac{\sum_{c}{L_{ay}}}{n}}

If \code{agerng} is not provided, the method will try to guess this argument by finding the ages that have less than 10\% negative logs along the time series. This procedure may have unexpected side effects !  

}
\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{trim}}, \code{\link{logr-class}}
}
\examples{
data(ple4)
# compute Z
ple4z <- z(catch.n(ple4), agerng=3:6)
# tune plot
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
# plot z by age along years
xyplot(data~year, data=ple4z@zy, type="l", main=ttl, ylab=yttl, xlab=xttl)
# plot z by age along ages
xyplot(data~age, data=ple4z@za, type="l", main=ttl, ylab=yttl, xlab=xttl)
# plot z by cohort along years
xyplot(data~cohort, data=ple4z@zc, type="l", main=ttl, ylab=yttl, xlab=xttl)
}
\keyword{classes}
