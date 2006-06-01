\name{FLCohort-class}
\docType{class}
\alias{FLCohort-class}
\alias{FLCohort}
\alias{FLCohort-methods}
\alias{FLCohort,FLQuant-method}
\alias{as.data.frame,FLCohort-method}
\alias{[,FLCohort-method}
\alias{quant,FLCohort-method}
\alias{trim,FLCohort-method}
\alias{units,FLCohort-method}
\alias{units<-,FLCohort,character-method}

\title{Class "FLCohort" for information by cohort}

\description{
This class encapsulates information by cohort or year-classes. It is very similar to \code{"FLQuant"} the major difference is the translation of the rows so that in each column (\code{year}) there will be a cohort or year-class.
}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the creator function \code{FLCohort(object, ...)}, where \code{object} must be of class \code{"FLQuant"} and \code{...} can be used to pass \code{trim} arguments to the creator funtion.}

\section{Slots}{
\describe{
	\item{\code{.Data}:}{\code{"array"} with age by year-class data.}
	\item{\code{units}:}{\code{"character"} with units.}
}}

\section{Methods}{
Type \code{showMethods(classes="FLCohort", where = "package:FLEDA", showEmprty=FALSE)} at the R prompt for a complete list of methods which are available for this class.

\describe{
\item{as.data.frame}{\code{signature(x = "FLCohort", row.names = "missing", optional = "missing")}: Coerces \code{"FLCohort"} into \code{"data.frames"}.}
\item{\[}{\code{signature(x = "FLCohort")}: Subseting for \code{"FLCohort"}.}
\item{bubbles}{\code{signature(x = "formula", data = "FLCohort")}: Bubbles plots.}
\item{ccplot}{\code{signature(x = "formula", data = "FLCohort")}: Catch curves plot.}
\item{flc2flq}{\code{signature(obj = "FLCohort")}: Coerces \code{"FLCohort"} into \code{"FLQUant"}.}
\item{quant}{\code{signature(object = "FLCohort")}: Extracts the \code{quant} dimension's name.}
\item{trim}{\code{signature(obj = "FLCohort")}: Subseting \code{"FLCohort"} based on dimnames.}
\item{units}{\code{signature(object = "FLCohort")}: Extracts the \code{units}.}
\item{units<-}{\code{signature(object = "FLCohort", value="character")}: Units replacement method.}
}

}
\seealso{
        \code{\link[FLCore]{FLQuant-class}}
}
\keyword{classes}
\examples{
data(ple4)
flc <- FLCohort(catch.n(ple4))
plot(flc)
}
