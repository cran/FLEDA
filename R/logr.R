# logr (log ratio)

## class
setClass("logr", contains="FLQuant")

# logr (log ratio)
## OK !!

setGeneric("logr", function(object, ...){
	standardGeneric("logr")
	}
)

setMethod("logr", signature("FLQuant"), function(object, ...){

	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}

	dobj <- dim(object)
	flc <- FLCohort(object)
	dflc <- dim(flc)
	logr <- log(flc[-dobj[1],,,,,drop=FALSE]/flc[-1,,,,,drop=FALSE])
	logr <- logr[,-c(1, dflc[2]),,,,drop=FALSE]
	logr <- new("FLCohort", logr)
	logr <- flc2flq(logr)
	new("logr", logr)
})

