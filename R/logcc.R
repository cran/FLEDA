# logcc (log catch curves)

## class

setClass("logcc", contains="FLCohort")

setGeneric("logcc", function(object, ...){
	standardGeneric("logcc")
	}
)

setMethod("logcc", signature("FLQuant"), function(object, ...){

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
	new("logcc", logr)

})
