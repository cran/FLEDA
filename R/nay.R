# nay (proportion at age)

## class
setClass("nay", contains="FLQuant")

setGeneric("nay", function(object, ...){
	standardGeneric("nay")
	}
)

setMethod("nay", signature("FLQuant"), function(object, ...){

	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}

	m <- apply(object@.Data,c(2:5),function(x){x/max(x, na.rm=TRUE)})
	nay <- object
	nay@.Data <- m
	dimnames(nay) <- dimnames(object)
	units(nay) <- "norm"
	new("nay", nay)

})

