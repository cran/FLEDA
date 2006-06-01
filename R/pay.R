# pay (proportion at age)

## class
setClass("pay", contains="FLQuant")

setGeneric("pay", function(object, ...){
	standardGeneric("pay")
	}
)

setMethod("pay", signature("FLQuant"), function(object, ...){

	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}

	m <- apply(object@.Data,c(2:5),function(x){x/sum(x, na.rm=TRUE)})
	pay <- object
	pay@.Data <- m
	dimnames(pay) <- dimnames(object)
	units(pay) <- "%"
	new("pay", pay)

})

