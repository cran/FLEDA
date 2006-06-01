# rpay (relative proportion at age)

## class
setClass("rpay", contains="FLQuant")

setGeneric("rpay", function(object, ...){
	standardGeneric("rpay")
	}
)

setMethod("rpay", signature("FLQuant"), function(object, ...){

	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}
	pay <- pay(object)
	m <- apply(pay@.Data,c(1,3:5),function(x){x/mean(x, na.rm=TRUE)})
	rpay <- object
	rpay@.Data <- aperm(m, c(2,1,3:5))
	dimnames(rpay@.Data) <- dimnames(object@.Data)
	units(rpay) <- "%"
	new("rpay", rpay)

})
