# bmass

## class
# Esta class está a estender a "list" via "FLQuants" mas não está a  
# herdar os names da lista ...

setClass("bmass", contains="FLQuants")

# constructor
setGeneric("bmass", function(object, ...){
	standardGeneric("bmass")
	}
)

setMethod("bmass", signature("FLStock"), function(object, ...){

	if(!missing(...)){
		catch.n <- trim(object@catch.n, ...)
		catch.wt <- trim(object@catch.wt, ...)
		mat <- trim(object@mat, ...)
	} else {
		catch.n <- object@catch.n
		catch.wt <- object@catch.wt
		mat <- object@mat
	}

	mb <- catch.n*catch.wt*mat
	mb <- apply(mb, c(2,3,4,5), sum, na.rm=TRUE)
	mb <- mb/mean(mb)
	
	ib <- catch.n*catch.wt*(1-mat)
	ib <- apply(ib, c(2,3,4,5), sum, na.rm=TRUE)
	ib <- ib/mean(ib)
		
#	new("bmass", FLQuants(list(ssb=ssb, ib=ib)))
	FLQuants(list(mb, ib))

	}
)


