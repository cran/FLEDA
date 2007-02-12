#=====================================================================
#
#! Class: FLCohort
#! Date: 31/05/2005
#! Version: 0.1_0
#! Author: EJ
#
#! ToDo: xyplot method
#
#! References (bibtex):
#
#! Notes: this was borroued from FLQuant code :-);
#! objects are built using FLQuants, for me it does not make sense
#! to build it directly.
#
#=====================================================================

setClass("FLCohort",
	representation("array", units="character"),
	prototype(array(NA, dim=c(1,1,1,1,1),
		dimnames=list(age="0", cohort="0", unit="unique", season="all", area="unique")),
		units="NA")
)

#! constructor

setGeneric("FLCohort", function(object, ...){
	standardGeneric("FLCohort")
	}
)

setMethod("FLCohort", signature("FLQuant"), function(object, ...){

	# reduce with trim
	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}

	# dimensions and co
	dobj <- dim(object)	
	dsobj <- dims(object) 
	dflc <- dobj
	ymin <- dsobj$minyear
	ymax <- dsobj$maxyear
	coh.min <- ymin - dsobj$min - dsobj[[1]] + 1
	coh.max <- ymax - dsobj$min
	dflc[2] <- length(coh.min:coh.max)

	# creating array flc
	flc <- array(NA, dim=dflc)
	coh.name <- as.character(coh.min:coh.max)
	dn.lst <- dimnames(object)
	dn.lst[[2]] <- coh.name
	names(dn.lst)[2] <- "cohort"
	dimnames(flc) <- dn.lst

	# creating the index
	m <- flc[,,1,1,1]
	lst <- split(1:ncol(object),1:ncol(object))
	lst <- lapply(lst, function(count){	
		paste("row(m)==(-col(m)+", dobj[1] + count, ")", sep="")
	})
	str <- paste(lst, collapse="|")
	ind <- eval(parse(text = str))
	flc.ind <- array(ind, dim=dflc)

	# feeding the array with terrible hack to feed by "diagonal"
	flc <- aperm(flc, c(2,1,3,4,5))
	flc.ind <- aperm(flc.ind, c(2,1,3,4,5))
	flq <- aperm(object@.Data, c(2,1,3,4,5))
	flc[flc.ind] <- flq
	flc <- aperm(flc, c(2,1,3,4,5))

	# et voilá
	new("FLCohort", flc, units=units(object))

})

#! FLCohort methods

# flc2flq
# this is a FLQuant creator method for FLCohorts 

setGeneric("flc2flq", function(object, ...){
	standardGeneric("flc2flq")
	}
)

setMethod("flc2flq", signature("FLCohort"), function(object, ...){

	# reduce with trim
	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}

	# dimensions and co
	ystart <- as.numeric(dimnames(object)$cohort[1])
	dobj <- dim(object)	
	dflq <- dobj
	dflq[2] <- dobj[2]-dobj[1]+1
	dnflq <- dimnames(object)
	dnflq[[2]] <- dnflq[[2]][-c(1:dobj[1]-1)]
	names(dnflq)[2]<-"year"

	# the new object
	flq <- array(NA, dim=dflq, dimnames=dnflq)
		
	# loop
	for(i in 1:dflq[1]){
		start <- dobj[1]-i+1
		end <- dobj[2]-i+1
		flq[i,,,,] <- object[i, start:end,,,]
	}
	
	# et voilá
	new("FLQuant", flq, units=units(object))

})


# trim

setMethod("trim", signature("FLCohort"), function(object, ...){

	args <- list(...)

	c1 <- args[[quant(object)]]
	c2 <- args[["cohort"]]
	c3 <- args[["unit"]]
	c4 <- args[["season"]]
	c5 <- args[["area"]]

	# check if the criteria is not larger then the objectect

	v <- c(length(c1), length(c2), length(c3), length(c4), length(c5))
	if(sum(v > dim(object))!=0){
		stop("\n  You're criteria are wider then the object dim. I don't know what to do !\n")
	}

	if(!is.null(c1)){
		v1 <- dimnames(object)[[1]] %in% c1
		object <- object[v1,,,,, drop=FALSE]
		}
	if(!is.null(c2)){
		v2 <- dimnames(object)[[2]] %in% c2
		object <- object[,v2,,,, drop=FALSE]
		}
	if(!is.null(c3)){
		v3 <- dimnames(object)[[3]] %in% c3
		object <- object[,,v3,,, drop=FALSE]
		}
	if(!is.null(c4)){
		v4 <- dimnames(object)[[4]] %in% c4
		object <- object[,,,v4,, drop=FALSE]
		}
	if(!is.null(c5)){
		v5 <- dimnames(object)[[5]] %in% c5
		object <- object[,,,,v5, drop=FALSE]
		}
	return(object)
})

# quant
setMethod("quant", signature(object="FLCohort"),
	function(object) {
		return(names(dimnames(object))[1])
	}
)

# units
setMethod("units", signature(object="FLCohort"),
	function(object)
		return(object@units)
)

setMethod("units<-", signature(object="FLCohort", value="character"),
	function(object, value) {
		if (!inherits(object, "FLCohort"))
			return(object)
		if (!is(value, "character"))
			stop("'units' must be a character string")
		object@units <- value
		return(object)
	}
) # }}}

# as.data.frame
setMethod("as.data.frame", signature(x="FLCohort"),
	function(x, row.names="missing", optional="missing"){
		df <- data.frame(expand.grid(quant=as.numeric(dimnames(x)[[1]]),
			cohort=as.numeric(dimnames(x)[[2]]),
			unit=dimnames(x)[[3]],
			season=dimnames(x)[[4]],
			area=dimnames(x)[[5]]),
			data=as.vector(x))
		names(df)[1] <- quant(x)
		return(df)
})

# plot

setMethod("plot", signature(x="FLCohort"),
	function(x, y="missing", ...){
		dots <- list(...)
		condnames <- names(dimnames(x)[c(3:5)][dim(x)[c(3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("*", cond)
		formula <- formula(paste("data~age|as.factor(cohort)", cond))
		dots$x <- formula
		dots$data <- x
		dots$ylab <- units(x)
		dots$xlab <- "age"
		dots$type <- c("l")	
		do.call("xyplot", dots)
#		xyplot(x=formula, data=obj, auto.key=T, type="l", ...)
	}
)

# [

setMethod("[", signature(x="FLCohort"),
	function(x, i, j, k, l, m, ..., drop=FALSE) {

		if (missing(i))
			i  <-  dimnames(x@.Data)[1][[1]]
		if (missing(j))
			j  <-  dimnames(x@.Data)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@.Data)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@.Data)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@.Data)[5][[1]]

   		if (drop==FALSE) {
	  		flc	 <- new("FLCohort", x@.Data[i, j, k, l, m, drop=FALSE])
			flc@units <- units(x)
#			quant(flc) <- quant(x)
		}
		else if(drop==TRUE)
             flc  <- x@.Data[i, j, k, l, m, ..., drop=TRUE]
   		return(flc)
	}
)   # }}}

# bubbles

setMethod("bubbles", signature(x="formula", data ="FLCohort"), function(x, data, bub.scale=2.5, ...){
	dots <- list(...)
	data <- as.data.frame(data)
	dots$data <- data
	dots$cex <- bub.scale*data$data/max(data$data, na.rm=TRUE)+0.1
	pfun <- function(x, y, ..., cex, subscripts){
		panel.xyplot(x, y, ..., cex = cex[subscripts])
		}
	call.list <- c(x = x, dots, panel=pfun)
	xyplot <- lattice::xyplot
	ans <- do.call("xyplot", call.list)
	ans$call <- match.call()
	ans

})

# ccplot

setGeneric("ccplot", function(x, data, ...){
	standardGeneric("ccplot")
	}
)


setMethod("ccplot", signature(x="formula", data ="FLCohort"), function(x, data, ...){

    dots <- list(...)
	# define a suitable xlim based on years
	if(all.vars(x)[2]=="year"){
		ys <- dimnames(data)$cohort[dim(data)[1]]
    	ye <- dimnames(data)$cohort[dim(data)[2]]
		xlim <- c(as.numeric(ys), as.numeric(ye)+2) 
	    dots$xlim <- xlim
	}
	# now data coerce
    data <- as.data.frame(data)
	# some options
    data$year <- data$cohort + data$age
    dots$data <- data
    dots$groups <- data$cohort
	# call & run
    call.list <- c(x = x, dots)
    xyplot <- lattice::xyplot
    ans <- do.call("xyplot", call.list)
    ans

})


# xyplot

setMethod("xyplot", signature("formula", "FLCohort"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
})
