# z (total catch)

## CLASS

setClass("z",
	representation(
		zy = "FLQuant",
		za = "FLQuant",
		zc = "FLCohort",
		agerng = "numeric")
)

setGeneric("z", function(object, ...){
	standardGeneric("z")
	}
)

setMethod("z", signature("FLQuant"), function(object, agerng="missing", ...){

# important note: agerng is defined by the dimnames so it should be
# the names of the ages not their position on the array

	if(!missing(...)){
		logr.obj <- logr(object, ...)
		logcc.obj <- logcc(object, ...)
	} else {
		logr.obj <- logr(object)
		logcc.obj <- logcc(object)
	}
	
	if(missing(agerng)){
		#try to find the full exploited ages by the age 
		# after which the logr are positive.
		agerngvec <- apply(logr.obj,1,function(x) mean(x<=0))
		agerng <- dimnames(logr.obj)[[1]][agerngvec<=0.1]

	}

	logr.obj <- trim(logr.obj, age=agerng)	
	logcc.obj <- trim(logcc.obj, age=agerng)	

	zy <- apply(logr.obj, c(2,3,4,5), mean) #, na.rm=TRUE)
	za <- apply(logr.obj, c(1,3,4,5), mean) #, na.rm=TRUE)
	dimnames(za)$year <- "all"
	
	# for cohort objects I need some hacking to make use of nice
	# flq's apply correction
	mat <- logcc.obj@.Data
	dimnames(mat) <- NULL
	flq <- FLQuant(mat)
	zc <- apply(flq, c(2,3,4,5), mean)

	# back to FLCohort and fix dimnames
	zc <- new("FLCohort", zc)
	dn <- dimnames(logcc.obj)
	dn$age <- "all"
	dn$cohort <- as.numeric(dn$cohort)-as.numeric(agerng[1])
	dimnames(zc) <- dn	

	# correct units
	units(zy) <- "year-1"
	units(za) <- "year-1"
	units(zc) <- "year-1"

	new("z", zy=zy, za=za, zc=zc, agerng=as.numeric(agerng))
	}
)

## METHODS

setMethod("summary", signature("z"), function(object){

	Zy <- object@zy@.Data
	Zc <- object@zc@.Data
	zybar <- mean(Zy, na.rm=TRUE)
	zcbar <- mean(Zc, na.rm=TRUE)
	zyvar <- var(Zy, na.rm=TRUE)
	zcvar <- var(Zc, na.rm=TRUE)
	df0 <- data.frame(Year=c(mean=zybar, var=zyvar), Cohort=c(mean=zcbar,var=zcvar))
	cat("Average Total Mortality\n")
	round(df0,3)
})

if (!isGeneric("t.test")) {
	setGeneric("t.test", function(x, ...){
		value <- standardGeneric("t.test")
		value
	})
}

setMethod("t.test", signature("z"), function(x){
	Zy <- x@zy@.Data
	Zc <- x@zc@.Data
	t.test(Zy, Zc)
})

