# splom
if (!isGeneric("splom")) {
	setGeneric("splom", useAsDefault = splom)
}

setMethod("splom", signature("formula", "FLQuant"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]

	# conditioning variables
	df <- expand.grid(dimnames(data)[2:5])
	v <- c()
	for(i in 1:3){
		if(length(unique(df[,i]))>1) v <- c(v,i)
	}
	# this is a hack to keep the names
	df <- data.frame(as.list(df)[v])
	
	# data
	df1 <- aperm(data, c(2,1,3,4,5))
	df1 <- apply(df1, c(3,4,5), function(z) data.frame(z))
	df1 <- do.call("rbind", df1)
	names(df1) <- dimnames(data)$age
	# all together now !!
	data <- cbind(df1, df)
	# we don't need years anymore
	data$year <- NULL


    lst$data <- data
	lst$x <- as.formula(deparse(x))
	do.call("splom", lst)
})

setMethod("splom", signature("formula", "FLCohort"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
	# conditioning variables
	df <- expand.grid(dimnames(data)[2:5])
	v <- c()
	for(i in 1:3){
		if(length(unique(df[,i]))>1) v <- c(v,i)
	}
	# this is a hack to keep the names
	df <- data.frame(as.list(df)[v])
	
	# data
	df1 <- aperm(data, c(2,1,3,4,5))
	df1 <- apply(df1, c(3,4,5), function(z) data.frame(z))
	df1 <- do.call("rbind", df1)
	names(df1) <- dimnames(data)$age
	# all together now !!
	data <- cbind(df1, df)
	# we don't need years anymore
	data$cohort <- NULL


    lst$data <- data
	lst$x <- as.formula(deparse(x))
	do.call("splom", lst)
})
