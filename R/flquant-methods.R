# FLQuant
if (!isGeneric("cor")) {
	setGeneric("cor", useAsDefault = cor)
}

setMethod("cor", signature("FLQuant"), function (x, y="missing", use = "pairwise.complete.obs", method = "pearson"){

	if(!is.na(pmatch(use, c("complete.obs","pairwise.complete.obs")))){
		x <- FLCohort(x)
	}
	x1 <- aperm(x, c(2,1,3,4,5))
	cx <- apply(x1, c(3,4,5), function(z){
		res <- cor(z, use=use, method=method)
		res[upper.tri(res)] <- NA
		data.frame(res)
	})

	dcx <- dim(x)
	dcx[2] <- dim(x)[1]
	dn <- dimnames(x)
	dn[2] <- dimnames(x)[1]
	names(dn)[2] <- quant(x)
	cx <- unlist(cx)
	cx <- array(cx, dim=dcx, dimnames=dn)
	cx

})

setMethod("cor", signature("FLQuant","FLQuant"), function (x, y="missing", use = "complete.obs", method = "pearson"){
	lst <- mcf(list(x,y))
	x1 <- lst[[1]]
	x2 <- lst[[2]]
	arr <- array(c(c(x1), c(x2)), dim=c(dim(x1),2))

	cx <- apply(arr, c(1,3,4,5), function(z){
		x0 <- z[,1]
		y0 <- z[,2]
		cor(x0, y0, use=use, method=method)
	})
	dn <- dimnames(x)
	dn[2] <- NULL
	dimnames(cx) <- dn
	cx
})

# missing values and 0

setGeneric("mv0", function(object, ...){
	standardGeneric("mv0")
	}
)

setMethod("mv0", signature("FLQuant"), function(object, ...){

	arr <- apply(object@.Data, c(2,3,4,5), function(x){
		c(sum(is.na(x)), sum(x==0, na.rm=TRUE))
	})
	dn <- dimnames(object)
	dn[[1]] <- c("NA","0")
	flq <- FLQuant(arr, dimnames=dn)
	quant(flq) <- "check"
	flq
})

# make compatible flquants

setGeneric("mcf", function(object, ...){
	standardGeneric("mcf")
	}
)

setMethod("mcf", signature("list"), function(object){

	# names 
	if(!is.null(object$names)){
		flqnames <- object$names
	} else {
		flqnames <- paste("v", 1:length(object), sep="")
	}
	# how many flquants exist ?
	v <- unlist(lapply(object, is.FLQuant))
	nflq <- sum(v)
	lst0 <- object[v]
	 
	# names and dim of the compatible flq
	dn <- dimnames(lst0[[1]])
	for(i in 1:nflq){
		dn1 <- dimnames(lst0[[i]])
		dn[[1]] <- as.character(sort(as.numeric(unique(c(dn[[1]],dn1[[1]])))))
		dn[[2]] <- as.character(sort(as.numeric(unique(c(dn[[2]],dn1[[2]])))))
		dn[[3]] <- unique(c(dn[[3]],dn1[[3]]))
		dn[[4]] <- unique(c(dn[[4]],dn1[[4]]))
		dn[[5]] <- unique(c(dn[[5]],dn1[[5]]))
	}
	dflq <- unlist(lapply(dn, length))	
	# new flquant
	flq <- FLQuant(dim=dflq, dimnames=dn)
	# preparing the list
	lst <- list()
	length(lst) <- nflq
	lst <- object
	names(lst) <- flqnames	

	# fulfiling the quants	
	for(j in 1:length(lst)){
		dn2 <- dimnames(lst[[j]])
		flq0 <- flq
		flq0[dn2[[1]], dn2[[2]], dn2[[3]], dn2[[4]], dn2[[5]]] <- lst[[j]]
		lst[[j]] <- flq0
	}
	
	# output
	FLQuants(lst)	
})

