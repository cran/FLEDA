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


