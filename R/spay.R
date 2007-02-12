# spay (standardized proportion at age)

## class
setClass("spay", contains="FLQuant")

setGeneric("spay", function(object, ...){
	standardGeneric("spay")
	}
)

setMethod("spay", signature("FLQuant"), function(object, ...){

	if(!missing(...)){
		object <- trim(object, ...)
	} else {
		object <- object
	}
	pay <- pay(object)
	m <- apply(pay@.Data,c(1,3:5),function(x){(x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)})
	spay <- object
	spay@.Data <- aperm(m, c(2,1,3:5))
	dimnames(spay@.Data) <- dimnames(object@.Data)
	units(spay) <- "%"
	new("spay", spay)

})


## METHODS

# bubbles
setMethod("bubbles", signature(x="formula", data ="spay"), function(x, data, bub.scale=2.5, bub.col=gray(c(0.1, 0.9)), ...){
	dots <- list(...)
	data <- as.data.frame(data)
	# def col to plot negative values
	col <- as.numeric(data$data>=0)
	coln <- vector(mode="character", length=length(col))
	# color for negs
	coln[col==0] <- bub.col[1]
	# color for pos
	coln[col==1] <- bub.col[2]
	coln[coln==""] <- NA
	dots$col <- coln

	# data
	data$data <- abs(data$data)
	dots$data <- data
	
	# bubles size to be setted by panel.function
	dots$cex <- data$data
	dots$cex <- bub.scale*dots$cex/max(dots$cex, na.rm=TRUE)+0.1*(dots$cex+1)

	# panel.function
	dots$panel <- function(x,y,..., cex, subscripts){
		dots <- list(...)
		dots$pch=19
		call.list <- dots
		call.list$x <- x
		call.list$y <- y
		call.list$cex = cex[subscripts]
		call.list$col = dots$col[subscripts]

		ans <- do.call("panel.xyplot", call.list)
		ans

		call.list$col <- 1
		call.list$pch <- 1
		ans <- do.call("panel.xyplot", call.list)
		ans

	}
	# call.list
	call.list <- c(x = x, dots)
	# hugly hack to be replaced with the new version of lattice 
#	xyplot <- lattice::xyplot

	# plot
	ans <- do.call("xyplot", call.list)
	ans

})

