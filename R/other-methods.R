setMethod(catch, signature("FLIndex"), function(object){
	catch <- object@catch.n*object@catch.wt
	catch <- quantSums(catch)
	catch
})

