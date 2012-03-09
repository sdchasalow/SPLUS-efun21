# $Id$

"dlapply"<-
function(X, Y = X, FUN = c, ...)
{
#  DATE WRITTEN:  6 Feb 1995              LAST REVISED:  23 Mar 1998
#  AUTHOR:  Scott Chasalow
#  DESCRIPTION:
#        Dyadic lapply:  apply a function to corresponding components of two
#        lists.  Returns a list of length max(length(X), length(Y)).
#
	if(is.character(FUN)) FUN <- get(FUN, mode = "function") else if(mode(
		FUN) != "function") {
		farg <- substitute(FUN)
		if(mode(farg) == "name")
			FUN <- get(farg, mode = "function")
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	lenX <- length(X)
	lenY <- length(Y)
	n <- max(lenX, lenY)
	X <- rep(X, length = n)
	Y <- rep(Y, length = n)
	i <- as.list(seq(along = X))
	nms <- names(if(lenX >= lenY) X else Y)
	if(!is.recursive(X))
		X <- as.list(X)
	if(!is.recursive(Y))
		Y <- as.list(Y)
	out <- lapply(i, function(ii, XX, YY, FFUN, ...)
	FFUN(XX[[ii]], YY[[ii]], ...), XX = X, YY = Y, FFUN = FUN, ...)
	names(out) <- nms
	out
}
