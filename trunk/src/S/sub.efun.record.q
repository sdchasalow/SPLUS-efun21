# $Id$

"[.efun.record"<-
function(x, ...)
{
#   DATE WRITTEN:  10 Feb 1995          LAST REVISED:  24 Feb 1995
#   AUTHOR:  Scott D. Chasalow
#
	cx <- class(x)
	known <- c("names", "class", "stamps", "labels")
	anames <- names(attributes(x))
	extra <- is.na(match(anames, known))
	if(any(extra)) {
		dropped <- paste("\"", anames[extra], "\"", sep = "")
		lend <- length(dropped)
		if(lend > 1) {
			dropped[lend] <- paste("and", dropped[lend])
			word <- "attributes"
			if(lend > 2)
				dropped <- paste(dropped, collapse = ", ")
			else dropped <- paste(dropped, collapse = " ")
		}
		else word <- "attribute"
		warning(paste("Dropped", word, dropped))
	}
	class(x) <- NULL
	y <- x[...]
	st <- attr(x, "stamps")
	if(length(st)) {
		oldnms <- names(st)
		names(st) <- names(x)
		st <- st[...]
		names(st) <- oldnms
		attr(y, "stamps") <- st
	}
	lab <- attr(x, "labels")
	if(length(lab)) {
		oldnms <- names(lab)
		names(lab) <- names(x)
		lab <- lab[...]
		names(lab) <- oldnms
		attr(y, "labels") <- lab
	}
	class(y) <- cx
	y
}
