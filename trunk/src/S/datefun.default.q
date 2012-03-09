# $Id$

"datefun.default"<-
function(x, where = 1, ...)
{
# DATE WRITTEN:  04 Aug 1997 		 LAST REVISED:  15 Sep 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Apply function dataset.date() to each element of character vector x.
#       Note that x could include character strings giving the names of ANY
#       Splus objects, not just functions. (Hence the name is probably not
#       so good.)
# 
	if(length(x) == 0) return(character(0))
	n <- max(length(x), length(where))
	x <- rep(x, length = n)
	where <- rep(where, length = n)
	out <- character(n)
	for(i in seq(along = x)) {
		out[i] <- dataset.date(x[i], where = where[i], ...)
	}
	data.frame(FUNCTION = I(x), WHERE = where, DATASET.DATE = I(out))
}
