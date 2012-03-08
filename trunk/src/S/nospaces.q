"nospaces"<-
function(x)
{
# DATE WRITTEN:  04 Aug 1997 		 LAST REVISED:  28 Sep 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Remove all white space from a character vector x. If x has length > 1, 
#       all white space is removed from each element and the elements are 
#       collapsed together with no space in between.
# 
	if(length(x) == 0) return(as.character(x))
	x <- x[x != ""]
	n <- nchar(x)
	first <- last <- unlist(lapply(n, seq))
	x <- rep(x, n)
	x <- substring(x, first, last)
	paste(x[!is.all.white(x)], collapse = "")
}
