# $Id$

"datefun.efun.record"<-
function(x, where = 1, ...)
{
# DATE WRITTEN:  04 Aug 1997 		 LAST REVISED:  26 Oct 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Method for generic function datefun() for objects of class 
#       "efun.record".
# 
	x <- names(x)
	NextMethod("datefun")
}
