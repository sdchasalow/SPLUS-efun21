# $Id$

"dfun.efun.record"<-
function(x, where, verbose = T)
{
#   DATE WRITTEN:   05 Feb 1997            LAST REVISED:   25 Jun 1998
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS:  Win 3.1
#
#   DESCRIPTION:
#         Method for objects of class "efun.record" for the generic function
#         dfun.
#
#         Diff FUNctions with entries in the efun.record object x: compare
#	  ASCII and S-PLUS versions of function(s).
#
	if(missing(where)) {
		dfun(x = names(x), efun.record = x, the.call = match.call(), 
			verbose = verbose)
	}
	else dfun(x = names(x), where = where, efun.record = x, the.call = 
			match.call(), verbose = verbose)
}
