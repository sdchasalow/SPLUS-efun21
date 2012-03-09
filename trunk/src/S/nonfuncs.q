# $Id$

"nonfuncs"<-
function(...)
{
# DATE WRITTEN:  25 May 1998      LAST REVISED:  26 May 1998
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
	ob <- objects(...)
	fu <- funcs(...)
	ob[match(ob, fu, nomatch = 0) == 0]
}
