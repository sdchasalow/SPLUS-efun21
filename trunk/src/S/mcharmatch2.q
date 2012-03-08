"mcharmatch2"<-
function(x, table, merge = F)
{
#  DATE WRITTEN:  24 Feb 1995                LAST REVISED:  2 June 1995
#  AUTHOR:  Scott Chasalow
#
#  DESCRIPTION:
#	 If merge = F,  return a list with component i giving the indices 
#        of those elements of table containing the pattern given by x[i].
#        If merge = T, return the indices of those elements of table 
#        containing the pattern given by at least one of the regular 
#        expressions in x.
#
#  NOTE:
#        Should behave identically to function mcharmatch(),  but is 
#        faster when merge = T.  Uses unadvertised feature of grep that 
#        argument "pattern" (in this case, "x") may be a vector.
#
	if(merge) {
		sort(grep(x, table))
	}
	else {
		out <- lapply(as.list(x), grep, text = table)
		names(out) <- x
		out
	}
}
