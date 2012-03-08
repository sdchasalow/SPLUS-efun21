"mcharmatch"<-
function(x, table, merge = F)
{
#  DATE WRITTEN:  24 Feb 1995                LAST REVISED:  4 April 1995
#  AUTHOR:  Scott Chasalow
#
#  DESCRIPTION:
#	 If merge = F,  return a list with component i giving the indices 
#        of those elements of table containing the pattern given by x[i].
#        If merge = T, return the indices of those elements of table 
#        containing the pattern given by at least one of the regular 
#        expressions in x.
#
	out <- lapply(as.list(x), grep, text = table)
	if(merge)
		sort(unique(unlist(out)))
	else {
		names(out) <- x
		out
	}
}
