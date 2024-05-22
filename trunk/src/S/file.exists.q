# $Id$

"file.exists"<-
function(name)
{
# DATE WRITTEN:  10 Feb 1997 		 LAST REVISED:  29 Apr 1999
# AUTHOR:  Scott D Chasalow (Scott.Chasalow@cereon.com)
# OS:  Win NT
#
# DESCRIPTION:
#       Check if a DOS file exists
# 
# The old function, from Venables and Ripley (1994), pg 105, always gives T
# under my new NT 4.0 system, whether or not the file exists, 
# length(dos(paste("attrib", name))) > 0
# Therefore, use the following dos call, taken from my checkfile().  This is
# needed only for Splus 3.3.  Splus 4.5 has its own builtin file.exists().
#
	if(length(name) == 0) return(logical(0))
	if(nchar(name) == 0)
		return(F)
	exi <- dos(paste("dir '/B/L/A'", name), output.to.S = T, translate = T, 
		multi = F)
	length(exi) > 0
}
