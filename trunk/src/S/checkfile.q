"checkfile"<-
function(x, notdir = F, readable = F, writable = F, nonzero = F)
{
#   DATE WRITTEN:  1 June 1994             LAST REVISED:  29 April 1999
#   AUTHOR:  Scott D. Chasalow   (Scott.Chasalow@users.pv.wau.nl)
#   OS: Win NT
#
#   DESCRIPTION:
#         Test existence of a DOS file or files.  
#
#         Now calls file.exists() instead of dos call to dir, which dumped
#         under Splus 4.5 if file did not exist.  This version should work
#         equally well under both Splus 3.3 and 4.5 for Windows.
#        
	if(!is.character(x)) stop("x not a character object")
	if(length(x) > 1) {
		filen <- x
		x <- as.list(x)
		thisfun <- as.character(match.call())[1]
		result <- lapply(x, FUN = thisfun, notdir = notdir, readable = 
			readable, writable = writable, nonzero = nonzero)
		names(result) <- filen
		return(result)
	}
	result <- logical(0)
	attr(result, "exists") <- file.exists(x)
	result
}
