# $Id$

"print.ftree"<-
function(x, prefix = "", indent = 5, ...)
{
# DATE WRITTEN:  25 May 1995             LAST REVISED:  05 Oct 1997
# AUTHOR:  Scott Chasalow
#
# Adapted from print.list()
#
	n <- names(x)
	if(is.null(n))
		n <- rep("", length(x))
	ind <- nchar(prefix) + indent	#ind <- nchar(prefix) + 1 + max(nchar(n))
	for(i in seq(along = x)) {
		this <- if(n[i] == "") paste(prefix, "[[", i, "]]", sep = "")
			 else {
			nn <- .C("names_unlex",
				ans = n[i],
				as.integer(1))$ans
			paste(prefix, " ", nn, sep = "")
		}
		cat(this)
		if(length(x[[i]]))
			cat(":\n", sep = "")
		else cat("\n")
		print(x[[i]], prefix = paste(rep(" ", ind), collapse = ""), 
			indent = indent, ...)	#	cat("\n")
	}
	invisible(x)
}
