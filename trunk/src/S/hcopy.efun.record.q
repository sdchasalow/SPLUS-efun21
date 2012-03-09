# $Id$

"hcopy.efun.record"<-
function(x, filename, verbose = T, print.it = T, unlink.it = print.it, ask = 
	print.it, ...)
{
#  DATE WRITTEN:  10 Apr 1995             LAST REVISED:  26 Oct 1997
#  AUTHOR:  Scott Chasalow
#
#  DESCRIPTION:
#        Method for objects of class "efun.record" for generic function
#        hcopy. Create a hardcopy of each function listed in the efun
#        record x, and optionally send to a printer. The Splus version
#        of a function is printed, NOT the ASCII version in the edit
#        file.
#
	funs <- names(x)
	n <- length(funs)
	out <- vector("list", n)
	if(!ask) {
		continue <- T
		cat("\n")
	}
	if(!missing(filename) && length(filename) != n)
		stop(paste("filename has length ", length(filename), 
			", should be ", n, sep = ""))
	done <- logical(n)
	for(i in 1:n) {
		if(ask) {
			cat("\n", funs[i], ":")
			cat("\n\tPress <Enter> to print,", 
				"any other key + <Enter>", "to skip function:")
			continue <- readline() == ""
		}
		if(verbose) {
			word <- if(continue) "Writing" else "Skipping"
			cat("\t", word, " function ", funs[i], " ...\n", sep = 
				"")
		}
		if(continue) {
			if(missing(filename))
				out[i] <- prfun(funs[i], print.it = print.it, 
				  unlink.it = unlink.it, ...)
			else out[i] <- prfun(funs[i], filename[i], print.it = 
				  print.it, unlink.it = unlink.it, ...)
			done[i] <- T
		}
	}
	cat("\n")
	n <- sum(as.numeric(done))
	cat("\nCreated", n, "files.", "\n")
	if(print.it)
		cat("Sent", n, "files to printer.", "\n")
	if(unlink.it)
		cat("Unlinked", n, "files.", "\n")
	out <- unlist(out[done])
	if(length(out) > 0) {
		names(out) <- funs[done]
		out
	}
	else invisible(out)
}
