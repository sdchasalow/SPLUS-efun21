# $Id$

"efun.purge"<-
function(which, where = 1, total = F)
{
#   DATE WRITTEN:  19 Jun 1994            LAST REVISED:  26 Oct 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
	eff <- if(total) efun.rec(where = where, which.stamp = NULL, silent = T
			) else efun.rec(where = where, which.stamp = NULL)
	db <- if(is.character(where)) where else search()[where]
	lene <- length(eff)
	if(!lene)
		stop(paste("Nothing to purge in database", db))
	if(total) {
		remove(".Efun.files", where = where)
		cat("\nRemoved efun record (.Efun.files) from", "database,\n", 
			db, "\n")
		return(invisible())
	}
	if(!is.numeric(which))
		stop("Argument `which' not numeric")
	if(!length(which))
		stop("Argument `which' has zero length")
	if(any(which <= 0))
		stop(paste("One or more elements of argument `which'", 
			"are not positive"))
	if(any(which > lene))
		stop(paste("One or more elements of argument `which'", 
			"are beyond bounds of efun record"))
	lab <- attr(eff, "labels")
	if(length(lab) == 0 || !all((1:lene) == lab))
		stop("Illegal labels attribute; cannot purge efun record")
	which <- unique(which)
	eff.new <- eff[ - which]
	attr(eff.new, "labels") <- as.character(seq(along = eff.new))
	if(length(eff.new)) {
		class(eff.new) <- class(eff)
		assign(".Efun.files", eff.new, where = where)
	}
	else Recall(where = where, total = T)
	cat("\nRemoved following functions from", "efun record in database\n", 
		db, ":\n")
	print(names(eff[which]))
	invisible(eff.new)
}
