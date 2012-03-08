"print.efun.record"<-
function(x, pager = options()$pager, ...)
{
#   DATE WRITTEN:  19 June 1994            LAST REVISED:  29 January 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS: Win 3.1
#
#   NOTE:  Uses modified version of page() that allows passing of arguments
#   to print().  If this is unavailable,  argument pager is ignored, and no 
#   paging is done.  Otherwise, argument "pager" is the same as argument 
#   "pager" to function page().
#
#   Paging is done only if the number of rows of output (including the
#   column labels) is greater than options()$length.
#
	if(length(x)) {
		x[x == ""] <- NA	# For less confusing printing
		nx <- names(x)
		st <- attr(x, "stamps")
		lab <- attr(x, "labels")
		lab[lab == ""] <- NA	# For less confusing printing
		out <- cbind(nx, x, st)
		col1lab <- format(c("FUNCTION", nx))[1]
		col2lab <- format(c("EDIT_FILE", x))[1]
		col3lab <- if(length(st)) format(c("STAMP", st))[1] else NULL
		rownames <- if(length(lab) == length(x)) lab else seq(along = x
				)
		dimnames(out) <- list(rownames, c(col1lab, col2lab, col3lab))
		if(length(pager) && any(names(page) == "...") && nrow(out) >= 
			options()$length)
			page(out, pager = pager, quote = F)
		else print(out, quote = F)
	}
	else {
		print(as.vector(unclass(x)))
	}
	invisible(x)
}
