# $Id$

"efun.record"<-
function(where = 1, which.stamp, silent = F, all.entries = F)
{
#   DATE WRITTEN:  19 Jun 1994            LAST REVISED:  26 Oct 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
#   DESCRIPTION:
#         Get an efun.record object (named .Efun.files).
#
	if(!is.atomic(where)) stop(
			"efun.rec needs an integer or a character string")
	if(!length(where))
		stop("Argument `where' has length zero")
	if(all.entries)
		which.stamp <- character(0)
	else {
		if(missing(which.stamp))
			which.stamp <- get.option("which.stamp", get.option(
				"stamp", character(0), "efun"), "efun")
		which.stamp <- switch(data.class(which.stamp),
			character = which.stamp,
			"function" = which.stamp(),
			expression = eval(which.stamp, local = F),
			name = eval(call("get", name = substitute(which.stamp)),
				local = F),
			format(which.stamp))
		if(!is.character(which.stamp))
			which.stamp <- format(which.stamp)
	}
	where <- where[1]
	if(exists(".Efun.files", where = where)) {
		x <- get(".Efun.files", where = where)
		if(class(x) != "efun.record")
			warning(paste("efun record does not have class", 
				"\"efun.record\""))
	}
	else {
		x <- character(0)
		if(!silent)
			cat("\nNo efun record in database,", if(is.character(
				where)) where else search()[where], "\n")
	}
	if(length(which.stamp) == 0 || length(x) == 0 || length(attr(x, 
		"stamps")) == 0)
		out <- x
	else {
#out <- x[attr(x, "stamps") == which.stamp]
#out <- x[!is.na(charmatch(attr(x,"stamps"),which.stamp))]
#out _ x[rev.charmatch(attr(x,"stamps"),which.stamp)] #slower
#out <- x[rev.charmatch2(attr(x, "stamps"), which.stamp)]	
		out <- x[mcharmatch2(which.stamp, attr(x, "stamps"), merge = T)
			]
	}
	if(!silent) {
		cat("\n(", length(x), "entries total in efun record )\n")
		cat("\n( which.stamp =", deparse(which.stamp), ")\n\n")
	}
	out
}
