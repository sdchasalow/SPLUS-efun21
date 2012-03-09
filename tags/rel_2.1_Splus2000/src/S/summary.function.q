# $Id$

"summary.function"<-
function(x, where, where.calling = where, sortit.called = T)
{
#  DATE WRITTEN:  7 February 1995              LAST REVISED:  08 Oct 1997
#  AUTHOR:  Scott Chasalow
#
	x <- substitute(x)
	if(is.name(x))
		x <- as.character(x)
	else if(!is.character(x))
		stop(paste("Need a function name or a character string", 
			"giving a function name"))
	found <- eval(call("find", what = x, numeric. = T))
	if(length(found) == 0)
		stop(paste("function", x, "not found"))
	if(missing(where))
		where <- found[1]
	else where <- where[1]
	if(is.numeric(where)) {
		nwhere <- where
		cwhere <- search()[where]
	}
	else {
		cwhere <- where
		nwhere <- match(cwhere, search())
	}
	fun <- get(x, where = where)
	calling <- functions.calling(x, where.calling)
	out <- list(name = x, nwhere = nwhere, cwhere = cwhere, dataset.date = 
		dataset.date(x, nwhere), head = head(fun, 2), find = found, 
		functions.called = functions.called2(fun, sortit.called), 
		functions.calling = calling, where.calling = where.calling)
	class(out) <- "summary.function"
	out
}
