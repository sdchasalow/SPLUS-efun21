# $Id$

"functions.calling"<-
function(name, where = 1)
{
# DATE WRITTEN:  3 Feb 1995              LAST REVISED:  7 Feb 1995
# AUTHOR:  Scott Chasalow (adapted from example in online doc for find.calls())
# PURPOSE:  find all functions on database(s) `where' that call `name':
#
#    Allow argument "name" to be the name of an object ONLY if
#    functions.calling is called in a top-level expression;  otherwise, 
#    argument "name" MUST be a character string.
#
	if(sys.parent() == 1) {
		name <- substitute(name)
		if(is.name(name))
			name <- as.character(name)
		else if(!is.character(name))
			stop(paste("Argument `name' must be an object name", 
				"or a character string giving an object name"))
	}
	else if(!is.character(name))
		stop("Argument `name' must be a character string")
	if(length(where) > 1) {
		out <- lapply(as.list(where), functions.calling, name = name)
		names(out) <- if(is.character(where)) where else search()[where
				]
		return(out)
	}
	old.keep <- options(keep = NULL)
	on.exit(options(old.keep))
	objvec <- objects(where)
	x <- logical(length(objvec))
	names(x) <- objvec
	for(i in objvec) {
		obj <- get(i, where = where, immediate = T)
		if(mode(obj) == "function" && find.calls(obj, name, expr = F) != 
			0)
			x[i] <- T
	}
	objvec[x]
}
