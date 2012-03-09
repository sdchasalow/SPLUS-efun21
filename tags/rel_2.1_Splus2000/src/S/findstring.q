# $Id$

"findstring"<-
function(pattern, where = 1, objects. = unlist(lapply(where, objects)), num = F
	)
{
#  DATE WRITTEN:  01 Jun 1995               LAST REVISED:  28 Jun 1998
#  AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
#  DESCRIPTION:
#        Find objects containing string(s) given by argument "pattern".
#
###
### First, get the vector of object names, and replicate "object. and
### where" as necessary.
###
	if(missing(objects.)) {
		objects. <- lapply(where, objects)
		tim <- unlist(lapply(objects., length), use.names = F)
		if(!any(tim > 0)) {
			warning("No objects found")
			return(data.frame(NULL))
		}
		where <- rep(where, tim)
		objects. <- unlist(objects., use.names = F)
	}
	else {
		if(!is.character(objects.))
			stop(paste("Argument \"objects.\" must be", 
				"a character vector giving the names", 
				"of objects"))
		mlen <- max(length(objects.), length(where))
		where <- rep(where, length = mlen)
		objects. <- rep(objects., length = mlen)
	}
	0	# Anti-formatting-bug feature
###
###  For each element of "objects.", and corresponding element of "where", 
###  get the object, deparse it if it is a function, and search for the
###  elements of "pattern" using grep().
###
	out <- dlapply(objects., where, function(ob, w, p)
	{
		ob <- get(ob, where = w)
		if(is.function(ob))
			ob <- deparse(ob)
		grep(p, ob)
	}
	, p = pattern)
	len <- unlist(lapply(out, length), use.names = F)
	if(num) {
		out <- data.frame(WHERE = I(where), OBJECT = I(objects.), 
			NUM.MATCHES = len)
	}
	else {
		hits <- len > 0
		if(any(hits)) {
			out <- data.frame(WHERE = I(where[hits]), OBJECT = I(
				objects.[hits]))
		}
		else out <- data.frame(NULL)
	}
	out
}
