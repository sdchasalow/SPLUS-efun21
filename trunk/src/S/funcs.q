"funcs"<-
function(where = 1, frame = NULL, pattern)
{
#  DATE WRITTEN:  24 Feb 1994      LAST REVISED:  25 Jun 1998
#
# call objects(); note if `frame' or `pattern' are (explicitly) NULL, want	
# to omit these from the call, as objects() uses missing(); so no match.call
#
	oargs <- if(!missing(frame) && !is.null(frame)) list(frame = frame)
		 else list(where = where)
	gargs <- oargs
	if(!missing(pattern) && !is.null(pattern))
		oargs <- c(oargs, pattern = pattern)
	obj <- do.call("objects", oargs)
	is.func <- apply(as.matrix(obj), 1, function(x, gargs)
	is.function(do.call("get", c(x, gargs))), gargs = gargs)
	out <- obj[is.func]	#
###
### Added 25Jun98:  never include ".Last.value" in the returned vector.
### If it were a function before the call to funcs(), it sure won't be
### directly afterwards.
###
	if(length(out) > 0) {
		todrop <- out == ".Last.value"
		if(any(todrop))
			out <- out[!todrop]
	}
	out
}
