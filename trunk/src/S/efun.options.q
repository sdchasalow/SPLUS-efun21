"efun.options"<-
function(...)
{
#       DATE WRITTEN: 15 March 1995           LAST REVISED:  31 January 1997
#       AUTHOR:  Scott Chasalow
#       OS:  Win 3.1
#
	if(nargs() == 0) {
		current <- .Options
		eop <- current[grep("efun*", names(current))]
		eop <- eop[sort(names(eop))]
		return(eop)
	}
	temp <- list(...)
	if(nargs() == 1 && length(names(temp)) == 0) {
# "..." is a single unnamed list or character vector
		temp <- temp[[1]]
		if(!is.list(temp))
			names(temp) <- temp
	}
	chk <- grep("efun*", names(temp))
	if(length(chk) < length(temp))
		stop(paste(length(temp) - length(chk), 
			"option names do not begin with string \"efun\""))
	if(!is.list(temp))
		options(...)
	else invisible(options(...))
}
