"page4.5"<-
function(x, pager = options()$pager, filename, multi = T, remove.file = F, ...
	)
{
# SDC, 18 May 1999: 
# 1. changed default of arg multi to T
# 2. added arg "..." to pass to print()
#
	QUOTE <- ifelse(is.win32s(), "", "\"")
	if(!missing(filename) && missing(x)) {
		qfilename <- paste(sep = "", QUOTE, filename, QUOTE)
		if(!interactive())
			return(invisible(dos(paste("type", qfilename), output
				 = F, trans = T)))
		cmd <- paste(pager, qfilename)
	}
	else {
# S expression
		if(!interactive()) return(invisible(print(x)))
		if(missing(filename))
			filename <- tempfile("page")
		if(missing(remove.file) && !multi)
			remove.file <- T
		qfilename <- paste(sep = "", QUOTE, filename, QUOTE)
		sink.n <- sink(filename)
		on.exit({
			sink(unsink.to = sink.n)
			unlink(filename)
		}
		)
		print(x, ...)	# "..." added by Scott Chasalow on 18 May 1999
		sink(unsink.to = sink.n)
		on.exit()
		cmd <- paste(pager, qfilename)
	}
	if(remove.file && multi) {
		remove.file <- F
		warning("ignoring remove.file=T when multi=T")
	}
	if(remove.file)
		on.exit(unlink(filename))
	else if(missing(filename))
		warning(paste("not removing paging file", filename))
	invisible(win3(cmd, trans = T, multi = multi))
}
