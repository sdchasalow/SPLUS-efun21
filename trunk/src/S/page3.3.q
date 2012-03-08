"page3.3"<-
function(x, pager = options()$pager, filename, multi = T, remove.file = F, ...
	)
{
# 22 Jan 1997: 
# 1. changed default of arg multi to T
# 2. added arg "..." to pass to print()
#
	if(!missing(filename) && missing(x)) {
		if(!interactive())
			return(invisible(dos(paste("type", filename), output = 
				F, trans = T)))
		cmd <- paste(pager, filename)
	}
	else {
# S expression
		if(!interactive()) return(invisible(print(x)))
		if(missing(filename))
			filename <- tempfile("page")
		if(missing(remove.file) && !multi)
			remove.file <- T
		on.exit({
			sink()
			unlink(filename)
		}
		)
		sink(filename)
		print(x, ...)	# "..." added by Scott Chasalow on 22 Jan 1997
		sink()
		on.exit()
		cmd <- paste(pager, filename)
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
