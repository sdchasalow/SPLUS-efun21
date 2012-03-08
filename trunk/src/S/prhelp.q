"prhelp"<-
function(fun, where, filter., ...)
{
# DATE WRITTEN:  02 Sep 1997 		 LAST REVISED:  25 Oct 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Filter the help file for a function through the function given by
#	argument "filter.". By default, the help file is filtered by
#	the function given by option "efun.helpfilter", if set; else by
#	the function given by option "efun.filter", if set; else by 
#	function enscript().
#
	if(!is.character(fun)) stop(paste(
			"Argument \"fun\" must be a character string", 
			"giving the name of a function"))
	if(missing(where)) {
		where <- eval(parse(text = paste("find(", fun, ", num = F)")))	
	# find(fun, num = F) doesn't work with my version of find!
		if(length(where) == 0)
			stop(paste("Function", fun, "not found"))
		where <- where[1]
	}
	if(is.numeric(where))
		where <- search()[where[1]]
	if(missing(filter.))
		filter. <- get.option("helpfilter", get.option("filter", 
			enscript, "efun"), "efun")
	if(is.character(filter.))
		filter. <- get(filter., mode = "function")
	file <- true.file.name(fun, where = where, write = F)
	file <- paste(where, file, sep = "\\_Help\\")
	if(!file.exists(file))
		stop(paste("File", file, "not found"))
	tit <- paste("?", fun, sep = "")
	filtfile <- filter.(file, title. = tit, ...)
	if(length(filtfile))
		cat("\nCreated file", filtfile, "\n")
	file
}
