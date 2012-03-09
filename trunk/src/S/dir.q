# $Id$

"dir"<-
function(directory = ".", sort.by = c("name", "date"), type = c("all", "files", 
	"directories"), long = F)
{
# DATE WRITTEN:  11 Feb 1997 		 LAST REVISED:  29 Apr 1999
# AUTHOR:  Scott D. Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Return a vector giving the names of files in a directory
#	(if long = F), or a vector giving the names and attributes of
#	the files (if long = T).
#
#       29 Apr 1999:  Added check for directory existence, and error
#       message if it does not exist.  Old function returned character(0)
#       under Splus 3.3, and dumped under Splus 4.5 in this case.
#
#       NOTE:  Changed default to directory = ".", and made directory = ""
#       return an error (does not exist).  Help page needs updating!
# 
	if(length(directory) == 0 || !file.exists(directory)) stop(paste(
			"can't open directory \"", directory, "\"", sep = ""))
	sort.by <- match.arg(sort.by)
	type <- match.arg(type)
	switches <- "'/L /-P /-W'"
	if(!long)
		switches <- paste(switches, "'/B'")
	typ <- switch(type,
		all = "'/A'",
		files = "'/A-D'",
		directories = "'/AD'")
	ord <- if(sort.by == "name") "'/O:GN'" else "'/O:G-D'"
	switches <- paste(switches, typ, ord)
	dos(paste("dir", switches, directory), output = T, multi = F, translate
		 = T)
}
