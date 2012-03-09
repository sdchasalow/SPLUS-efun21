# $Id$

"ehelp"<-
function(object, dir, editor = getenv("EDITOR"))
{
# DATE WRITTEN:  7 Feb 1995                 LAST REVISED:  10 Feb 1997
# AUTHOR:  Scott Chasalow
# PURPOSE:  Call prompt() to create a helpfile template,  if none exists 
#     (otherwise use existing helpfile template),  and open an editor to
#     edit the helpfile.
#
# NOTE: This function not yet ported to Windows! At the moment it will
#       NOT work.
#
#       The prompt() function is rather different in Splus for Win. Argument
#       names are different. Should probably just call prompt, and then open
#       an editor on the filename it creates. (But the current version
#       of prompt.default does NOT return any useful value. Note that it
#       also will overwrite a pre-existing file!)
#
	name <- substitute(object)
	if(is.language(name) && !is.name(name))
		name <- eval(name)
	name <- as.character(name)
	if(translate)
		filename <- dosname(filename)
	test <- checkfile(filename)
	if(!attr(test, "exists"))
		do.call("prompt.default", list(object = name, dir = dir))
	else {
#	if(!test["notdir"] || !test["readable"] || !test["writable"])
#		stop(paste("Cannot open file", filename))
		cat(paste("\nOpening existing file,", filename, "\n"))
	}
	if(editor == "" || length(editor) == 0) {
		editor <- "notepad"
		cat("\nNo editor specified;  using notepad...\n")
		if(getenv("EDITOR") == "") {
			cat("(You may wish to set the DOS ")
			cat("environment variable, S_EDITOR)\n")
		}
	}
	edit.cmd <- editor
	win3(paste(edit.cmd, fileout), multi = T)
	invisible(filename)
}
