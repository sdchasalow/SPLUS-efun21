# $Id$

"efun.control"<-
function(editor = get.option("editor", getenv("S_EDITOR"), "efun"), 
	prefix.fileout = get.option("prefix.fileout", "efun", "efun"), 
	suffix.fileout = get.option("suffix.fileout", tempfile2("", ""), "efun"
	), translate = get.option("translate", T, "efun"), template, ...)
{
#   DATE WRITTEN:   29 March 1995            LAST REVISED:  02 August 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS:  Win 3.1
#
#   DESCRIPTION:
#         Set control parameters for efun() and cfun(). 
#	  Allows users to set some parameters for efun and cfun.
#
	if(missing(template)) template <- get.option("template", function()
		{
		}
		, "efun")
	if(editor == "" || length(editor) == 0) {
		editor <- "notepad"
		warning(paste("the value of editor supplied is invalid;", 
			"default value of \"notepad\" was used instead"))
		if(getenv("S_EDITOR") == "") {
			warning(paste("(You may wish to set the DOS", 
				"environment variable, S_EDITOR)"))
		}
	}
	if(!is.logical(translate) || length(translate) != 1) {
		translate <- T
		warning(paste("the value of translate supplied is invalid;", 
			"default value of TRUE was used instead"))
	}
	list(editor = editor, prefix.fileout = prefix.fileout, suffix.fileout
		 = suffix.fileout, translate = translate, template = template)
}
