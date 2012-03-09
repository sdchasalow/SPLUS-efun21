# $Id$

"get.option"<-
function(x, default = NULL, prefix = "automate", label = F)
{
#  DATE WRITTEN:  14 Feb 1995                 LAST REVISED:  16 March 1995
#  AUTHOR:  Scott Chasalow
#
#  NOTE:  Returns default value if option given by x and prefix has zero
#      length,  whether or not the option has ever been set.
#
	opnm <- if(length(prefix) && nchar(prefix)) paste(prefix, x, sep = ".")
		 else x
	op <- eval(parse(text = paste("options()$", opnm, sep = "")))
	if(!length(op)) {
		op <- default
		if(label)
			attr(op, "which") <- "default"
	}
	else if(label)
		attr(op, "which") <- paste("options(\"", opnm, "\")", sep = "")
	op
}
