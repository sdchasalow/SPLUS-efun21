"make.template"<-
function(date. = today(), author)
{
# DATE WRITTEN:  06 Feb 1997		LAST REVISED:  28 Feb 2001
# AUTHOR:  Scott Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Creates a function template.  Returns a function having no
#       arguments and only comments in the body.
# 
	date. <- format(date.)
	if(missing(author))
		author <- get.option("author", "?", "efun", F)
	body <- character(16)
	body[1] <- paste("# DATE WRITTEN: ", date., "\t\t", "LAST REVISED: ", 
		date.)
	body[2] <- paste("# AUTHOR: ", author)
	body[3] <- "#"
	body[4] <- "# DESCRIPTION:"
	body[5] <- "#       ?"
	body[6] <- "#"
	body[7] <- "# REQUIRED ARGUMENTS:"
	body[8] <- "#"
	body[9] <- "# OPTIONAL ARGUMENTS:"
	body[10] <- "#"
	body[11] <- "# VALUE:"
	body[12] <- "#"
	body[13] <- "# DETAILS:"
	body[14] <- "#"
	body[15] <- "# SEE ALSO:"
	body[16] <- "#"
	body <- paste(body, collapse = "\n")
	the.text <- paste("function() {\n", body, "\n}")
	eval(parse(text = the.text))
}
