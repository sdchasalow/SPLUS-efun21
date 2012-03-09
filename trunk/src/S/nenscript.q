# $Id$

"nenscript"<-
function(infile, outfile = tempfile2(...), title. = NULL, op.title = "-i", 
	options. = "-2rG -TA4 -L62", printer = getenv("PRINTER"), path = 
	"nenscrib", overwrite = F, ...)
{
# DATE WRITTEN:  25 Oct 1997 		 LAST REVISED:  26 Nov 1997
# AUTHOR:  Scott D. Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Wrapper for function enscript(); changes default for argument
#	"path" from "enscript" to "nenscrib", and changes defaults
#	for arguments "op.title" and "options.".
# 
	enscript(infile, outfile, title., op.title, options., printer, path, 
		overwrite)
}
