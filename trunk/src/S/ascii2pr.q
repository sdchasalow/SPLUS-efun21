"ascii2pr"<-
function(infile, ...)
{
#   Date written:  01 Dec 1997         Last revised:  01 Dec 1997
#   AUTHOR:  Scott D. Chasalow (Scott.Chasalow@users.pv.wau.nl)
#   OS: Win 3.1
#
#   DESCRIPTION: 
#	Wrapper for function enscript(): takes the enscript() output file
#	and sends it to a printer.
#
	psfile <- enscript(infile, ...)
	if(length(psfile) > 0 && nchar(psfile) > 0) {
		on.exit(unlink(psfile))
		cmd <- paste("lpr", psfile)
		dos(cmd, output = F, translate = F, multi = F, minimized = T)
	}
	return(invisible(NULL))
}
