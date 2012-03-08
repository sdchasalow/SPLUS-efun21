"enscript"<-
function(infile, outfile = tempfile2(...), title. = NULL, op.title = "-b", 
	options. = "-2rG -MA4 -L62", printer = getenv("PRINTER"), path = 
	"enscript", overwrite = F, ...)
{
# DATE WRITTEN:  10 Feb 1997 		 LAST REVISED:  26 Nov 1997
# AUTHOR:  Scott Chasalow (Scott.Chasalow@users.pv.wau.nl)
# OS: Win 3.1
#
# DESCRIPTION:
#       Filter the file given by infile through the enscript ASCII to
#       PostScript converter.
# 
	infile <- infile[1]
	title. <- nospaces(title.)
	titop <- if(length(title.) > 0) paste(op.title, "\"", title., "\"", sep
			 = "") else ""
	if(length(outfile) > 0) {
		outfile <- outfile[1]
		if(outfile == infile)
			stop(paste("May not overwrite input file, ", infile, 
				"!", sep = ""))
		if(!overwrite && file.exists(outfile))
			stop(paste("will not overwrite file", outfile))
		outfileop <- paste("-p", outfile, sep = "")
		printerop <- ""
	}
	else {
		outfileop <- ""
		if(length(printer) == 0 || nchar(printer) == 0) {
			printer <- "LPT1"
			warning("No printer specified, using LPT1")
		}
		printerop <- paste("-P", printer, sep = "")
	}
	cmd <- paste(path, options., titop, outfileop, printerop, infile)	#
###
### Write cmd to a batch file, and execute the batch file via dos().
### This is to avoid the problems with dos() when argument "command" is too
### long, e.g. if infile is a long string.  In such cases, the command often
### fails, even though it works fine if called directly from DOS rather than
### via dos(). The problem starts to appear when nchar(cmd) is around 102 or
### more, but the exact bug description is elusive.  Please, can I ditch this
### sorry excuse for an operating system and stick with UNIX?
### 
	tmp <- paste(getenv("S_TMP"), "\\__tmp.bat", sep = "")
	on.exit(unlink(tmp))
	cat(cmd, file = tmp)
	dos(tmp, output.to.S = T, translate = F)
	outfile
}
