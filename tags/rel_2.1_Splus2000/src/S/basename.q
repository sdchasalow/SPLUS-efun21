# $Id$

"basename"<-
function(file, translate = T)
{
#   DATE WRITTEN: 31 January 1997       LAST REVISED: 31 January 1997
#   AUTHOR:  Scott Chasalow
#   OS:  Win 3.1
#
#   Extract the base filename from a filename that possibly includes a
#   complete path.
#
#   Argument "file" should be a character vector of length one.
#
	file <- as.character(file)
	if(length(file) == 0)
		return(file)
	file <- file[1]
	if(translate)
		file <- .C("dos_sed",
			ret = as.character(file),
			as.integer(1))$ret
	ncfile <- nchar(file)
	if(ncfile == 0)
		return(file)
	ind <- 1:ncfile
	allc <- substring(file, ind, ind)
	slashes <- ind[allc == "\\"]
	if(length(slashes)) {
		substring(file, max(slashes) + 1, ncfile)
	}
	else file
}
