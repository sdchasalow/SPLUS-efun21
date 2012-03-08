"tempfile2"<-
function(pattern = "file", dir = getenv("S_TMP"), translate = T)
{
#   DATE WRITTEN: 31 January 1997       LAST REVISED: 21 September 1997
#   AUTHOR:  Scott Chasalow
#   OS:  Win 3.1
#
#   Added to function tempfile() the ability to specify a directory
#   other than S_TMP in which to locate the temporary file.
#   If dir == "", no leading directory is used.
#
	if(translate) dir <- .C("dos_sed",
			ret = as.character(dir),
			as.integer(1))$ret
	filen <- .C("dos_tmpnam",
		as.character(pattern),
		length(pattern),
		ret = character(length(pattern)))$ret
	if(length(filen) > 1)
		filen <- unlist(lapply(filen, basename))
	else filen <- basename(filen)
	ncdir <- nchar(dir)
	lastc <- substring(dir, ncdir, ncdir)
	which <- ncdir > 0 & lastc != "\\"
	dir[which] <- paste(dir[which], "\\", sep = "")
	paste(dir, filen, sep = "")
}
