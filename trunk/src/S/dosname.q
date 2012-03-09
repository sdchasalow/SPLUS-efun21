# $Id$

"dosname"<-
function(x)
{
#   DATE WRITTEN: 31 January 1997       LAST REVISED: 24 May 1999
#   AUTHOR:  Scott Chasalow
#   OS:  Win 3.1/95/NT
#   S-PLUS Version 4.5
#
#   Translate a character string into a legal DOS file name.
#
#   Argument "x" should be a character vector of length one.
#
#   DETAILS:
#         The algorithm (a) converts the string x into a legal S-PLUS name by a
#   call to make.names() (except does not translate "_" to "."); (b) removes a
#   leading "x" if one has been added by the call to make.names(); (c) 
#   translates any "." except the final one to "_"; (d) truncates the string
#   prefix if any (i.e. characters preceding a final "."), or else the entire
#   string, to at most 8 characters;  and (e)  truncates if necessary any
#   suffix following a "." to at most 3 characters.
#
	x <- as.character(x)
	if(length(x) == 0)
		return(x)
	x <- x[1]
	ncx <- nchar(x)
	if(ncx == 0)
		return(x)
	ind <- 1:ncx
	allc <- substring(x, ind, ind)
	numstart <- match(allc[1], 0:9, nomatch = 0) != 0	
	# does x begin with an integer?
	uscore <- allc == "_"	# where are the pre-existing "_" characters?
	x <- make.names(x)	# convert x into a legal S-PLUS name
	ncx <- nchar(x)
	ind <- 1:ncx
	allc <- substring(x, ind, ind)
	if(numstart) {
# Remove the initial "x" that make.names has stuck on the string
		allc <- allc[-1]
		ind <- ind[ - ncx]
	}
	if(any(uscore)) {
# Translate "." back to "_" for pre-existing "_", including the last one
		allc[uscore] <- "_"
	}
# Translate all but the last "." to a "_"
	aredots <- allc == "."
	if(any(aredots)) {
		lastdot <- max(ind[aredots])
		allc[ind != lastdot & aredots] <- "_"
		if(lastdot > 9) {
# Truncate prefix to 8 chars
			allc <- allc[ - (9:(lastdot - 1))]
		}
		0	#
# 1999.05.24: Truncate suffix (post-"." characters) if necessary to be
# at most 3 characters.  "dos_sed" did this in Splus 3.3, but no longer
# does in Splus 4.5!
#
#	.C("dos_sed", 
#		ret = as.character(x), 
#		as.integer(3))$ret
#
		dotloc <- seq(along = allc)[allc == "."]
		if(length(allc) > dotloc + 3)
			allc <- allc[1:(dotloc + 3)]
	}
	else allc <- allc[1:8]
	paste(allc, collapse = "")
}
