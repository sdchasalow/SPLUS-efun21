# $Id$

"date2"<-
function(which.fields, sep = " ", iso = F)
{
# DATE WRITTEN:  27 Feb 1997 		 LAST REVISED:  04 Apr 2001
# AUTHOR:  Scott Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Returns a character string giving the current date and time.
#       Default value is same as date(), but allows other formats.
#
# OPTIONAL ARGUMENTS:
# which.fields  an integer vector, specifying the order of, and
#       possibly subsetting, the (white-space separated) fields in the
#       string returned by function date that should be returned.
#       Fields are coded as follows: 1 = Day of the week, 2 = Month, 
#       3 = Day of the month, 4 = time (hour:min:sec), 5 = year.
# sep   a character string, which will be used to separate date and
#       time fields in the output string. The default is a blank space, 
#       " ", if iso is FALSE, or a dash, "-", if ISO is TRUE. Ignored
#       if argument which.fields is missing and iso is FALSE.
# iso   logical flag.  Ignored if argument which.fields is NOT missing.
#       If TRUE, date is returned in ISO format, e.g. 2001-03-20.  The
#       default is FALSE.
#
# VALUE:
#       a character string, possibly giving the current date and/or
#       time, depending on the value of argument which.fields.
#
# DETAILS:
#       This function calls the DOS date command via a call to
#       function date, and then allows substrings of the resulting
#       string to be extracted and/or reordered. Under UNIX, you can
#       do this, with finer control, by calling the UNIX date command
#       with appropriate command-line options. Under DOS, this is not
#       possible, hence this function.
#
	x <- date()
	if(!iso && missing(which.fields))
		return(x)
	tmp <- tempfile("datejunk")
	on.exit(unlink(tmp))
	cat(x, file = tmp)
	out <- scan(tmp, character())
	if(iso && missing(which.fields)) {
		mo <- out[2]
		mos <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
			"Sep", "Oct", "Nov", "Dec")
		mo <- leadzero(match(mo, mos), 2)
		d <- leadzero(out[3], 2)
		if(missing(sep))
			sep <- "-"
		paste(c(out[5], mo, d), collapse = sep)
	}
	else paste(out[which.fields], collapse = sep)
}
