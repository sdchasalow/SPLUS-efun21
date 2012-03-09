# $Id$

"now"<-
function(out.format = c(dates = "day mon year", times = "h:m:s"))
{
# DATE WRITTEN:  30 Sep 1997             LAST REVISED:  30 Sep 1997
# AUTHOR:  Scott Chasalow
# OS:  Win 3.1
#
# Return a chron object giving the current date and time.
#
# SEE ALSO: today
#
	x <- date2(c(3, 2, 5))
	tt <- date2(4)
	if(exists("chron", mode = "function"))
		chron(x, times = tt, format = c(dates = "day mon year", times
			 = "h:m:s"), out.format = out.format)
	else {
		warning(paste("function chron not found,  returned", 
			"character string, not dates object"))
		paste(x, tt)
	}
}
