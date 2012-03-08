"today"<-
function(out.format = "day mon year")
{
# DATE WRITTEN:  21 Feb 1995             LAST REVISED:  27 February 1997
# AUTHOR:  Scott Chasalow
# OS:  Win 3.1
#
# Return date object giving today's date (no time).
#
	x <- date2(c(3, 2, 5))
	if(exists("chron", mode = "function"))
		chron(x, format = "day mon year", out.format = out.format)
	else {
		warning(paste("function chron not found,  returned", 
			"character string, not date object"))
		x
	}
}
