"sleep2"<-
function(n = 0, units = c("seconds", "minutes", "hours", "days"))
{
# DATE WRITTEN:  20 Oct 1997 		 LAST REVISED:  20 Oct 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Pause n time units.
#
#	This is the native S-PLUS version of sleep(). It is computationally
#	intensive, since it executes now() as fast as it can until the
#	requested time has elapsed. Function sleep() is preferred, if you have
#	sleep.exe in your DOS path.
# 
	units <- match.arg(units)
	n <- switch(units,
		seconds = n/(3600 * 24),
		minutes = n/(60 * 24),
		hours = n/24,
		days = n)
	start <- now()
	while(now() - start < n) {
	}
	invisible(NULL)
}
