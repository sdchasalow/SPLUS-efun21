"sleep"<-
function(n = 0, units = c("seconds", "minutes", "hours", "days"), calibrate = 3
	)
{
# DATE WRITTEN:  20 Oct 1997 		 LAST REVISED:  20 Oct 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Pause n time units.
#
#	This version requires a sleep.exe command in your DOS path
#	(e.g. from gnu-win32). See also sleep2().
#
#	If n is in seconds, it should be a non-negative integer. It will be
#	rounded if necessary.
#
	units <- match.arg(units)
	n <- switch(units,
		seconds = n,
		minutes = n * 60,
		hours = n * 3600,
		days = n * 86400)
	n <- round(max(0, n - calibrate))
	dos(paste("sleep ", n, "s", sep = ""))
	invisible(NULL)
}
