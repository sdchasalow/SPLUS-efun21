"print.summary.function"<-
function(x, ...)
{
# DATE WRITTEN:  08 Oct 1997 		 LAST REVISED:  12 Oct 1997
# AUTHOR:  Scott D. Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       print an object of class "summary.function", such as that returned
#	by function summary.function().
# 
	labs <- c("Function:", "In Position:", "In Directory:", 
		"Function Date:", "All Positions:", "Head:")
	labs <- format(labs)
	xname <- x$name
	xfind <- paste(x$find, collapse = ", ")
	cat("\n", labs[1], "\t", xname, "\n", sep = "")
	cat(labs[2], "\t", x$nwhere, "\n", sep = "")
	cat(labs[3], "\t", x$cwhere, "\n", sep = "")
	cat(labs[4], "\t", x$dataset.date, "\n", sep = "")
	cat(labs[5], "\t", xfind, "\n", sep = "")
	cat("\n", labs[6], "\n", sep = "")
	print(x$head)
	cat("\nFunctions called by ", xname, ":\n", sep = "")
	print(x$functions.called, quote = F)
	cat("\nFunctions in position ", x$where.calling, " that call ", xname, 
		":\n", sep = "")
	print(x$functions.calling, quote = F)
	invisible(x)
}
