# $Id$

"print.efun.version"<-
function(x, ...)
{
	ver <- paste(x$major, x$minor, sep = ".")
	sver <- x$splus.version
	if(length(sver) > 1) {
		word <- "versions"
		sver <- paste(sver[c(1, length(sver))], collapse = " - ")
	}
	else {
		word <- "version"
	}
	word <- paste("(", word, " ", sver, ")", sep = "")
	cat("Version", ver, x$status, x$status.rev, "for S-PLUS", word, "for", 
		x$os, ":", x$year, "\n")
	invisible(x)
}
