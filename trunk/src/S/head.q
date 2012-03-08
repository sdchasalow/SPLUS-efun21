"head"<-
function(x, n)
{
# DATE WRITTEN:  25 Jun 1997 		 LAST REVISED:  25 Jun 1997
# AUTHOR:  Scott Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Extract the head of (i.e. initial comments in) a function. 
#
	if(mode(x) != "function") {
		if(mode(x) == "character")
			x <- get(x, mode = "function")
		else stop("need the name of a function")
	}
	dpx <- deparse(x)
	firstc <- substring(dpx, 1, 1)
	bodystart <- seq(along = dpx)[firstc == "{"][1]
	if(is.na(bodystart))
		return(x)
	out1 <- dpx[1:bodystart]
	pounds <- seq(along = dpx)[firstc == "#"]
	nonp <- seq(along = dpx)[firstc != "#"]
	firstnonp <- min(nonp[nonp > min(pounds)])
	end <- if(missing(n)) firstnonp else min(bodystart + n + 1, firstnonp)
	thehead <- pounds[pounds < end]
	out2 <- dpx[thehead]
	out <- c(out1, out2, dpx[length(dpx)])
	eval(parse(text = out))
}
