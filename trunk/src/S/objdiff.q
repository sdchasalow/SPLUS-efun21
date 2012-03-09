# $Id$

"objdiff"<-
function(x, y, file = tempfile("diff"), command = "fc", multi = T)
{
# Revised by Scott Chasalow on 24 May 1999:  added argument multi = T
	old <- tempfile("old")
	new <- tempfile("new")
	on.exit(unlink(c(old, new)))
	dput(x, old)
	dput(y, new)
	if(missing(file)) {
		if(!multi)
			on.exit(unlink(file), add = T)
		else warning(paste("not removing paging file", file))
	}
	status <- dos(paste(command, old, new, ">", file), trans = T)
	page(file = file, multi = multi)
	if(!missing(file))
		file
	else invisible(status)
}
