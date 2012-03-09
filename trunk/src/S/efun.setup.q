# $Id$

"efun.setup"<-
function(stamp = tod, which.stamp = stamp, dir.fileout = "S_src01\\", 
	prefix.fileout = "", suffix.fileout = NULL, translate = T, editor = 
	NULL, template = NULL, author = NULL, filter = NULL, prcmd = NULL, 
	opsys = NULL, helpfilter = NULL)
{
#       DATE WRITTEN: 15 March 1995		 LAST REVISED:  25 Oct 1997
#       AUTHOR:  Scott Chasalow
#       OS:  Win 3.1
#
#       Note that argument default values are relevant ONLY if the
#       corresponding options have not already been set.  If the corresponding
#       option is already set, then you can change its value only by explicitly
#       providing a value for the argument.
#
	dir.fileout <- final.slash(dir.fileout, translate = T, empty = F)
	argnms <- names(amatch(efun.setup, expression(efun.setup())))
	argnms <- paste("efun", argnms, sep = ".")
	which <- logical(length(argnms))
	if(missing(stamp)) tod <- format(today())	#	
#
# Want oplist to be able to contain NULL's, so don't e.g. use
# oplist[[1]] <- stamp, which would drop that component if stamp were NULL.
#
	oplist <- list(stamp, which.stamp, dir.fileout, prefix.fileout, 
		suffix.fileout, translate, editor, template, author, filter, 
		prcmd, opsys, helpfilter)
	names(oplist) <- argnms
	current <- efun.options()
	nms.cur <- names(current)
	if(!missing(stamp) || is.na(match("efun.stamp", nms.cur))) {
		which[1] <- T
	}
	if(!missing(which.stamp) || is.na(match("efun.which.stamp", nms.cur))) 
		{
		which[2] <- T
	}
	if(!missing(dir.fileout) || is.na(match("efun.dir.fileout", nms.cur))) 
		{
		which[3] <- T
	}
	if(!missing(prefix.fileout) || is.na(match("efun.prefix.fileout", 
		nms.cur))) {
		which[4] <- T
	}
	if(!missing(suffix.fileout)) {
		which[5] <- T
	}
	if(!missing(translate) || is.na(match("efun.translate", nms.cur))) {
		which[6] <- T
	}
	if(!missing(editor)) {
		which[7] <- T
	}
	if(!missing(template)) {
		which[8] <- T
	}
	if(!missing(author)) {
		which[9] <- T
	}
	if(!missing(filter)) {
		which[10] <- T
	}
	if(!missing(prcmd)) {
		which[11] <- T
	}
	if(!missing(opsys)) {
		which[12] <- T
	}
	if(!missing(helpfilter)) {
		which[13] <- T
	}
	oldops <- options(oplist[which])
	invisible(oldops)
}
