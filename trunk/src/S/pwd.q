# $Id$

"pwd"<-
function()
{
# DATE WRITTEN:  11 Feb 1997 		 LAST REVISED:  20 Sept 1997
# AUTHOR:  Scott Chasalow (Scott.Chasalow@users.pv.wau.nl)
#
# DESCRIPTION:
#       Print working directory. This is the "Working Directory"
#       property of an Splus icon: the DOS directory from which Splus
#       was started.  It has NOTHING to do with the S_WORK environment
#       variable nor with the Splus search list. But commonly, the
#       directory (e.g. _Data) in position 1 of the Splus search list
#       will be located in the directory returned by this function. 
# 
	dir <- dos("dir /A:D /-P", output = T, multi = F, translate = F)
	which <- match(" Directory", substring(dir, 1, 10))
	dir <- dir[which]
	ncd <- nchar(dir)
	dvec <- substring(dir, 1:ncd, 1:ncd)
	dvec <- dvec[ - (1:14)]
	paste(dvec, collapse = "")
}
