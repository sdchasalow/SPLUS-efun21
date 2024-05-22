# $Id$

"final.slash"<-
function(x, translate = T, empty.string = F)
{
# DATE WRITTEN:  03 Oct 1997 		 LAST REVISED:  12 Oct 1997
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
# OS:  Win 3.1
#
# DESCRIPTION:
#       Add the character "\\" to the end of every element of x that does
#	not already end with that character.
#
# x	a character vector
# 
	x <- as.character(x)
	if(translate)
		x <- flip.slash(x)
	nc <- nchar(x)
	lastc <- substring(x, nc, nc)
	which <- lastc != "\\"
	if(!empty.string)
		which <- which & nc != 0
	x[which] <- paste(x[which], "\\", sep = "")
	x
}
