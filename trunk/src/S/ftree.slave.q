# $Id$

"ftree.slave"<-
function(x, table, error.level, recursive)
{
# DATE WRITTEN:  25 May 1995             LAST REVISED:  2 June 1995
# AUTHOR:  Scott Chasalow
#
#  Support for function ftree().
#
	if(error.level == 2) {
		f <- intersect(functions.called2(x), table)
	}
	else f <- intersect(functions.called(x), table)
	ff <- as.list(f)
	if(length(ff)) {
		if(recursive) {
			ftr <- get(".ftree.record", frame = 0)
			which <- is.na(match(f, ftr))
			if(any(which))
				assign(".ftree.record", unique(c(ftr, f)), 
				  frame = 0)
			ff[which] <- lapply(ff[which], ftree.slave, table = 
				table, error.level = error.level, recursive = T
				)
			if(!all(which)) {
				nullftree <- as.ftree(list(PRUNED... = as.ftree(
				  list())))
				ff[!which] <- lapply(ff[!which], ftree.slave, 
				  table = table, error.level = error.level, 
				  recursive = F)
				ff[!which & sapply(ff, length)] <- list(
				  nullftree)
			}
		}
		else {
			ff <- rep(list(as.ftree(list())), length(ff))
		}
	}
	names(ff) <- f
	class(ff) <- "ftree"
	ff
}
