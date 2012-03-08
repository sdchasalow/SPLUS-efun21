"ftree"<-
function(x, where = 1, table = unlist(lapply(where, objects)), error.level = 2:
	1, recursive = T)
{
# DATE WRITTEN:  25 May 1995             LAST REVISED:  30 Sep 1997
# AUTHOR:  Scott Chasalow
#
#  Function-calling hierarchy.
#
	error.level <- match.arg(error.level)
	f <- if(is.character(x)) x else deparse(substitute(x))
	table <- unique(c(f, table))	# add the root function to table
	assign(".ftree.record", f, frame = 0)
	on.exit(remove(".ftree.record", frame = 0))
	ff <- lapply(as.list(f), ftree.slave, table = table, error.level = 
		error.level, recursive = recursive)
	names(ff) <- f
	class(ff) <- "ftree"
	ff
}
