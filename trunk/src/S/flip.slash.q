"flip.slash"<-
function(x)
{
#   DATE WRITTEN: 3 February 1997       LAST REVISED: 28 September 1997
#   AUTHOR:  Scott Chasalow
#   OS:  Win 3.1
#
#   Translate "/" to "\\"
#
#   Argument "x" should be a character vector.
#
	x <- as.character(x)
	switch(as.character(1 + length(x)),
		"1" = return(x),
		"2" = .C("dos_sed",
			ret = as.character(x),
			as.integer(1))$ret,
		unlist(lapply(as.list(x), function(y)
		.C("dos_sed",
			ret = as.character(y),
			as.integer(1))$ret)))
}
