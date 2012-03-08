"as.ftree"<-
function(x)
{
	x <- as.list(x)
	class(x) <- "ftree"
	x
}
