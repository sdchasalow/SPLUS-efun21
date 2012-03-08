"functions.called2"<-
function(definition, sortit = T)
{
#  Find all functions called by function "definition".
#
#  Function functions.called() misses functions passed as arguments, e.g. to 
#  lapply, do.call, etc.  This function picks them up,  but can be fooled into 
#  reporting false positives by local objects that mask functions.
#  (Dependencies STILL may be missed in exceptional circumstances,  e.g.  if 
#  a function is assigned a new local name, or called without a new name, by a 
#  call to get();  see for example,  data.frame.default().  This function also 
#  cannot return methods called via a generic function.)
#
	if(is.character(definition)) definition <- get(definition, mode = 
			"function") else if(mode(definition) != "function") {
		farg <- substitute(definition)
		if(mode(farg) == "name")
			definition <- get(farg, mode = "function")
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	alln <- all.names(definition, max.names = 5000, unique = T)
	out <- setdiff(alln, all.names(definition, functions = F, max.names = 
		5000, unique = T))
	out2 <- unlist(lapply(alln, function(a)
	{
		if(exists(a) && is.function(get(a)))
			a
		else NULL
	}
	))
	out2 <- setdiff(out2, names(definition))
	out <- unique(c(out, out2))
	if(sortit)
		sort(out)
	else out
}
