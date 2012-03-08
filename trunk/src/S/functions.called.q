"functions.called"<-
function(definition, sortit = T)
{
# Find all functions called by function "definition"
	if(is.character(definition)) definition <- get(definition, mode = 
			"function") else if(mode(definition) != "function") {
		farg <- substitute(definition)
		if(mode(farg) == "name")
			definition <- get(farg, mode = "function")
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	out <- setdiff(all.names(definition, max.names = 5000, unique = T), 
		all.names(definition, functions = F, max.names = 5000, unique
		 = T))
	if(sortit)
		sort(out)
	else out
}
