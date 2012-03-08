"ufun"<-
function(x, assignit = T)
{
#   DATE WRITTEN:  01 Jun 1994             LAST REVISED:  30 Jun 1998
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS: Win 3.1
#
#   DESCRIPTION:
#         Source a file created by the call,  efun(x), to update the value of
#         function x.  If argument "x" is missing,  update the value of the 
#         argument to the most recent call to efun (only if efun record has
#	  length one).
#
	if(!exists(".Efun.files", where = 1)) stop(paste(
			"efun record does not exist in database \"", search()[1
			], "\"; call function efun to edit ", 
			"a function before calling function ufun", sep = ""))
	lene <- length(.Efun.files)
	if(missing(x)) {
		if(lene > 1)
			stop(paste("Argument x must not be missing if", 
				"efun record has length > 1"))
		filein <- .Efun.files[1]
		name <- names(filein)
	}
	else {
		name <- deparse(substitute(x))
		if(!any(names(.Efun.files) == name))
			stop(paste("no entry for function \"", name, 
				"\" in efun record; call function efun to ", 
				"edit ", name, " before ", 
				"calling function ufun", sep = ""))
		filein <- .Efun.files[name]
	}
	isfirst <- (filein == .Efun.files[1])
	checkf <- checkfile(filein, writable = F)
	if(!attr(checkf, "exists")) stop(paste("File \"", filein, 
			"\" not found", sep = ""))	#
#	else if(!all(checkf)) {
#		words <- c("is a directory", "is not readable", "has size zero")
#		words <- words[!checkf]
#		lenw <- length(words)
#		if(lenw > 1) {
#			words[lenw] <- paste("and", words[lenw])
#			msg <- paste(words, collapse = ", ")
#		}
#		else msg <- words
#		stop(paste("File", filein, msg))
#	}
#
#	open <- edwin.open(name)
	open <- T
	if(assignit && open) {
		on.exit(cat("\nErrors occurred; re-edit this function", 
			"by call to efun() and use\n", if(lene == 1) "ufun()"
			 else deparse(match.call()), "to update it.\n"))
		assign(name, xnow <- dget(filein), where = 1)
		on.exit()
		cat("\nAssigned value in file \"", filein, "\" to function \"", 
			name, "\"\n", sep = "")
		invisible(xnow)
	}
	else {
		ucmd <- if(lene == 1) "ufun()" else deparse(match.call())
		if(open) {
			ecmd <- "re-edit this function by call to efun()"
		}
		else {
			ecmd <- if(isfirst) "efun()" else paste("efun(", name, 
				  ")", sep = "")
			ecmd <- paste("use", ecmd, "to re-edit this function")
			ucmd <- paste(name, "<-", ucmd)
		}
		message <- paste("\nErrors occurred;", ecmd, "and use\n", ucmd, 
			"to update it.\n")
		on.exit(cat(message))
		xnow <- dget(filein)
		on.exit()
		xnow
	}
}
