"dfun.default"<-
function(x, where, efun.record = 1, the.call = match.call(), verbose = T)
{
#   DATE WRITTEN:  04 Feb 1997            LAST REVISED:  25 Jun 1998
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS:  Win 3.1
#
#   DESCRIPTION:
#         Default method for generic function dfun.
#         Diff FUNction: compare ASCII and S-PLUS versions of function(s).
#
#         NOTE!: Argument "x" MUST be a character vector, giving the name(s) of
#         function(s) (one or more) to be compared. This differs from argument
#         "x" to function efun(), which MUST be an unquoted function name.
#
###
### If x is a character string of length > 1, recall this
### function in a loop, once per element of x.
###
	the.call <- deparse(the.call)
	if(!is.character(x))
		stop("Argument x must be a character string or vector")
	lenx <- length(x)
	if(lenx == 0)
		stop("Argument x has zero length")
	nmw <- !missing(where)
	iner <- is.numeric(efun.record)
	lenw <- if(nmw) length(where) else 1
	if(lenw == 0)
		stop("Argument \"where\" has zero length")
	lene <- if(iner) length(efun.record) else 1
	if(length(efun.record) == 0)
		stop("Argument \"efun.record\" has zero length")
	maxl <- max(lenx, lenw, lene)
	if(maxl > 1) {
		if(nmw) {
			where <- rep(where, length = maxl)
			lenw <- maxl
		}
		if(iner) {
			efun.record <- rep(efun.record, length = maxl)
			lene <- maxl
		}
		x <- rep(x, length = maxl)
		lenx <- maxl
	}
	if(lenx > 1) {
		EDIT.FILE <- character(lenx)
		EFUN.RECORD <- character(lenx)
		WHERE <- numeric(lenx)
		ALL.EQUAL <- logical(lenx)
		thiscall <- match.call()
		ind <- 1:lenx
		for(i in ind) {
			if(nmw)
				thiscall$where <- where[i]
			thiscall$efun.record <- if(iner) efun.record[i] else 
				  efun.record
			thiscall$x <- x[i]
			thiscall$the.call <- the.call
			outi <- eval(thiscall)
			EDIT.FILE[i] <- outi$EDIT.FILE
			EFUN.RECORD[i] <- outi$EFUN.RECORD
			WHERE[i] <- outi$WHERE
			ALL.EQUAL[i] <- outi$ALL.EQUAL
		}
		if(verbose) {
			isna <- is.na(ALL.EQUAL)
			if(!any(isna) && all(ALL.EQUAL)) {
				cat("\nNo differences detected\n\n")
			}
			else {
				ndiff <- if(!all(isna)) sum(!ALL.EQUAL[!isna])
				   else 0
				nna <- sum(isna)
				cat("\n")
				if(ndiff > 0) {
				  word <- if(ndiff > 1) "differences" else 
				      "difference"
				  cat("*** ", ndiff, word, "detected  ***\n")
				}
				if(nna > 0) {
				  word <- if(nna > 1) "NA's" else "NA"
				  cat("*** ", nna, word, "generated  ***\n")
				}
				cat("\n")
			}
		}
		out <- data.frame(FUNCTION = I(x), WHERE = WHERE, EDIT.FILE = I(
			EDIT.FILE), EFUN.RECORD = I(EFUN.RECORD), ALL.EQUAL = I(
			ALL.EQUAL))
		attr(out, "call") <- the.call
		return(out)
	}
	out <- data.frame(FUNCTION = I(x), WHERE = I(as.numeric(NA)), EDIT.FILE
		 = I(as.character(NA)), EFUN.RECORD = I(as.character(NA)), 
		ALL.EQUAL = I(as.logical(NA)))
	attr(out, "call") <- the.call	#
###
### Get the function named by argument x
###
	if(nchar(x) == 0) {
		warning("Illegal function name, \"\"")
		return(out)
	}
	name <- x
	if(nmw) {
		names(where) <- NULL
		foundnm <- eval(call("exists", name = name, where = where), 
			local = F)
		if(!foundnm) {
			warning(paste("Object", name, "not found in position", 
				where))
			out$WHERE <- where
			return(out)
		}
	}
	else {
		foundnm <- eval(call("exists", name = name), local = F)
		if(!foundnm) {
			warning(paste("Object", name, "not found"))
			return(out)
		}
# If missing "where", find the function. Use eval and call to avoid finding
# an object with name "name", which my "fixed" find() would (try to) do.
		where <- eval(call("find", what = name, numeric. = T))[1]
		names(where) <- NULL
	}
	x <- get(name, where = where)
	out$WHERE <- where
	if(!is.function(x)) {
		warning(paste("Object", name, "is not a function"))
		return(out)
	}
###
### Get the proper efun record (.Efun.files in position 1 by default)
###
	if(iner) {
		if(!exists(".Efun.files", where = efun.record)) {
			warning(paste("No efun record in position", efun.record
				))
			return(out)
		}
		allfiles <- get(".Efun.files", where = efun.record)
	}
	else {
		classer <- class(efun.record)
		if(length(classer) == 0 || classer != "efun.record") {
			warning("Invalid efun record")
			return(out)
		}
		allfiles <- efun.record
	}
	out$EFUN.RECORD <- if(iner) efun.record else "(see call)"	#
###
### Check the efun record for a pre-existing entry for the function.
### If it is not found, return NA with a warning.
###
	namematch <- names(allfiles) == name
	funfile <- allfiles[namematch]
	if(length(funfile) == 0) {
		warning(paste("No entry for function", name, "in efun record"))
		return(out)
	}
	funfile <- as.character(funfile[1])
	checkf.funf <- checkfile(funfile)
	out$EDIT.FILE <- funfile
	if(!attr(checkf.funf, "exists")) {
		warning(paste("File", funfile, "not found"))
		return(out)
	}
###
### Compare ASCII and S-PLUS versions of function
###
	on.exit(cat("\nSyntax errors in ", "version of function \"", name, 
		"\" in file \"", funfile, "\"\n", sep = ""))
	alle <- all.equal(x, dget(funfile))[1]
	on.exit()
	out$ALL.EQUAL <- (alle == TRUE)
	out
}
