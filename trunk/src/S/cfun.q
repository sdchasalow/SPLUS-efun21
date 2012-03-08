"cfun"<-
function(x, fileout, dir.fileout = get.option("dir.fileout", "C:\\TEMP\\", 
	"efun"), silent = F, reverse = T, stamp. = get.option("stamp", format(
	today()), "efun"), control = efun.control(...), ...)
{
#   DATE WRITTEN:  01 June 1994            LAST REVISED:  26 Oct 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS:  Win 3.1
#
#   DESCRIPTION:
#         Do everything function efun() does, except do NOT start up an edit
#         session.
#
#         NOTE!: Argument "x" MUST be a character vector, giving the name(s) of
#         function(s) (one or more) to be checked in to the efun record. This 
#         differs from argument "x" to function efun(), which MUST be an
#         unquoted function name.
#
###
### If x is a character string of length > 1, replicate other args as necessary
### and recall this function in a loop, once per element of x. I used
### match.call() and eval() so that if arguments "control" and "..." (e.g.
### prefix.fileout and suffix.fileout) are missing in the original call they
### are still missing in the re-calls. This is desirable, for example, so that
### the default unique file names created in efun.control() do not use the same
### endings for multiple functions.
###
	if(!is.character(x)) stop(
			"Argument x must be a character string or vector")
	lenx <- length(x)
	if(lenx == 0)
		stop("Argument x has zero-length")
	if(lenx > 1) {
		notmissfo <- !missing(fileout)
		if(notmissfo && length(fileout) != lenx)
			stop(paste("fileout has length ", length(fileout), 
				", should be ", lenx, sep = ""))
		dir.fileout <- rep(dir.fileout, length = lenx)
		stamp. <- switch(data.class(stamp.),
			character = stamp.,
			"function" = stamp.(),
			expression = eval(stamp., local = F),
			name = eval(call("get", name = substitute(stamp.)), 
				local = F),
			format(stamp.))
		stamp. <- if(is.character(stamp.)) stamp. else format(stamp.)
		stamp. <- rep(stamp., length = lenx)
		silent <- silent[1]
		thiscall <- match.call()
		thefiles <- character(lenx)
		ind <- if(reverse) lenx:1 else 1:lenx
		is.oldrec <- exists(".Efun.files", where = 1)
		oldrec <- if(is.oldrec) get(".Efun.files", where = 1) else 
				character(0)
		exitfun <- function(oldrec, thefiles, is.oldrec)
		{
			if(is.oldrec) {
				assign(".Efun.files", oldrec, where = 1, 
				  immediate = T)
				newf <- thefiles[match(thefiles, as.vector(
				  oldrec), nomatch = 0) == 0]
				msg <- paste("\nBacking out changes:", 
				  "restoring efun record", 
				  "and unlinking new files:\n")
			}
			else {
				remove(".Efun.files", where = 1)
				newf <- thefiles
				msg <- paste("\nBacking out changes:", 
				  "removing new efun record", 
				  "and unlinking new files:\n")
			}
			newf <- newf[newf != ""]
			cat(msg)
			if(length(newf)) {
				unlink(newf)
				print(newf)
			}
		}
		on.exit(exitfun(oldrec, thefiles, is.oldrec))
		for(i in ind) {
			thiscall$x <- x[i]
			if(notmissfo)
				thiscall$fileout <- fileout[i]
			thiscall$dir.fileout <- dir.fileout[i]
			thiscall$stamp. <- stamp.[i]
			thefiles[i] <- eval(thiscall)
		}
		on.exit(add = F)
		return(invisible(thefiles))
	}
###
### Get the function named by argument x; if it does not exist, get the
### function template (control$template)
###
	if(nchar(x) == 0)
		stop("Illegal function name, \"\"")
	name <- x
	foundnm <- eval(call("exists", name = name), local = F)
	if(foundnm) {
		x <- get(name)
		if(!is.function(x))
			stop(paste("Object", name, "is not a function"))
	}
	else {
		x <- control$template
		if(!is.function(x))
			stop(paste("control$template is not a function"))
	}
	dir.fileout <- final.slash(dir.fileout, translate = T, empty.string = F
		)	#
###
### Construct the file name (fileout)
###
	prefix <- control$prefix.fileout
	suffix <- control$suffix.fileout
	if(missing(fileout)) {
		if(nchar(prefix))
			fileout <- paste(prefix, name, sep = ".")
		else fileout <- name
		if(nchar(suffix))
			fileout <- paste(fileout, suffix, sep = ".")
	}
	if(control$translate) fileout <- dosname(fileout)	
	# Translate fileout to a legal DOS filename
	fileout <- paste(dir.fileout, fileout, sep = "")	#
###
### Check the efun record in position 1, if any, for a pre-existing entry for
### the function. If it is found, exit with a warning; the efun record is
### unchanged.
###
	foundfi <- exists(".Efun.files", where = 1)
	if(foundfi) {
		allfiles <- get(".Efun.files", where = 1)
		namematch <- names(allfiles) == name
		oldf <- allfiles[namematch]
		if(length(oldf)) {
			oldf <- as.character(oldf[1])
			checkf.old <- checkfile(oldf)
			if(!attr(checkf.old, "exists"))
				warning(paste("File", oldf, "not found"))
			if(!silent) {
				n <- length(allfiles)
				i <- min((1:n)[namematch])
				cat(paste("* Function \"", name, 
				  "\" already in efun record (", i, "/", n, 
				  ")\n", sep = ""))
			}
			return(invisible(oldf))
		}
	}
	else allfiles <- NULL
	fo.exists <- attr(checkfile(fileout), "exists")
	if(fo.exists)
		stop(paste("File", fileout, "already exists. ", 
			"Remove it if you insist,  or try", 
			"a different file name."))
	dput(x, fileout)
	on.exit(unlink(fileout), add = F)	#
###
### Update the efun record; save the old one as .Efun.files.BAK000
###
	assign(".Efun.files.BAK000", allfiles, where = 1, immediate = T)
	stampsnow <- attr(allfiles, "stamps")	
	#  For compat. with old .Efun.files that don't have "stamps" attribute:
	lendiff <- length(allfiles) - length(stampsnow)
	if(lendiff > 0)
		stampsnow <- c(stampsnow, rep("", lendiff))
	allfiles <- c(fileout, allfiles)
	stamp. <- switch(data.class(stamp.),
		character = stamp.,
		"function" = stamp.(),
		expression = eval(stamp., local = F),
		name = eval(call("get", name = substitute(stamp.)), local = F),
		format(stamp.))
	stamp. <- if(is.character(stamp.)) stamp.[1] else format(stamp.[1])	
	# format() used instead of as.character so chron library not needed.
	attr(allfiles, "stamps") <- c(stamp., stampsnow)
	attr(allfiles, "labels") <- as.character(seq(along = allfiles))
	names(allfiles)[1] <- name
	class(allfiles) <- "efun.record"
	assign(".Efun.files", allfiles, where = 1, immediate = T)
	on.exit(add = F)	# Turn off unlinking of fileout
	n <- length(allfiles)
	if(!silent) cat(paste("  Function \"", name, 
			"\" checked in to efun record (1/", n, ")\n", sep = "")
			)	#
###
### Check the function for syntax errors
###
	on.exit(cat("\nSyntax errors in ", "version of function \"", name, 
		"\" in file \"", oldf, "\"\n", sep = ""))
	dget(fileout)
	on.exit()
	invisible(fileout)
}
