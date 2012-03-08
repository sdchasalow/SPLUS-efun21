"efun"<-
function(x, fileout, dir.fileout = get.option("dir.fileout", "C:\\TEMP\\", 
	"efun"), clobber = F, stamp. = get.option("stamp", format(today()), 
	"efun"), control = efun.control(...), ...)
{
#   DATE WRITTEN:   01 June 1994            LAST REVISED:  26 Oct 1997
#   AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@users.pv.wau.nl)
#   OS:  Win 3.1
#
#   DESCRIPTION:
#         Dputs a function, "x", to a file and opens an editor on that
#         file in a new window.
#         Will NOT change the current value of the function;  to do so
#         after editing,  use function "ufun" (for "update function").
#
	dir.fileout <- final.slash(dir.fileout, translate = T, empty = F)
	translate <- control$translate
	template <- control$template
	editor <- control$editor
	prefix <- control$prefix.fileout
	suffix <- control$suffix.fileout
	foundfi <- exists(".Efun.files", where = 1)
	if(missing(x)) {
		if(clobber)
			stop(paste("Argument x must not be missing", 
				"if clobber = T"))
		if(foundfi) {
			file <- .Efun.files[1]
			name <- names(file)
			if(foundnm <- eval(call("exists", name = name), local
				 = F))
				x <- eval(call("get", name = name), local = F)
		}
		else stop(paste("efun record not found in database \"", search(
				)[1], "\";\n nothing available for re-editing.",
				sep = ""))
	}
	else {
		sub.x <- substitute(x)
		if(length(sub.x) != 1 || !is.name(sub.x)) stop(
				"efun() needs an unquoted object name")	
	# prevent efun(get("y",....)) on analogy with vi()
		name <- deparse(sub.x)
		foundnm <- eval(call("exists", name = name), local = F)
	}
	if(foundnm && !is.function(x)) stop(paste("Object", name, 
			"is not a function"))	#
###
### Construct file name (fileout)
###
	if(missing(fileout)) {
		if(nchar(prefix))
			fileout <- paste(prefix, name, sep = ".")
		else fileout <- name
		if(nchar(suffix))
			fileout <- paste(fileout, suffix, sep = ".")
	}
	if(translate) fileout <- dosname(fileout)	
	# Translate fileout to a legal DOS filename
	fileout <- paste(dir.fileout, fileout, sep = "")
	if(foundfi) {
		allfiles <- get(".Efun.files", where = 1)
		cat("\n", length(allfiles), 
			"functions in previous efun record:\n\n")
		oldf <- allfiles[names(allfiles) == name]
		if(length(oldf)) {
			oldf <- oldf[1]
			checkf.old <- checkfile(oldf)
			if(!attr(checkf.old, "exists"))
				warning(paste("File", oldf, "not found"))
		}
	}
	else allfiles <- NULL
	ucmd <- if(foundfi && length(allfiles[names(allfiles) != name])) paste(
			"ufun(", name, ")", sep = "") else "ufun()"
	message <- paste("Editing function \"", name, "\"; *** use ", ucmd, 
		" to update ***\n", sep = "")	#
#
# Would like to check here if an edit session is already open on the file, but
# don't know how to do that in DOS.
#
#	open <- edwin.open(name)
#	if(open) {
#		on.exit({
#			cat("\nError:  editor window already ", 
#			 "open for function \"", name, "\"\n", sep = "")
#			cat("*** Use", ucmd, "to update function ***\n")
#		}
#		)
#		if(foundfi) {
#			if(!length(oldf))
#				warning(paste("Name of edit file ", 
#				 "for function \"", name, 
#				  "\" not an element of vector .Efun.files", 
#				 sep = ""))
#			else if(foundnm && all(checkf.old[c("notdir", 
#				"readable")])) {
#				if(all.equal(x, dget(oldf))[1] != T)
#				  warning(paste("Function \"", name, 
#				    "\" not equal to current ", 
#				    "version in file \"", oldf, "\"", 
#				    sep = ""))
#			}
#		}
#		else warning(paste("efun record not found ", "in database \"", 
#			 search()[1], "\"", sep = ""))
#		if(!foundnm)
#			warning(paste("Function \"", name, 
#				"\" is being edited but was not found", 
#				 sep = ""))
#		on.exit()
#		stop(paste("Editor window already open ", "for function \"", 
#			name, "\"\n", "*** Use ", ucmd, 
# 			" to update function ***\n", sep = ""))
#	}
#
	if(editor == "" || length(editor) == 0) {
		editor <- "notepad"
		cat("\nNo editor specified;  using notepad\n")
		if(getenv("EDITOR") == "") {
			cat("(You may wish to set the DOS ")
			cat("environment variable, S_EDITOR)\n")
		}
	}
	edit.cmd <- editor
	if(foundfi && length(oldf)) {
		editok <- attr(checkf.old, "exists")
		if(!clobber) {
			if(editok)
				fileout <- oldf
			else stop(paste("Cannot edit file \"", oldf, "\"", sep
				   = ""))
			if(!foundnm)
				warning(paste("Function \"", name, 
				  "\" is being edited but was not found", sep
				   = ""))
		}
		else {
			fo.exists <- attr(checkfile(fileout), "exists")
			if(fo.exists && fileout != oldf)
				stop(paste("File", fileout, "already exists. ", 
				  "Remove it if you insist,  or try", 
				  "a different file name."))
			if(editok) {
				if(foundnm)
				  dput(x, fileout)
				else stop(paste("May not clobber existing", 
				    "file with function template"))
			}
			else {
				if(!foundnm) {
				  if(!is.function(template))
				    stop("template is not a function")
				  x <- template
				}
				dput(x, fileout)
				warning("Clobbered uneditable file")
			}
			warning(paste("Clobbered file, \"", oldf, "\"", sep = 
				""))
		}
		win3(paste(edit.cmd, fileout), multi = T)
		cat(message)
	}
	else {
		fo.exists <- attr(checkfile(fileout), "exists")
		if(fo.exists)
			stop(paste("File", fileout, "already exists. ", 
				"Remove it if you insist,  or try", 
				"a different file name."))
		if(!foundnm) {
			if(!is.function(template))
				stop("template is not a function")
			x <- template
		}
		dput(x, fileout)
		on.exit(unlink(fileout), add = F)
		win3(paste(edit.cmd, fileout), multi = T)
		cat(message)
	}
	assign(".Efun.files.BAK000", allfiles, where = 1, immediate = T)
	allfiles <- allfiles[names(allfiles) != name]
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
	if(foundfi && length(oldf)) {
		if(foundnm && editok) {
			on.exit(cat("\nSyntax errors in ", 
				"version of function \"", name, "\" in file \"",
				oldf, "\"\n", sep = ""))
			if(all.equal(x, dget(oldf))[1] != T)
				warning(paste("Function \"", name, 
				  "\" not equal to current version in file \"", 
				  oldf, "\"", sep = ""))
			on.exit()
		}
		if(clobber && fileout != oldf)
			unlink(oldf)
	}
	invisible(fileout)
}
