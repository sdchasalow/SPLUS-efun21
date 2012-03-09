# $Id$

"prfun"<-
function(funvalue, filename = tempfile2(), filter. = get.option("filter", NULL, 
	"efun"), prcmd = get.option("prcmd", "notepad", "efun"), opsys, author
	 = get.option("author", "?", "efun"), where, print.it = T, unlink.it, 
	width = 80, ...)
{
#   Date written:  14 Aug 1989         Last revised:  01 Apr 1998
#   AUTHOR:  Scott Chasalow
#   OS: Win 3.1
#
#   DESCRIPTION: 
#         Print hardcopy of S function (to a printer)
#
#   funvalue = a function,  or the name of a function,  or a character string 
#   giving a function name
#
#   I use filter. = nenscript, prcmd = "y:\\win\\ip\\gs\\gsview.exe /P", to
#   filter the file through nenscript, and then print it to a non-postscript
#   printer using gsview (a ghostscript GUI)
#
	if(missing(unlink.it)) unlink.it <- print.it	#
###
###  Process funvalue, to yield function name ("name") and the actual
###  function ("funvalue")
###
	if(is.character(funvalue)) {
		name <- funvalue
		funvalue <- get(funvalue, mode = "function")
	}
	else if(mode(funvalue) != "function") {
		farg <- substitute(funvalue)
		if(mode(farg) == "name") {
			name <- farg
			funvalue <- get(farg, mode = "function")
		}
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	else name <- deparse(substitute(funvalue))
	0	# Just to prevent following comment from deletion
###
###  Process filter., to yield filter function (or NULL)
###
	if(is.character(filter.)) {
		filter. <- get(filter., mode = "function")
	}
	if(mode(filter.) != "function" && !is.null(filter.))
		stop("argument filter. must be a function or NULL")
	0	# Just to prevent following comment from deletion
###
###  Process opsys, to yield opsys function
###
	if(missing(opsys)) {
		opsys <- get.option("opsys", NULL, "efun")
		if(length(opsys) == 0)
			opsys <- function(command)
			win3(command, translate = F, multi = F, minimized = F)
	}
	if(is.character(opsys))
		opsys <- get(opsys, mode = "function")
	if(mode(opsys) != "function")
		stop(paste("argument opsys must be a function or a", 
			"character string giving the name of a function"))
	0	# Just to prevent following comment from deletion
###
###  Create header
###
	if(missing(where)) {
		where <- paste("find(\"", name, "\", num=T)", sep = "")
		where <- eval(parse(text = where))[1]
		wstring <- names(where)
	}
	else {
		if(is.numeric(where))
			wstring <- search()[where]
		else {
			wstring <- where
			where <- match(where, search())
			if(is.na(where))
				stop(paste("database", wstring, 
				  "not in search list"))
		}
	}
	ds.date <- dataset.date(name, where)
	if(ds.date == "NA")
		warning(paste("dateset date not available for function \"", 
			name, "\" in database ", wstring, sep = ""))
	head <- matrix(0, 5, 2)
	head[, 1] <- format(c("PRINT DATE:", "LAST MODIFIED: ", "AUTHOR:", 
		"WHERE:", "CURRENT DIRECTORY:"))
	head[, 2] <- c(date(), ds.date, author, wstring, pwd())	
	# format() here converts "\\" to "\\\\", which is ugly
	dimnames(head) <- list(rep("", 5), rep("", 2))	#
###
###  Print header and function to file filename
###
	if(length(filename) != 1 || nchar(filename) == 0)
		stop("illegal filename")
	if(file.exists(filename))
		stop(paste("will not overwrite file", filename))
	oldop <- options(width = width)
	on.exit(options(oldop))
	sink(filename)
	on.exit(sink(), add = T)
	if(unlink.it)
		on.exit(unlink(filename), add = T)
	print(head[1:3,  ], quote = F)
	cat("", head[4,  ], "\n")	# So prints "\" instead of "\\"
	cat("", head[5,  ], "\n")
	dashes <- paste(rep("-", width - 1), collapse = "")
	cat(dashes, "\n")
	cat("S FUNCTION:\t", name, "\n")
	cat(dashes, "\n\n")
	print(funvalue)
	sink()
	options(oldop)
	if(unlink.it)
		on.exit(unlink(filename))
	else on.exit()
	0	# Just to prevent following comment from deletion
###
###  If print.it is TRUE, send filename to a printer, after possibly
###  filtering it
###
	if(print.it) {
		if(length(filter.) == 0)
			filtfile <- filename
		else filtfile <- filter.(filename, title. = name, ...)
		if(length(filtfile) && nchar(filtfile) && length(prcmd) && 
			nchar(prcmd)) {
			if(filtfile != filename)
				on.exit(unlink(filtfile), add = T)
			opsys(paste(prcmd, filtfile))
		}
	}
	if(unlink.it)
		return(character(0))
	else return(filename)
}
