"library3.3"<-
function(section = NULL, first = F, help = NULL, lib.loc = paste(getenv("SHOME"
	), "library", sep = "\\"))
{
#####
# Bug fix, 23 Oct 1997, Scott Chasalow
# I finally fixed this stupid annoying bug which generated warnings whenever
# length(lib.loc) was greater than 1.
# What did I do: I changed match(lib.loc, def.lib.loc ... to
# match(def.lib.loc, lib.loc ...
#####
#
# user may want to assign lib.loc on where=0 in her .First().
# if given as argument, we ignore the stored version entirely
# we always search $SHOME/library, but after the others on the list
	if(missing(lib.loc) && exists("lib.loc", where = 0)) lib.loc <- get(
			"lib.loc", where = 0)
	def.lib.loc <- paste(getenv("SHOME"), "library", sep = "\\")
	if(!match(def.lib.loc, lib.loc, nomatch = F)) lib.loc <- c(lib.loc, 
			def.lib.loc)	
	# remember that lib.loc may be a vector of directory names
# apparently the author forgot!! (SDC)
	if(missing(section) && missing(help)) {
		for(ll in lib.loc) {
			win3(paste("notepad ", ll, "\\", "README.txt", sep = ""
				))
		}
		pos <- NULL	# nothing attached
	}
	else {
# find directory containing .Data using vectorized is.dir function
# Windows is.dir function is already vectorized
		is.dir.v <- is.dir
		if(!missing(help))
			section <- as.character(substitute(help))
		if(!missing(section)) section <- as.character(substitute(
				section))	
	# note above makes library(section=foo, help=T)
# give help for foo and not attach foo
		dir <- paste(lib.loc, section, sep = "\\")
		idir <- min((1:length(dir))[is.dir.v(paste(dir, "_Data", sep = 
			"\\"))])
		if(is.na(idir))
			stop(paste(sep = "", "No section \"", section, 
				"\" in the library ", ifelse(length(lib.loc) > 
				1, "directories: ", "directory: "), paste(
				lib.loc, collapse = ", ")))
		dir <- dir[idir]
		lib.loc <- lib.loc[idir]	
	# now we know dir/.Data is a directory, length(dir)==length(lib.loc)==1
		if(!missing(help)) {
			readme <- paste(dir, "README.txt", sep = "\\")
			if(access(readme, 4) == 0)
				win3(paste("notepad ", readme, sep = ""))
			else stop(paste("No README file for section \"", 
				  section, "\" in library \"", lib.loc, "\"", 
				  sep = ""))
			pos <- NULL	# nothing attached
		}
		else {
			name <- paste(dir, "_Data", sep = "\\")	
	# we've already checked that name is a directory
			if(!(pos <- match(name, search(), nomatch = F))) {
				pos <- if(first) 2 else length(search()) + 1
				attach(name, pos = pos)
				if(exists(".First.lib", where = pos, mode = 
				  "function")) {
				  .First.lib <- get(".First.lib", where = pos, 
				    mode = "function")	
	# could use get(".First.lib", ...)(...) but then any error messages would look odd
				  .First.lib(lib.loc, section)
				}
			}
# else { warning(paste("library already attached")) }
		}
	}
	invisible(pos)	
	# position section attached at, or NULL if we just printed the README file
}
