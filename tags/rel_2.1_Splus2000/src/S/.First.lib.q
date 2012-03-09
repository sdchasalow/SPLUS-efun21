# $Id$

".First.lib"<-
function(library, section)
{
# Copied from Hmisc .First.lib (by Frank Harrell)
# SDC: added cat("") to action; invisible() doesn't work.
#
# SDC, 27 May 1998: Added check to make sure menu item is not
# already present. This is in case the efun module is attached, 
# detached, and re-attached during a single S-PLUS session.
#
## Change all \\ in library to / for winhelp to be able to find .hlp
	library2 <- as.character(library)
	w <- substring(library2, 1:nchar(library2), 1:nchar(library2))
	w[w == "\\"] <- "/"
	library2 <- paste(w, collapse = "")
	ds <- paste(library2, section, sep = "/")	
	## Bug in Splus 4.0 beta3 prevents add.menu.item from working
	if(version$major > 3) {
		cat("Type help(library=\"efun21\") to open help", 
			"window for efun21\n\n")
	}
	else if(is.null(get.menu.item("Ef&un Help"))) {
		add.menu.item(name = "Ef&un Help", action = paste(
			"win3('winhelp ", ds, "/", section, 
			".hlp', translate=T, ", "multi=T); cat(\"\")", sep = ""
			))
	}
	invisible()
}
