# $Id$

"edwin.open"<-
function(name)
{
# DATE WRITTEN:  07 Feb 1997 		 LAST REVISED:  07 Feb 1997
# AUTHOR:  Scott Chasalow (Scott.Chasalow@users.pv.wau.nl)
# OS:  UNIX
#
# DESCRIPTION:
#       Test whether an edit window opened by function efun for the function 
#       given by argument "name" is currently open.
#
#       If yes, return TRUE, else return FALSE.
#
#       NOTE: This function is not currently available for Splus for Windows.
# 
	user <- getenv("USER")
	cmd1 <- "test 0 -eq"
	pipec1 <- "ps -auxw"
	pipec2 <- paste("grep 'xterm -ls -title ", name, " '", sep = "")	
	# NOTE: in pipec2, a space is very important between name and "'"!
	pipec3 <- "grep -v 'grep'"
	pipec4 <- paste("grep -c '", user, " '", sep = "")
	cmd2 <- paste(pipec1, pipec2, pipec3, pipec4, sep = " | ")
	cmd3 <- "echo stopit"
	cmd <- paste(cmd1, " `", cmd2, "` ", "|| ", cmd3, sep = "")
	length(unix(cmd, output = T)) > 0
}
