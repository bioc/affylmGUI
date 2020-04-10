#  UTILITY FUNCTIONS


ALGlimmaUsersGuide <- function(view=TRUE)
#	Find and optionally view limma User's Guide
#	Gordon Smyth
#	25 Oct 2004.
{
	f <- system.file("doc","usersguide.pdf",package="limma")
	if(view) {
		if(.Platform$OS.type == "windows")
			shell.exec(f)
		else
			system(paste(Sys.getenv("R_PDFVIEWER"),f,"&"))
	}
	return(f)
}

ALGchangeLog <- function(n=20)
#	Write first n lines of affylmGUI changelog
#	Gordon Smyth
#	20 Sep 2004.  Last modified 30 Oct 2004.
{
	writeLines(readLines(system.file("doc","changelog.txt",package="affylmGUI"),n=n))
}

.cdfName2AnnPkg <- function(x)
#	Construct probe annotation package names from cdfName
#	Gordon Smyth
#	Created 10 April 2020
{
	paste0(x,".db")
}
