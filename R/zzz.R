
.First.lib <- function(libname, pkgname, where) 
{
     capable <- capabilities()
     if(!capable["tcltk"]){
         stop(paste("The tcl/tk library is not available in your system.",
                    "Download/install the tcltk library from",
                    "www.tcl.tk/software/tcltk/"))
     }else{
         if(interactive()){
             out <- paste("Package tcltk not able to be loaded!")
             if (.Platform$OS.type == "windows")
                 out <- paste(out,"\nThe most likely cause of this",
                              "is that your Tcl/Tk installation is",
                              "misconfigured\nPlease see the R",
                              "Windows FAQ, question 3.6:\n",
                              "http://cran.r-project.org/bin/windows/contrib/rw-FAQ.html#Package%20TclTk%20does%20not%20work.")

             require("tcltk", character.only = TRUE) || stop(out)
         }
     }

	if (require(limma)==FALSE)
	{
		  if (interactive())
			  tkmessageBox(title="An error has occured!",message=paste("Cannot find package limma"),icon="error",type="ok")
			stop("Cannot find package limma")
	}

  if (interactive())
  {
		if (.Platform$OS.type=="windows")
		{
			regPath  <- "HKEY_CURRENT_USER\\SOFTWARE\\ActiveState\\ActiveTcl"
			regPath2 <- "HKEY_LOCAL_MACHINE\\SOFTWARE\\ActiveState\\ActiveTcl"
			if(inherits(try(TclVersion <- tclvalue(tkcmd("registry","get",regPath,"CurrentVersion")),TRUE),"try-error")&&
				 inherits(try(TclVersion2 <- tclvalue(tkcmd("registry","get",regPath2,"CurrentVersion")),TRUE),"try-error"))
			{
				cat(paste("\nWarning: ActiveTcl could not be found in the Windows Registry.\n"))
				cat(paste("\nEither it has not been installed or it has not been installed with sufficient privileges.\n\n"))
				cat(paste("\naffylmGUI requires the Tcl/Tk extensions Tktable and BWidget which are not distributed with R,\n"))
				cat(paste("\nbut they are distributed with ActiveTcl.\n"))
			}
			else
			{
				if (!inherits(try(TclVersion <- tclvalue(tkcmd("registry","get",regPath,"CurrentVersion")),TRUE),"try-error"))
				{
					regPath <- paste(regPath,TclVersion,sep="\\")
					TclPath <-  tclvalue(tkcmd("registry","get",regPath,""))
					cat(paste("\nActiveTcl was found in the Windows Registry (for CURRENT_USER), installed in",TclPath,sep="\n"))
					cat(paste("\nThis directory will be added to the Tcl search path to enable affylmGUI\n"))
					cat(paste("to find the Tktable and BWidget extensions.\n"))
					addTclPath(paste(gsub("\\\\","/",TclPath),"lib",sep="/"))
				}
				if (!inherits(try(TclVersion2 <- tclvalue(tkcmd("registry","get",regPath2,"CurrentVersion")),TRUE),"try-error"))      
				{
					regPath2 <- paste(regPath2,TclVersion2,sep="\\")
					TclPath2 <-  tclvalue(tkcmd("registry","get",regPath2,""))
					cat(paste("\nActiveTcl was found in the Windows Registry (for LOCAL_MACHINE), installed in",TclPath2,sep="\n"))
					cat(paste("\nThis directory will be added to the Tcl search path to enable affylmGUI\n"))
					cat(paste("to find the Tktable and BWidget extensions.\n"))
					addTclPath(paste(gsub("\\\\","/",TclPath2),"lib",sep="/"))
				}
			}
		}


		if ((.Platform$OS.type=="windows")&&(.Platform$GUI == "Rgui"))
		{
			winMenuAdd("affylmGUI");winMenuAddItem("affylmGUI","affylmGUI","affylmGUI()")
			cat(paste("\nTo begin, type affylmGUI() or use the pull-down menu.\n"))
		}
		else
			cat(paste("\nTo begin, type affylmGUI()\n"))

    # I only get .First.lib to ask the user whether they want to start the GUI with
    # a message box for the Windows OS.  I encountered some problems under linux
    # for the case where the Tcl/Tk extensions can't be found (so affylmGUI tries
    # to exit), and speculated that there could be problems arising from running
    # the whole affylmGUI() program before finishing .First.lib.
    if (interactive() && .Platform$OS.type=="windows")
    {
      BeginAffyLimmaGUI <- tclvalue(tkmessageBox(title="affylmGUI",message="Begin affylmGUI?",type="yesno",icon="question"))
      if (BeginAffyLimmaGUI=="yes") 
        affylmGUI()
      else
        bringToTop(-1)
    }
  }
}


