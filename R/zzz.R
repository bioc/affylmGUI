
.First.lib <- function(libname, pkgname, where) 
{
	require(tcltk) || stop ("Cannot find package tcltk")
	if (data.class(try(require(limma),TRUE))=="try-error")
	{
			tkmessageBox(title="An error has occured!",message=paste("Cannot find package limma"),icon="error",type="ok")
			stop("Cannot find package limma")
	} 
	if (require(limma)==FALSE)
	{
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


		if (.Platform$OS.type=="windows")
		{
			winMenuAdd("affylmGUI");winMenuAddItem("affylmGUI","affylmGUI","affylmGUI()")
			cat(paste("\nTo begin, type affylmGUI() or use the pull-down menu.\n"))
		}
		else
			cat(paste("\nTo begin, type affylmGUI()\n"))

		BeginAffyLimmaGUI <- tclvalue(tkmessageBox(title="affylmGUI",message="Begin affylmGUI?",type="yesno",icon="question"))
		if (BeginAffyLimmaGUI=="yes") 
			affylmGUI()
		else
			if (.Platform$OS.type=="windows")
				bringToTop(-1)
  }
}


