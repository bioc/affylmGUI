.onAttach <- function(libname, pkgname){
	if (length(grep("^limmaGUI$", .packages()))>0){
	 stop("Please run limmaGUI and affylmGUI in separate R sessions.")
	}
	if (length(grep("^limmaGUI$", .packages()))>0){
	 stop("Please run limmaGUI and affylmGUI in separate R sessions.")
	}
	#Check whether R has been built with tcltk capabilities.
	capable <- capabilities()
	#the output of this command is like this:
	#    jpeg      png     tiff    tcltk      X11     aqua http/ftp  sockets   libxml     fifo   cledit    iconv      NLS  profmem    cairo 
	#    TRUE     TRUE     TRUE     TRUE    FALSE    FALSE     TRUE     TRUE     TRUE    FALSE     TRUE     TRUE     TRUE     TRUE     TRUE 
	#
	if(!capable["tcltk"]){
		stop(paste("R needs to be built with tcltk capabilities.",
			   "Install a version of R which has this capability.",
			   "MS Windows binary versions of R include tcltk capabilities.",
			   "MAC OS X binary versions of R include tcltk capabilities.",
			   "When source code versions of R are compiled the default value ",
			   "for the --with-tcltk switch is yes. Do not set it to no.", 
			   "Check your version of R with the \"capabilities()\" command."
			  )
		    )
	}else{ 
		#R has tcltk capabilities
	} #end of else/if(!capable["tcltk"])
	#
	#
	#
	if (interactive()){
		if ((.Platform$OS.type=="windows")&&(.Platform$GUI == "Rgui")){
			winMenuAdd("affylmGUI");winMenuAddItem("affylmGUI","affylmGUI","affylmGUI()")
			packageStartupMessage("\nTo begin, type affylmGUI() or use the pull-down menu.\n", appendLF = FALSE)
			#cat(paste("\nTo begin, type affylmGUI() or use the pull-down menu.\n"))
		}else{
			packageStartupMessage("\nTo begin, type affylmGUI()\n", appendLF = FALSE)
			#cat(paste("\nTo begin, type affylmGUI()\n"))
		} #end of if ((.Platform$OS.type=="windows")&&(.Platform$GUI == "Rgui"))
		#
		if (interactive() && .Platform$OS.type=="windows"){
			BeginAffyLimmaGUI <- tclvalue(tkmessageBox(title="affylmGUI",message="Begin affylmGUI?",type="yesno",icon="question"))
			if (BeginAffyLimmaGUI=="yes"){
				affylmGUI()
			}else{
				bringToTop(-1)
			}
		} #end of if (interactive() && .Platform$OS.type=="windows")
	} #end of if (interactive())
} #end of.onAttach <- function(libname, pkgname)
