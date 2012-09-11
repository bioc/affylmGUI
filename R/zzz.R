###.First.lib <- function(libname, pkgname)<--opening brace here
###.onLoad <- function(libname, pkgname)<--opening brace here
.onAttach <- function(libname, pkgname){
	if (length(grep("^limmaGUI$", .packages()))>0){
	 stop("Please run limmaGUI and affylmGUI in separate R sessions.")
	}
	if (length(grep("^limmaGUI$", .packages()))>0){
	 stop("Please run limmaGUI and affylmGUI in separate R sessions.")
	}
	#capable <- capabilities()
	#if(!capable["tcltk"]){
	#	stop(paste("The tcl/tk library is not available in your system.",
	#							"Download/install the tcltk library from",
	#							"www.tcl.tk/software/tcltk/"))
	#}else{ ###KS-why is this like this?
	#	if(interactive()){
	#		out <- paste("Package tcltk not able to be loaded!")
	#		require("tcltk", character.only = TRUE) || stop(out)
	#	} #end of if(interactive())
	#} #end of else/if(!capable["tcltk"])
	#
	#if(require(limma)==FALSE){
	#	if(interactive()){
	#		tkmessageBox(title="An error has occured!",message=paste("Cannot find package limma"),icon="error",type="ok")
	#	}
	#	stop("Cannot find package limma")
	#} #end of if (require(limma)==FALSE)
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
	#	# I only get .First.lib to ask the user whether they want to start the GUI with
	#	# a message box for the Windows OS.  I encountered some problems under linux
	#	# for the case where the Tcl/Tk extensions can't be found (so affylmGUI tries
	#	# to exit), and speculated that there could be problems arising from running
	#	# the whole affylmGUI() program before finishing .First.lib.
		if (interactive() && .Platform$OS.type=="windows"){
			BeginAffyLimmaGUI <- tclvalue(tkmessageBox(title="affylmGUI",message="Begin affylmGUI?",type="yesno",icon="question"))
			if (BeginAffyLimmaGUI=="yes"){
				affylmGUI()
			}else{
				bringToTop(-1)
			}
		} #end of if (interactive() && .Platform$OS.type=="windows")
	} #end of if (interactive())
} #end of .onLoad <- function(libname, pkgname)
