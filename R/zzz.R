.onAttach <- function(libname, pkgname)
#	Package startup messages, Rgui menu items and some checking
{
	if(!interactive()) {
		packageStartupMessage("affylmGUI is launched only in interactive sessions")
		return()
	}

	if(!capabilities(what="tcltk"))	stop("R needs to be built with tcltk capabilities. Install a version of R which has this capability. MS Windows binary versions of R include tcltk capabilities. MAC OS X binary versions of R include tcltk capabilities. When source code versions of R are compiled the default value for the --with-tcltk switch is yes. Do not set it to no. Check your version of R with the capabilities() command.")

	if ("limmaGUI" %in% .packages()) stop("Please run limmaGUI and affylmGUI in separate R sessions.")

	if ((.Platform$OS.type=="windows") && (.Platform$GUI == "Rgui")) {
		winMenuAdd("affylmGUI")
		winMenuAddItem("affylmGUI","affylmGUI","affylmGUI()")
		packageStartupMessage("\nTo begin, type affylmGUI() or use the pull-down menu.\n", appendLF = FALSE)
	}else{
		packageStartupMessage("\nTo begin, type affylmGUI()\n", appendLF = FALSE)
	}

#	I (Gordon) am not sure why the startup question box is only for Windows
	if (.Platform$OS.type=="windows"){
		BeginAffyLimmaGUI <- tclvalue(tkmessageBox(title="affylmGUI",message="Begin affylmGUI?",type="yesno",icon="question"))
		if (BeginAffyLimmaGUI=="yes"){
			affylmGUI()
		}else{
			bringToTop(-1)
		}
	}

	return()
}
