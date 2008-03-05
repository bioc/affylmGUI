if(require(limma)==FALSE){
	if(interactive()){
		tkmessageBox(
			title="An error has occured!",
			message=paste("Cannot find package limma"),
			icon="error",
			type="ok"
		)
	} #end of if(interactive())
	stop("Cannot find package limma")
} #end of if(require(limma)==FALSE)
#
#
#
if(require(affy)==FALSE){
	if(interactive()){
		tkmessageBox(
			title="An error has occured!",
			message=paste("Cannot find package affy"),
			icon="error",
			type="ok"
		)
	} #end of if(interactive())
	stop("Cannot find package affy")
} #end of if(require(affy)==FALSE)
#
#
#
Try <- function(expr){
	if(data.class(result<-try(expr,TRUE))=="try-error"){
		tkmessageBox(
			title="An error has occured!",
			message=as.character(result),
			icon="error",
			type="ok"
		)
	}else{
		return (result)
	}
} #end of Try <- function(expr)
#
#
#
TryReadImgProcFile <- function(expr){
	if(data.class(result<-try(expr,TRUE))=="try-error"){
		tkmessageBox(
			title="Reading Image Processing Files Failed!",
			message="affylmGUI was unable to read the CEL files listed in the Targets file.",
			icon="error",
			type="ok"
		)
	}else{
		return (result)
	}
} #end of TryReadImgProcFile <- function(expr)
#
#
#
Require <- function(pkg){
	if(data.class(result<-try(.find.package(pkg),TRUE))=="try-error"){
		tkmessageBox(title="An error has occured!",message=paste("Cannot find package",pkg),icon="error",type="ok")
	}else{
		result <- Try(require(pkg,character.only=TRUE))
	} #end of else/if(data.class(result<-try(.find.package(pkg),TRUE))=="try-error")
	return (result)
} #end of Require <- function(pkg)
#
#
#
TclRequire <- function(tclPkg){
	if((data.class(result<-try(tclRequire(tclPkg),TRUE))=="try-error") || (is.logical(result) && result==FALSE)){
		affylmGUIglobals <- .affylmGUIglobals
		affylmGUIglobals$TclRequireFailed <- TRUE
		assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv)
		Try(winTitle<-"Tcl/Tk Extension(s) Not Found")
		Try(
			message<-paste(
				"Cannot find Tcl/Tk package \"", tclPkg,
				"\".	affylmGUI cannot continue.\n\n",
				"affylmGUI requires the Tcl/Tk extensions, BWidget and Tktable.\n",
				"You must have Tcl/Tk installed on your computer, not just the minimal\n",
				"Tcl/Tk installation which comes with R (for Windows).	If you do have\n",
				"Tcl/Tk installed, including the extensions (e.g. using the ActiveTcl\n",
				"distribution in Windows), make sure that R can find the path to the\n",
				"Tcl library, e.g. C:\\Tcl\\lib (on Windows) or /usr/lib (on Linux/Unix)\n",
				"or /sw/lib on Mac OSX.\n\n",
				"If you don't know how to set environment variables in Windows, one way\n",
				"to make sure that R can find the Tcl/Tk extensions Tktable2.8 and bwidget1.6\n",
				"is to copy them from your ActiveTcl installation e.g. in C:\\Tcl\\lib into\n",
				"the Tcl subdirectory of your R installation.\n",
				"If you do understand how to set environment variables...\n",
				"make sure that you have the TCL_LIBRARY environment variable set to the\n",
				"appropriate path, e.g.C:\\Tcl\\lib\\tcl8.4 and the MY_TCLTK environment\n",
				"variable set to a non-empty string, e.g. \"Yes\".\n\n",
				"If using Windows, be sure to read the R for windows FAQ at\nhttp://www.stats.ox.ac.uk/pub/R/rw-FAQ.html\n\n",
				"If your Tcl/Tk extensions still can't be found, try\n",
				"addTclPath(\"<path to Tcl library>\").\nThis could be put in $HOME/.Rprofile\n\n",
				"If you need further instructions, please contact your system administrator\n",
				"and consider emailing r-help@stat.math.ethz.ch, or browse through the R-help\n",
				"archives for a similar question.\n\n",
				"The URLs for Tktable and BWidget are:\n",
				"http://tktable.sourceforge.net\n",
				"http://tcllib.sourceforge.net",
				sep=""
			) #end of message<-paste
		) #end of Try
		#
		# Don't make ttMain a parent of this, because we might want to use TclRequire before
		# defining ttMain.
		Try(ttTclTkExtension <- tktoplevel())
		onDestroy <- function(){
			if(exists(".affylmGUIglobals",envir=.GlobalEnv)&&"ttMain" %in% names(.affylmGUIglobals)){
				try(tkdestroy(.affylmGUIglobals$ttMain),silent=TRUE)
			}else{
				stop("Tcl/Tk extensions (Tktable and BWidget) not found!")
			}
			stop("Aborted from affylmGUI.")
		} #end of onDestroy <- function()
		Try(tkbind(ttTclTkExtension, "<Destroy>", onDestroy))
		Try(tkwm.title(ttTclTkExtension,winTitle))
		Try(tkwm.deiconify(ttTclTkExtension))
		Try(scr <- tkscrollbar(ttTclTkExtension, repeatinterval=5,command=function(...)tkyview(txt,...)))
		Try(txt <- tktext(ttTclTkExtension,bg="white",yscrollcommand=function(...)tkset(scr,...)))
		Try(tkgrid(txt,scr,columnspan=2))
		Try(tkgrid.configure(scr,columnspan=1,sticky="ns"))
		Try(tkgrid.configure(txt,sticky="nsew"))
		Try(tkinsert(txt,"end",message))
		Try(tkconfigure(txt, state="disabled"))
		Try(tkfocus(txt))
		Try(
			onOK <- function(){
				try(tkdestroy(ttTclTkExtension),silent=TRUE)
				if(exists(".affylmGUIglobals",envir=.GlobalEnv)&&"ttMain" %in% names(.affylmGUIglobals)){
					try(tkdestroy(.affylmGUIglobals$ttMain),silent=TRUE)
				}else{
					stop("Tcl/Tk extensions (Tktable and BWidget) not found!")
				}
				Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
				Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
				if(limmaDataSetNameText!="Untitled"){
					Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
					Try(
						mbVal <- tkmessageBox(
							title  = "Aborting from affylmGUI",
							message= paste("Save changes to ",fixSeps(LimmaFileName),"?",sep=""),
							icon   = "question",type="yesno",default="yes"
						) #end of mbVal <- tkmessageBox
					) #end of Try
					try(if(tclvalue(mbVal)=="yes"){try(SaveLimmaFile(),silent=TRUE)},silent=TRUE)
				} #end of if(limmaDataSetNameText!="Untitled")
				stop("Tcl/Tk extensions (Tktable and BWidget) not found!")
			} #end of onOK <- function()
		) #end of Try
		Try(OK.but <- tkbutton(ttTclTkExtension,text="  OK  ",command=onOK))
		#
		Try(tkgrid.configure(txt,columnspan=2))
		Try(tkgrid          (tklabel(ttTclTkExtension,text="    "                    )             ))
		Try(tkgrid          (tklabel(ttTclTkExtension,text="affylmGUI will now exit."),columnspan=2))
		Try(tkgrid          (tklabel(ttTclTkExtension,text="    "                    )             ))
		Try(tkgrid          (OK.but           ))
		Try(tkgrid.configure(OK.but,sticky="e"))
		Try(tkgrid          (tklabel(ttTclTkExtension,text="    "                    )             ))
		Try(tkfocus         (OK.but           ))
		Try(tkwait.window   (ttTclTkExtension ))
	} #end of if((data.class(result<-try(tclRequire(tclPkg),TRUE))=="try-error") || (is.logical(result) && result==FALSE))
} #end of TclRequire <- function(tclPkg)
#
#
#
fixSeps <- function(string){
	Try(if(.Platform$OS.type=="windows")
		string <- gsub("/","\\\\",string))
	return (string)
} #end of fixSeps <- function(string)
#
#
#
affylmGUIhelp <- function(){
	Try(affylmGUIhelpIndex <- file.path(system.file("doc",package="affylmGUI"),"index.html"))
	Try(browseURL(affylmGUIhelpIndex))
	##Try(tkmessageBox(title="affylmGUI Help",message=paste("Opening affylmGUI help...\nIf nothing happens, please open :\n",affylmGUIhelpIndex,"\nyourself.",sep="")))
	Try(cat(paste("Opening affylmGUI help...\nIf nothing happens, please open :\n",affylmGUIhelpIndex,"\nyourself.",sep="")))
} #end of affylmGUIhelp <- function()
#
#
#
limmaHelp <- function(){
	Try(limmaHelpIndex <- file.path(system.file("doc",package="limma"),"index.html"))
	Try(browseURL(limmaHelpIndex))
	##Try(tkmessageBox(title="limma Help",message=paste("Opening limma help...\nIf nothing happens, please open :\n",limmaHelpIndex,"\nyourself.",sep="")))
	Try(cat(paste("Opening limma help...\nIf nothing happens, please open :\n",limmaHelpIndex,"\nyourself.",sep="")))
} #end of limmaHelp <- function()
#
#
#
affyHelp <- function(){
	Try(affyHelpIndex <- paste(system.file("doc",package="affy"),"affy.pdf",sep="/"))
	Try(browseURL(affyHelpIndex))
	##Try(tkmessageBox(title="affy Help",message=paste("Opening affy help...\nIf nothing happens, please open :\n",affyHelpIndex,"\nyourself.",sep="")))
	Try(cat(paste("Opening affy help...\nIf nothing happens, please open :\n",affyHelpIndex,"\nyourself.",sep="")))
} #end of affyHelp <- function()
#
#
#
showCitations <- function(){
	Try(print(citation("affylmGUI"))) #Put this on the R Console first.
	#citationOutput is the ouput from the citation("affylmGUI") command given at the R prompt.
	citationOutput <-
	"
	affylmGUI is an implementation of a body of methodological research by the authors and coworkers. Please cite the
	appropriate methodological papers whenever you use results from the limma software in a publication. Such citations
	are the main means by which the authors receive professional credit for their work.

	Citing limma and affylmGUI in publications will usually involve citing one or more of the methodology papers that the
	limma software is based on as well as citing the limma(Ref.2) and affylmGUI(Ref.5) software packages themselves.

	If you use limma/affylmGUI for differential expression analysis, please cite reference 1 which describes the linear
	modeling approach implemented by lmFit and the empirical Bayes statistics implemented by eBayes, topTable etc.

	To cite the limma software itself please refer to reference 2 which describes the software package in the context of
	the Bioconductor project and surveys the range of experimental designs for which the package can be used, including
	spotspecific dye-effects. The pre-processing capabilities of the package are also described but more briefly, with
	examples of background correction, spot quality weights and filtering with control spots. This article is also the
	best current reference for the normexp background correction method.

	To cite the GC robust multiarray average (GCRMA) background correction method please refer to citation 3.

	To cite the robust multiarray average (RMA) background correction method please refer to citation 4.

		1. Smyth, G. K. (2004). Linear models and empirical Bayes methods for assessing differential expression in
		microarray experiments. Statistical Applications in Genetics and Molecular Biology Vol. 3, No. 1, Article 3.

		2. Smyth, G. K. (2005). Limma: linear models for microarray data. In: 'Bioinformatics and Computational Biology
		Solutions using R and Bioconductor'. R. Gentleman, V. Carey, S. Dudoit, R. Irizarry, W. Huber (eds), Springer, New
		York, pages 397-420.

		3. Zhijin Wu1, Rafael A. Irizarry, Robert Gentleman, Francisco Martinez-Murillo, Forrest Spencer. (2004). A Model
		Based Background Adjustment for Oligonucleotide Expression Arrays In the Journal of the American Statistical
		Association. Volume 99, Pages 909-917.

		4. Rafael A. Irizarry, Bridget Hobbs, Francois Collin, Yasmin D. Beazer-Barclay, Kristen J. Antonellis, Uwe Scherf,
		Terence P. Speed. (2003). Exploration, normalization, and summaries of high density oligonucleotide array probe
		level data In the Journal Biostatistics. Volume 4(2), Pages 249-264.

		5. James M. Wettenhall, Ken M. Simpson, Keith Satterley and Gordon K. Smyth. affylmGUI: a graphical user interface
		for linear modeling of single channel microarray data. Bioinformatics, 22:897-899, 2006.
	"
	#
	citationNote <- "\nThis information is also displayed on the R console, where you may select and copy it."
	#
	citationMessage <- paste(citationOutput,citationNote,sep="")
	Try(tkmessageBox(title="Citations",message=citationMessage))
} #end of showCitations <- function()
#
#
#
showChangeLog <- function(n=20){
	Try(tkmessageBox(title="ChangeLog",message="See the R console for the first 20 lines of the ChangeLog file.\nTo see more lines, use the ALGchangeLog(n=nnn) function, where nnn is the number of lines to view."))
	Try(ALGchangeLog(20))
} #end of showChangeLog <- function(n=20)
#
#
#
Try(
	onDestroy <- function(){
		Try(.JustAskedWhetherToSave <- get(".JustAskedWhetherToSave",envir=.GlobalEnv))
		Try(if(.JustAskedWhetherToSave==FALSE)
		{
			Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
			Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
			if(limmaDataSetNameText!="Untitled")
			{
				Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
				Try(mbVal <- tkmessageBox(title="Aborting from affylmGUI",
							message=paste("Save changes to ",fixSeps(LimmaFileName),"?",sep=""),
							icon="question",type="yesno",default="yes"))
				try(if(tclvalue(mbVal)=="yes")
						try(SaveLimmaFile(),silent=TRUE),silent=TRUE)
			}
			Try(assign(".JustAskedWhetherToSave",TRUE,.GlobalEnv))
		})
	}#end of onDestroy
)#end of Try
#
#
#
affylmGUI <- function(BigfontsForaffylmGUIpresentation=FALSE){
	assign("affylmGUIenvironment",new.env(),.GlobalEnv)
	assign("Try",get("Try",envir=.GlobalEnv),affylmGUIenvironment)
	# This option is for when I give a Presentation/talk on affylmGUI and want large affylmGUIfonts.	Currently, there are
	# some affylmGUIfonts which affylmGUI can't control, like menus, so as well as changing .affylmGUIglobals$affylmGUIpresentation to TRUE here, I
	# Right-Click the Windows Desktop, click Properties (to get Display properties which can also be accessed
	# through the Control Panel) then click on Appearance, and then change the affylmGUIfont size for menu,window title, etc.)
	# Rather than change each affylmGUIfont (menu,window title,...) manually each time, I save the changes as a "scheme".
	Try(affylmGUIglobals <- list())
	Try(if(BigfontsForaffylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIpresentation <- TRUE)
	else
		Try(affylmGUIglobals$affylmGUIpresentation <- FALSE))
	Try(affylmGUIglobals$limmaDataSetNameTcl <- tclVar("Untitled"))
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))

	Try(initGlobals())

	Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
	Try(affylmGUIglobals$graphicsDevice <- "tkrplot")
	Try(if(Sys.info()["sysname"]=="Darwin")
		Try(affylmGUIglobals$graphicsDevice <- "R"))
	Try(affylmGUIglobals$Myhscale <- 1)
	Try(affylmGUIglobals$Myvscale <- 1)
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))

	Try(if(exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
		{
			Try(if(Sys.info()["sysname"]=="Darwin")
				{
					Try(addTclPath("/sw/lib/tcl8.4"))
					Try(addTclPath("/sw/lib"))
					Try(addTclPath("./lib"))
					Try(addTclPath("/sw/lib/tk8.4"))
					Try(addTclPath(paste(Sys.getenv("HOME"),.Platform$file.sep,"TkExtensions",sep="")))
				}
			)#end of Try(if(Sys.info()["sysname"]=="Darwin"))
			Try(addTclPath("/usr/local/lib"))
			Try(addTclPath("/usr/local/Tcl/lib"))
			Try(addTclPath("/usr/local/lib/Tcl"))
			Try(addTclPath("/usr/lib"))
			Try(addTclPath("/usr/lib/Tcl"))
			Try(addTclPath("/usr/local/ActiveTcl/lib"))
			Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
			Try(affylmGUIglobals$Myhscale <- 1)
			Try(affylmGUIglobals$Myvscale <- 1)
			Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
		}#end of if(exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
	)#end of Try(if(Sys.info...))

	Try(if(Sys.info()["sysname"] == "Windows")
		{
		 Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
		 Try(affylmGUIglobals$Myhscale <- 1.6)
		 Try(affylmGUIglobals$Myvscale <- 1.6)
		 Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
		}#end of if
	)#end of Try

	Try(if(Sys.info()["sysname"] == "Darwin" && !exists("X11", env=.GlobalEnv))
		{
			Try(addTclPath("/Library/Tcl"))
			Try(addTclPath("/Network/Library/Tcl"))
			Try(addTclPath("/System/Library/Tcl"))
			Try(addTclPath("/Library/Frameworks/Tcl"))
			Try(HOME <- Sys.getenv("HOME"))
			Try(if(nchar(HOME)>0)
				{
					Try(addTclPath(paste(HOME,"/Library/Tcl",sep="")))
					Try(addTclPath(paste(HOME,"/Network/Library/Tcl",sep="")))
					Try(addTclPath(paste(HOME,"/System/Library/Tcl",sep="")))
					Try(addTclPath(paste(HOME,"/Library/Frameworks/Tcl",sep="")))
				}
			)
			Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
			Try(affylmGUIglobals$Myhscale <- 1)
			Try(affylmGUIglobals$Myvscale <- 1)
			Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
		}#end of if
	)#end of Try(if(Sys.info()["sysname"] == "Darwin" && !exists("X11", env=.GlobalEnv))...)

	Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfont1	<- tkfont.create(family="times",size=48,weight="bold",slant="italic"))
	else
		Try(affylmGUIglobals$affylmGUIfont1	<- tkfont.create(family="times",size=24,weight="bold",slant="italic"))
	)#end of Try
	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfont2 <- tkfont.create(family="arial",size=16))
	else
		Try(affylmGUIglobals$affylmGUIfont2 <- tkfont.create(family="arial",size=10))
	)#end of Try
	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfontTree <- tkfont.create(family="arial",size=14))
	else
		Try(affylmGUIglobals$affylmGUIfontTree <- tkfont.create(family="arial",size=10))
	)#end of Try

	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfontTable <- tkfont.create(family="arial",size=16))
	else
		Try(affylmGUIglobals$affylmGUIfontTable <- tkfont.create(family="arial",size=10))
	)#end of Try
	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfontTopTable <- tkfont.create(family="arial",size=12,weight="bold"))
	else
		Try(affylmGUIglobals$affylmGUIfontTopTable <- affylmGUIglobals$affylmGUIfontTable)
	)#end of Try

	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfont2b <- tkfont.create(family="arial",size=16,weight="bold"))
	else
		Try(affylmGUIglobals$affylmGUIfont2b <- tkfont.create(family="arial",size=10,weight="bold"))
	)#end of Try

	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$affylmGUIfontCourier <- tkfont.create(family="courier",size=16))
	else
		Try(affylmGUIglobals$affylmGUIfontCourier <- tkfont.create(family="courier",size=10))
	)#end of Try

	Try(affylmGUIglobals$mainTreeWidth <- 30)

	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(affylmGUIglobals$ContrastParameterizationTREEWidth <- 40)
	else
		Try(affylmGUIglobals$ContrastParameterizationTREEWidth <- 30)
	)#end of Try

	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		{
			Try(affylmGUIglobals$ContrastParameterizationTREEHeight <- 20)
			Try(affylmGUIglobals$mainTreeHeight <- 20)
		}
		else
		{
			Try(affylmGUIglobals$ContrastParameterizationTREEHeight <- 15)
			Try(affylmGUIglobals$mainTreeHeight <- 15)
		}
	)#end of Try
	# Try(assign("affylmGUIfontMenu",tkfont.create(family="arial",size=10),.GlobalEnv))

	Try(affylmGUIglobals$ttMain <- tktoplevel())
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Destroy>", onDestroy))

	TclRequire("BWidget")
	if("TclRequireFailed" %in% names(.affylmGUIglobals))
		stop("Error occurred in TclRequire(\"BWidget\")")
	TclRequire("Tktable")
	if("TclRequireFailed" %in% names(.affylmGUIglobals))
		stop("Error occurred in TclRequire(\"Tktable\")")
	#
	# Try(assign("opar",par(bg="white"),.GlobalEnv))
	Try(oldOptions <- options(warn=-1)) # Otherwise R complains that I'm trying to set main in plots, i.e. set a plot title)
	# Maybe it would be nice to eventually use the MainFrame widget from BWidget so we can have a nice toolbar etc.
	#
	Try(if(.affylmGUIglobals$affylmGUIpresentation==FALSE)
		Try(mainFrame <- tkframe(.affylmGUIglobals$ttMain,relief="groove",borderwidth="2"))
	else
		Try(mainFrame <- tkframe(.affylmGUIglobals$ttMain))
	)#end of Try
	Try(if(.affylmGUIglobals$affylmGUIpresentation==FALSE)
		{
			Try(toolbarFrame <- tkframe(mainFrame,relief="groove",borderwidth="2"))
			Try(tb <- tkframe(toolbarFrame,relief="groove",borderwidth="2"))
			# The Bitmap::get stuff below requires the BWidget package.
			# I think this could be done more simply with something like :
			#	 Try(newButton <- tkbutton(tb,image=tkcmd("Bitmap::get"new"),command=NewLimmaFile))
			Try(newButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get new]",.Tcl.args(command=NewLimmaFile))))
			Try(openButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get open]",.Tcl.args(command=OpenLimmaFile))))
			Try(saveButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get save]",.Tcl.args(command=SaveLimmaFile))))
			Try(tkgrid(newButton,openButton,saveButton,sticky="w"))
			Try(tkgrid(tb,sticky="nw"))
		#	Try(tkgrid(toolbarFrame,sticky="ew"))
			Try(tkgrid(toolbarFrame,sticky="w"))
		#	Try(tkgrid.configure(tb,sticky="w"))
		}#end of if
	)#end of Try

	Try(LimmaFileName <- get("LimmaFileName",affylmGUIenvironment))
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
	Try(if(.Platform$OS.type=="windows")
		Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",gsub("/","\\\\",LimmaFileName))))
	else
		Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",LimmaFileName)))
	)#end of Try
	Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
	Try(affylmGUIglobals$CDFfileBoxTitle <- tclVar("Please select a Chip Definition (CDF) file."))
	Try(affylmGUIglobals$CDFfileName <- tclVar("No filename is selected at the moment.	Press the Select CDF File Button."))
	Try(affylmGUIglobals$TargetsfileBoxTitle <- tclVar("Please select a tab-delimited file listing the CEL files."))
	Try(affylmGUIglobals$TargetsfileName <- tclVar("No filename is selected at the moment.	Press the Select Targets File Button."))
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))

	Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=3))
	Try(if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
		Try(tkgrid(tklabel(mainFrame,text="affylmGUI",font=.affylmGUIglobals$affylmGUIfont1),column=1,columnspan=3,sticky="ew"))
	else
		Try(tkgrid(tklabel(mainFrame,text="     affylmGUI ",font=.affylmGUIglobals$affylmGUIfont1),column=2,sticky="ew"))
	)#end of Try
	Try(tkgrid(tklabel(mainFrame,text="Welcome to affylmGUI, a package for Linear Modelling of Microarray Data.\nPlease select the Citations item from the Help Menu for citation information.",font=.affylmGUIglobals$affylmGUIfont2),columnspan=5))
	Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
	Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
	Try(limmaDataSetName.but <- tkbutton(mainFrame,text="Data Set Name",command=GetlimmaDataSetName,font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(limmaDataSetName.but,column=2,columnspan=1))
	Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
	Try(TclRequire("BWidget"))
	Try(mainTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(.affylmGUIglobals$mainTree,...),orient="horizontal"))
	Try(mainTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(.affylmGUIglobals$mainTree,...)))
	Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=.GlobalEnv))
	Try(affylmGUIglobals$mainTree <- tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(mainTreeXScr,...),yscrollcommand=function(...)tkset(mainTreeYScr,...),width=.affylmGUIglobals$mainTreeWidth,height=.affylmGUIglobals$mainTreeHeight,bg="white"))
	Try(LinModTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(.affylmGUIglobals$ContrastParameterizationTREE,...),orient="horizontal"))
	Try(LinModTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(.affylmGUIglobals$ContrastParameterizationTREE,...)))
	Try(affylmGUIglobals$ContrastParameterizationTREE <- tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(LinModTreeXScr,...),yscrollcommand=function(...)tkset(LinModTreeYScr,...),width=.affylmGUIglobals$ContrastParameterizationTREEWidth,height=.affylmGUIglobals$ContrastParameterizationTREEHeight,bg="white"))
	Try(affylmGUIglobals$limmaDataSetNameTextLabel <- tklabel(mainFrame,text=limmaDataSetNameText,font=.affylmGUIglobals$affylmGUIfont2b))
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))

	Try(tkgrid(tklabel(mainFrame,text="    "),.affylmGUIglobals$limmaDataSetNameTextLabel,tklabel(mainFrame,text="    "),tklabel(mainFrame,text="CONTRASTS PARAMETERIZATIONS",font=.affylmGUIglobals$affylmGUIfont2b),tklabel(mainFrame,text="                ")))
	Try(tkgrid(tklabel(mainFrame,text="    "),.affylmGUIglobals$mainTree,mainTreeYScr,.affylmGUIglobals$ContrastParameterizationTREE,LinModTreeYScr))
	Try(tkconfigure(.affylmGUIglobals$limmaDataSetNameTextLabel,textvariable=.affylmGUIglobals$limmaDataSetNameTcl))
	Try(tkgrid.configure(.affylmGUIglobals$mainTree,rowspan=6,sticky="ns"))
	Try(tkgrid.configure(mainTreeYScr,rowspan=6,sticky="wns"))
	Try(tkgrid.configure(.affylmGUIglobals$ContrastParameterizationTREE,rowspan=6,sticky="ns"))
	Try(tkgrid.configure(LinModTreeYScr,rowspan=6,sticky="wns"))
	Try(tkgrid(tklabel(mainFrame,text="    "),mainTreeXScr,tklabel(mainFrame,text="    "),LinModTreeXScr))
	Try(tkgrid.configure(mainTreeXScr,sticky="ewn"))
	Try(tkgrid.configure(LinModTreeXScr,sticky="ewn"))

	Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))

	Try(tkgrid(mainFrame))

	Try(tkinsert(.affylmGUIglobals$mainTree,"end","root","RawAffyData" ,text="Raw Affy Data",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","root","NormalizedAffyData" ,text="Normalized Affy Data",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","root","LinearModelFit" ,text="Linear Model Fit",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","LinearModelFit","LinearModelFit.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","root","Parameters" ,text="Parameters",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","root","ContrastParameterizations" ,text="Contrasts Parameterizations",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
	#
	Try(assign("ArraysLoaded",FALSE,affylmGUIenvironment))
	#
	# Menu code below was taken from Rcmdr (and slightly modified)
	#
	Try(etc <- system.file("etc",package="affylmGUI"))
	Try(cat(paste("\nSearching for user-defined affylmGUI commands in",etc,"...\n")))
	Try(source.files <- list.files(etc, pattern="\\.R$"))
		Try(for (file in source.files) {
				Try(source(file.path(etc, file)))
				Try(cat(paste("Sourced:", file, "\n")))
				})

	#Try(topMenu <- tkmenu(.affylmGUIglobals$ttMain))
	Try(topMenu <- tkmenu(.affylmGUIglobals$ttMain))
	#Try(tkconfigure(.affylmGUIglobals$ttMain,menu=topMenu))
	Try(tkconfigure(.affylmGUIglobals$ttMain,menu=topMenu))
	Try(Menus <- read.table(file.path(system.file("etc",package="affylmGUI"),"affylmGUI-menus.txt"), as.is=TRUE))
	Try(
		for (m in 1:nrow(Menus)){
			Try(if(Menus[m, 1] == "menu"){
					assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE))
				}else if(Menus[m, 1] == "item"){
					if(Menus[m, 3] == "command")
						tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
					else if(Menus[m, 3] == "cascade"){
						cascadeMenu <- eval(parse(text=Menus[m, 5]))
						tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=cascadeMenu)
						if(Menus[m, 4]=="File"){
							Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=affylmGUIenvironment))
							Try(menuNames <- unique(Menus[,2,drop=TRUE]))
							Try(numMenus <- length(menuNames))
							Try(menus <- list())
							Try(for (j in (1:numMenus))
								menus[[j]] <- eval(parse(text=Menus[j,2]))
							)#end of Try
							Try(names(menus) <- menuNames)
							Try(affylmGUIglobals$menus <- menus)
							Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
						}#end of if(Menus[m, 4]=="File")
					}else if(Menus[m, 3] == "separator"){#end of else if(Menus[m, 3] == "cascade")
						if(nrow(Menus)>m && Menus[m+1, 4]=="Exit"){
							recentFilesFileName <- system.file("etc/recent-files.txt",package="affylmGUI")
							recentFiles <- readLines(recentFilesFileName)
							recentFiles <- gsub("\\\\","/",recentFiles)
							# Remove any blank lines:
							blanks <- grep("^[ \t\n]*$",recentFiles)
							if(length(blanks)>0)
								recentFiles <- recentFiles[-blanks]
							numRecentFiles <- length(recentFiles)
							if(numRecentFiles>0){
								tkadd(eval(parse(text=Menus[m, 2])),"separator")
								for (i in (1:numRecentFiles)){
									label <- recentFiles[i]
									fileNameOnly <- strsplit(label,"/")[[1]]
									fileNameOnly <- fileNameOnly[length(fileNameOnly)]
									if(nchar(recentFiles[i])>60)
											label <- paste(".../",fileNameOnly)
									eval(parse(text=paste("assign(\".OpenALimmaFile_",i,"\",function() OpenALimmaFile(\"",recentFiles[i],"\"),.GlobalEnv)",sep="")))
									Try(
										if(.Platform$OS.type=="windows"){
											tkadd(
												eval(parse(text=Menus[m,2])),"command",label=paste(i,". ",gsub("/","\\\\",label),sep=""),
												command=eval(parse(text=paste(".OpenALimmaFile_",i,sep="")))
											)#end of tkadd
										}else{
											tkadd(
												eval(parse(text=Menus[m,2])),"command",label=paste(i,". ",label,sep=""),
												command=eval(parse(text=paste(".OpenALimmaFile_",i,sep="")))
											)#end of tkadd
										}#end of else/if
									)#end of Try
								}#end of for (i in (1:numRecentFiles))
							}#end of if(numRecentFiles>0)
						}#end of if(nrow(Menus)>m && Menus[m+1, 4]=="Exit")
						tkadd(eval(parse(text=Menus[m, 2])),"separator")
					}else{
						stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
					}#end of else if(Menus[m, 3] == "separator")
				}else{
					stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
				}#end of else/if(Menus[m, 1] == "item")
			)#end of Try(if(Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE))...)
		}#end of for (m in 1:nrow(Menus))
	)#end of Try(for (m in 1:nrow(Menus))..)

	Try(affylmGUIglobals <- get(".affylmGUIglobals",envir=affylmGUIenvironment))
	Try(affylmGUIglobals$mainMenu <- topMenu)
	Try(assign(".affylmGUIglobals",affylmGUIglobals,.GlobalEnv))
	#
	Try(
		if(.affylmGUIglobals$affylmGUIpresentation==FALSE){
			Try(labelStatusBar <- tklabel(.affylmGUIglobals$ttMain,font=.affylmGUIglobals$affylmGUIfont2))
			Try(tkgrid(labelStatusBar,sticky="w"))
			Try(CurrentStatus <- tclVar("    "))
			Try(tkconfigure(labelStatusBar,textvariable=CurrentStatus))
			Try(tkbind(saveButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Save the current Limma file."))
			Try(tkbind(saveButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
			Try(tkbind(openButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Open an existing Limma file."))
			Try(tkbind(openButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
			Try(tkbind(newButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Start a new Limma analysis."))
			Try(tkbind(newButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
		}#end of if(.affylmGUIglobals$affylmGUIpresentation==FALSE)
	)#end of Try
	#
	#Try(tkwm.resizable(.affylmGUIglobals$ttMain,"true","false"))
	#
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-N>", NewLimmaFile))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-S>", SaveLimmaFile))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-O>", OpenLimmaFile))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-n>", NewLimmaFile))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-s>", SaveLimmaFile))
	Try(tkbind(.affylmGUIglobals$ttMain, "<Control-o>", OpenLimmaFile))
	#
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(temp <- options(oldOptions))
	invisible()
}#end of affylmGUI <- function(BigfontsForaffylmGUIpresentation=FALSE)
#
#
#
getPackageVersion <- function(pkgName){
	DESCRIPTION <- readLines(paste(system.file(package=pkgName),"/DESCRIPTION",sep=""))
	lineNum <- grep("Version",DESCRIPTION)
	VersionLineWords <- strsplit(DESCRIPTION[lineNum]," ")[[1]]
	numWords <- length(VersionLineWords)
	VersionLineWords[numWords]
} #end of getPackageVersion <- function(pkgName)
#
#
#
initGlobals <- function(){
	assign("affylmGUIVersion",                     getPackageVersion("affylmGUI"), affylmGUIenvironment)
	assign("limmaVersion",                         getPackageVersion("limma"),     affylmGUIenvironment)
	assign("LimmaFileName",                        "Untitled",                     affylmGUIenvironment)
	assign("RawAffyData" ,                         0,                              affylmGUIenvironment)
	assign("CDFFile" ,                             "",                             affylmGUIenvironment)
	assign("ContrastParameterizationList" ,        list(),                         affylmGUIenvironment)
	assign("cdf" ,                                 data.frame(),                   affylmGUIenvironment)
	assign("cdfName" ,                             "",                             affylmGUIenvironment)
	assign("NumSlides" ,                           0,                              affylmGUIenvironment)
	assign("NumContrastParameterizations",         0,                              affylmGUIenvironment)
	assign("ContrastParameterizationNamesVec",     c(),                            affylmGUIenvironment)
	assign("ContrastParameterizationTREEIndexVec", c(),                            affylmGUIenvironment)
	assign("NumParameters" ,                       0,                              affylmGUIenvironment)
	assign("SlideNamesVec" ,                       c(),                            affylmGUIenvironment)
	assign("Targets" ,                             data.frame(),                   affylmGUIenvironment)
	assign("limmaDataSetNameText" ,                "Untitled",                     affylmGUIenvironment)
	Try(tclvalue(.affylmGUIglobals$limmaDataSetNameTcl) <- "Untitled")
	assign("ArraysLoaded",                         FALSE,                          affylmGUIenvironment)
	assign("RawAffyData.Available",                FALSE,                          affylmGUIenvironment)
	assign("NormalizedAffyData.Available",         FALSE,                          affylmGUIenvironment)
	assign("LinearModelFit.Available",             FALSE,                          affylmGUIenvironment)
	assign("numConnectedSubGraphs",                1,                              affylmGUIenvironment)
	assign("connectedSubGraphs",                   list(),                         affylmGUIenvironment)
	assign("NumRNATypes",                          2,                              affylmGUIenvironment)
	assign("NormalizedAffyData",                   0,                              affylmGUIenvironment)
	assign("NormalizedAffyData.exprs",             0,                              affylmGUIenvironment)
	assign("NormalizedAffyData.se.exprs",          NULL,                           affylmGUIenvironment)
	assign("geneNames",                            c(),                            affylmGUIenvironment)
	assign("geneSymbols",                          c(),                            affylmGUIenvironment)
	assign("NormMethod",                           "RMA",                          affylmGUIenvironment)
	assign("numberOfGenes",                        50,                             affylmGUIenvironment)
	assign("sortBy",                               "B",                            affylmGUIenvironment)
	assign("adjustMethod",                         "BH",                           affylmGUIenvironment)
	assign("weightsPLM",                           data.frame(),                   affylmGUIenvironment)
	assign(".JustAskedWhetherToSave",              FALSE,                          .GlobalEnv)
	assign("PsetData.Available",                   FALSE,                          affylmGUIenvironment)
	#assign("Pset",                                Pset,                           affylmGUIenvironment) ###not sure whether & how to put this into affylmGUIenvironment
} #end of initGlobals <- function()
#
#
#
# I wrote the function deleteItemFromList before I discovered
# that you could simply assign an item to NULL in a list to
# delete it (or use negative-indexing).	Because I am only
# dealing with very small lists, it does not matter that
# I am using an inefficient method, and it may actually make
# the code more readable that assigning an element to NULL.
deleteItemFromList <- function(list1,itemName=NULL,index=NULL){
	if(is.null(index)){
		index <- match(itemName,attributes(list1)$names)
	} #end of if(is.null(index))
	if(is.na(index)){
		return(list1)
	}
	len <- length(list1)
	newlist <- list()
	count <- 0
	for (i in (1:len)){
		if(i!=index){
			count <- count + 1
			if(!is.null(attributes(list1)$names[i])){
				newlist <- c(newlist,list(foo=list1[[i]]))
				attributes(newlist)$names[count] <- attributes(list1)$names[i]
			}else{
				newlist[[count]] <- list1[[i]]
			} #end of if(!is.null(attributes(list1)$names[i]))
		} #end of if(i!=index)
	} #end of for (i in (1:len))
	return (newlist)
} #end of deleteItemFromList <- function(list1,itemName=NULL,index=NULL)
#
#
#
OpenCDFFile <- function(){
	Try(cdfName <- ChooseCDF())
	Try(if(cdfName=="") return())
	Try(assign("CDFFile",cdf,affylmGUIenvironment))#KS. cdf is a data.frame
	Try(CDFFile <- get("CDFFile",envir=affylmGUIenvironment))
	Try(tclvalue(.affylmGUIglobals$CDFfileBoxTitle) <- "Chip Definition (CDF) File")
	Try(tclvalue(.affylmGUIglobals$CDFfileName) <-paste(CDFFile))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))###inserted by keith
	Try(assign("cdfName",cdfName,affylmGUIenvironment))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(ArraysLoaded <- FALSE)
	Try(assign("ArraysLoaded",ArraysLoaded,affylmGUIenvironment))
	#
	tkfocus(.affylmGUIglobals$ttMain)
} #end of OpenCDFFile <- function()
#
#
#
OpenTargetsFile <- function(){
	Try(TargetsFile <- tclvalue(tkgetOpenFile(filetypes="{{Targets Files} {.txt}} {{All files} *}")))
	Try(if(!nchar(TargetsFile)) return())
	Try(assign("TargetsFile",TargetsFile,affylmGUIenvironment))
	Try(tclvalue(.affylmGUIglobals$TargetsfileBoxTitle) <- paste("Targets File"))
	Try(tclvalue(.affylmGUIglobals$TargetsfileName) <- fixSeps(paste(TargetsFile)))
	Try(Targets <- read.table(TargetsFile,header=TRUE,sep="\t",quote="\"",as.is=TRUE))
	Try(
		if(!("FileName" %in% colnames(Targets))){
			Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"FileName\" column.",icon="error"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		} #end of if(!("FileName" %in% colnames(Targets)))
	)
	Try(
		if(!("Target" %in% colnames(Targets))){
			Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"Target\" column.",icon="error"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		} #end of if(!("Target" %in% colnames(Targets)))
	)
	Try(
		if(!("Name" %in% colnames(Targets))){
			Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"Name\" column.",icon="error"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		} #end of if(!("Name" %in% colnames(Targets)))
	)
	#
	Try(assign("Targets",Targets,affylmGUIenvironment))
	Try(assign("NumSlides",nrow(Targets),affylmGUIenvironment))
	#
	Try(ArraysLoaded <- FALSE)
	Try(assign("ArraysLoaded",ArraysLoaded,affylmGUIenvironment))
	#
	Try(tkfocus(.affylmGUIglobals$ttMain))
} #end of OpenTargetsFile <- function()
#
#
#
tclArrayVar <- function(){
	Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
	Try(name <- paste("::RTcl", n,sep = ""))
	Try(l <- list(env = new.env()))
	Try(assign(name, NULL, envir = l$env))
	Try(reg.finalizer(l$env, function(env) tcl("unset", ls(env))))
	Try(class(l) <- "tclArrayVar")
	Try(.Tcl(paste("set ",name,"(0,0) \"\"",sep="")))
	l  ### Investigate this line KS
} #end of tclArrayVar <- function()
#
#
#
GetContrasts <- function(NumContrasts=0){
	Try(NumSlides     <- get("NumSlides",     envir=affylmGUIenvironment))
	Try(Targets       <- get("Targets",       envir=affylmGUIenvironment))
	Try(ArraysLoaded  <- get("ArraysLoaded",  envir=affylmGUIenvironment))
	Try(LinearModelFit.Available <- get("LinearModelFit.Available", envir=affylmGUIenvironment))
	#
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	#
	if(ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Contrasts Matrix",message="No arrays have been loaded.	Please try New or Open from the File menu.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
	} #end of if(ArraysLoaded==FALSE)
	#
	if(LinearModelFit.Available==FALSE){
		Try(ComputeLinearModelFit())
	#		Try(tkmessageBox(title="Compute Contrasts",message="There is no linear model fit available.	Select \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
	#		Try(tkfocus(.affylmGUIglobals$ttMain))
	#		return()
	} #end of if(LinearModelFit.Available==FALSE)
	#
	Try(design             <- get("design", envir=affylmGUIenvironment))
	Try(NumParameters      <- ncol(design))
	Try(ParameterNamesVec  <- colnames(design))
	#
	GetContrastsTable <- function(contrastsFromDropDowns){
		Try(TclRequire("Tktable"))
		Try(ttContrastsTable <- tktoplevel(.affylmGUIglobals$ttMain))
		Try(tkwm.deiconify(ttContrastsTable))
		Try(tkgrab.set(ttContrastsTable))
		Try(tkfocus(ttContrastsTable))
		Try(tkwm.title(ttContrastsTable,"Contrasts Matrix"))
		Try(ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))
		Try(contrastsMatrix <- contrastsFromDropDowns$contrasts)
		Try(tclArrayVar1 <- tclArrayVar())
		Try(tclArrayName <- ls(tclArrayVar1$env))
		#
		onOK <- function(){
			Try(tcl("event","generate",.Tk.ID(table1),"<Leave>"))
			NumRows <- NumParameters
			NumCols <- NumContrasts
			Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
			Try(rownamescontrastsMatrix <- c())
			for (i in (1:NumRows))
					Try(rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
			Try(colnamescontrastsMatrix <- c())
			if(NumCols>0)
				for (j in (1:NumCols))
					Try(colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
			Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
			Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
			if(NumCols>0)
				for (i in (1:NumRows))
					for (j in (1:NumCols))
							Try(contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))
			Try(tkgrab.release(ttContrastsTable))
			Try(tkdestroy(ttContrastsTable))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			Try(ReturnVal <<- list(contrasts=contrastsMatrix,contrastsCreatedFromDropDowns=FALSE))
		} #end of onOK <- function()
		onCancel <- function(){
			Try(tkgrab.release(ttContrastsTable))
			Try(tkdestroy(ttContrastsTable))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE)
		} #ed of onCancel <- function()
		Try(OK.but <-tkbutton(ttContrastsTable,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
		Try(Cancel.but <-tkbutton(ttContrastsTable,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
		Try(tkgrid(tklabel(ttContrastsTable,text="    ")))
		Try(PleaseEntercontrastsMatrixLabel<-tklabel(ttContrastsTable,text="Please enter the contrasts matrix to be used for linear-modelling.",font=.affylmGUIglobals$affylmGUIfont2))
		Try(tkgrid(tklabel(ttContrastsTable,text="    "),PleaseEntercontrastsMatrixLabel))
		Try(tkgrid.configure(PleaseEntercontrastsMatrixLabel,columnspan=2))
		Try(tkgrid(tklabel(ttContrastsTable,text="    ")))
		NumRows <- NumParameters
		NumCols <- NumContrasts
		#
		Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
		###
		Try(
			if(nrow(contrastsMatrix)==0){
				Try(ContrastsNamesVec <- c())
				if(NumContrasts>0){
					for (i in (1:NumContrasts)){
						Try(ContrastsNamesVec <- c(ContrastsNamesVec,paste("Contrast ",i,sep="")))
					} #end of for (i in (1:NumContrasts))
				} #end of if(NumContrasts>0)
			}else{
				Try(ContrastsNamesVec <- colnames(contrastsMatrix))
			} #end of else/if if(nrow(contrastsMatrix)==0)
		) #end of Try
		###
		Try(ColNamesVec <- ContrastsNamesVec)
		#
		Try(rownamescontrastsMatrix <- c())
		Try(myRarray <- "    ")
		for (i in (1:NumRows)){
				Try(RowName <- ParameterNamesVec[i])
				Try(rownamescontrastsMatrix <- c(rownamescontrastsMatrix,RowName))
				Try(myRarray <- c(myRarray,paste(RowName)))
		} #end of for (i in (1:NumRows))
		if(NumCols>0){
			for (j in (1:NumCols)){
				Try(myRarray <- c(myRarray,paste(ColNamesVec[j])))
				for (i in (1:NumRows))
				{
						if(nrow(contrastsMatrix)==0)
								Try(myRarray <- c(myRarray,"0"))
						else
								Try(myRarray <- c(myRarray,paste(contrastsMatrix[i,j])))
				}
			} #end of for (j in (1:NumCols))
		} #end of if(NumCols>0)
		# This will give an error if tclArray doesn't exist.
		# .Tcl("unset tclArray")
		Try(dim(myRarray) <- c(NumRows+1,NumCols+1))
		if(NumCols>0){
			for (i in (0:NumRows)){
				for (j in (0:NumCols)){
					 # Modified to use tcl!
					 Try(tcl("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))
				} #end of for (j in (0:NumCols))
			} #end of for (i in (0:NumRows))
		} #end of if(NumCols>0)
		# Below, can I just use tkwidget(ttContrastsTable,"table",...) ?	Yes, of course.
		Try(table1 <- .Tk.subwin(ttContrastsTable))
		Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols+1),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0"))))
		Try(tkgrid(tklabel(ttContrastsTable,text="    "),table1))
		#
		Try(tcl(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownamescontrastsMatrix))+2))))
		Try(
			if(nrow(contrastsMatrix)>0){
				Try(
					for (j in (1:NumCols)){
						Try(tcl(.Tk.ID(table1),"width",paste(j),paste(max(4,max(nchar(ColNamesVec))+2,max(nchar(contrastsMatrix[,j]))+2))))
					}
				) #end of Try
			} #end of if(nrow(contrastsMatrix)>0)
		) #end of Try
		#
		#				 Try(tkcmd(.Tk.ID(table1),"width","0","25"))
		#
		Try(tkconfigure(table1,font=.affylmGUIglobals$affylmGUIfontTable))
		Try(tkgrid.configure(table1,columnspan=2))
		#
		Try(copyFcn <-			function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))
		#
		openContrastsMatrixFile <- function(){
			Try(contrastsMatrixFileName <- tclvalue(tkgetOpenFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}")))
			Try(if(!nchar(contrastsMatrixFileName)) return())
			Try(contrastsMatrixTable <- read.table(contrastsMatrixFileName,header=FALSE,sep="\t",quote="\"",as.is=TRUE))
			# This will give an error if tclArray doesn't exist.
			# .Tcl("unset tclArray")
			if(NumCols>0)
				for (i in (0:NumRows))
					for (j in (0:NumCols))
						Try(tcl("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(contrastsMatrixTable[i+1,j+1])))
		} #end of openContrastsMatrixFile <- function()
		#
		saveContrastsMatrixFile <- function(){
			Try(contrastsMatrixFileName <- tclvalue(tkgetSaveFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}")))
			Try(if(!nchar(contrastsMatrixFileName)) return())
			Try(len <- nchar(contrastsMatrixFileName))
			if(len<=4){
				Try(	contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
			}else if(substring(contrastsMatrixFileName,len-3,len)!=".txt"){
						Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
			}
			Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumSlides,ncol=NumParameters)))
			Try(rownamescontrastsMatrix <- c())
			Try(
				for (i in (1:NumRows)){
					rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep=""))
				}
			)
			Try(colnamescontrastsMatrix <- c())
			if(NumParameters>0){
				Try(
					for (j in (1:NumCols)){
						colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep=""))
					}
				)
			} #end of if(NumParameters>0)
			Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
			Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
			if(NumParameters>0){
				Try(for (i in (1:NumRows))
					for (j in (1:NumParameters)){
						contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep="")))
					}
				)
			} #end of if(NumParameters>0)
			Try(write.table(contrastsMatrix,file=contrastsMatrixFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
		} #end of saveContrastsMatrixFile <- function()
		#
		Try(topMenu <- tkmenu(ttContrastsTable, tearoff=FALSE))
		Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
		Try(tkadd(fileMenu, "command", label="Open",			command=openContrastsMatrixFile)) # ) # ,font=affylmGUIfontMenu))
		Try(tkadd(fileMenu, "command", label="Save As",			command=saveContrastsMatrixFile)) # ) # ,font=affylmGUIfontMenu))
		Try(tkadd(topMenu,	"cascade", label="File",menu=fileMenu)) # ) # ,font=affylmGUIfontMenu))
		#
		Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
		Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",			command=copyFcn)) # ) # ,font=affylmGUIfontMenu))
		Try(tkadd(topMenu,	"cascade", label="Edit",menu=editMenu)) # ) # ,font=affylmGUIfontMenu))
		#
		Try(tkconfigure(ttContrastsTable,menu=topMenu))
		#
		Try(BlankLabel1<-tklabel(ttContrastsTable,text="    "))
		Try(tkgrid(BlankLabel1))
		Try(BlankLabel2<-tklabel(ttContrastsTable,text="    "))
		Try(tkgrid(BlankLabel2,OK.but,Cancel.but))
		Try(tkgrid.configure(OK.but,sticky="e"))
		Try(tkgrid.configure(Cancel.but,sticky="w"))
		Try(BlankLabel3<-tklabel(ttContrastsTable,text="    "))
		Try(tkgrid(BlankLabel3))
		#
		Try(tkfocus(ttContrastsTable))
		Try(tkbind(ttContrastsTable, "<Destroy>", function() {Try(tkgrab.release(ttContrastsTable));Try(tkfocus(.affylmGUIglobals$ttMain));}))
		Try(tkwait.window(ttContrastsTable))
		return (ReturnVal)
	} #end of GetContrastsTable <- function(contrastsFromDropDowns)
	#
	if(NumParameters<=0){
		Try(tkmessageBox(title="At Least Two RNA Types Are Required",message="You must have at least two types of RNA in your Targets file.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return(list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))
	} #end of if(NumParameters<=0)
	#
	Try(NumRows <- NumParameters)
	Try(NumCols <- NumContrasts)
	#
	Try(ttContrasts<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttContrasts))
	Try(tkgrab.set(ttContrasts))
	Try(tkfocus(ttContrasts))
	Try(tkwm.title(ttContrasts,"Contrasts"))
	#
	Try(lbl2<-tklabel(ttContrasts,text="Please specify pairs of parameters for which contrasts will be estimated",font=.affylmGUIglobals$affylmGUIfont2))
	Try(lbl3<-tklabel(ttContrasts,text="                                                                    "))
	Try(tkgrid(tklabel(ttContrasts,text="      "),row=0,column=1,columnspan=1))
	Try(tkgrid(tklabel(ttContrasts,text="      "),row=0,column=4,columnspan=1))
	Try(tkgrid(lbl2,row=1,column=2,columnspan=4,rowspan=1,sticky="ew"))
	Try(tkgrid.configure(lbl2,sticky="w"))
	Try(tkgrid(tklabel(ttContrasts,text="         "),column=1))
	Try(tkgrid(tklabel(ttContrasts,text="         ")))
	Try(tkgrid(tklabel(ttContrasts,text="         "),column=1))
	#	plus<-tklabel(ttContrasts,text="   +   ",font=.affylmGUIglobals$affylmGUIfont2)
	#	minus<-tklabel(ttContrasts,text="   -   ",font=.affylmGUIglobals$affylmGUIfont2)
	#	tkgrid(plus,row=3, column=2,sticky="ew")
	#	tkgrid(minus,row=3,column=6,sticky="ew")
	#
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
	#
	Try(TclList1AsString <- "{")
	Try(for (i in (1:NumParameters))
		TclList1AsString <- paste(TclList1AsString,"{",ParameterNamesVec[i],"} ",sep=""))
	TclList1AsString <- paste(TclList1AsString,"}",sep="")
	TclList2AsString <- TclList1AsString
	#
	#	Try(plusOrMinusTclListAsString <- "{{minus} {plus}}")
	Try(plusOrMinusTclListAsString <- "{{minus}}")
	#
	Try(TclRequire("BWidget"))
	#
	Try(combo1 <- c())
	Try(combo2 <- c())
	Try(combo3 <- c())
	Try(
		if(NumCols>0){
			for (contrastIndex in (1:NumCols)){
				Try(FirstDropDownColumn <- .Tk.subwin(ttContrasts))
				combo1 <- c(combo1,FirstDropDownColumn)
				Try(.Tcl(paste("ComboBox",.Tk.ID(FirstDropDownColumn),"-editable false -values",TclList1AsString)))
				Try(SecondDropDownColumn <- .Tk.subwin(ttContrasts))
				Try(combo2 <- c(combo2,SecondDropDownColumn))
				Try(.Tcl(paste("ComboBox",.Tk.ID(SecondDropDownColumn),"-editable false -values",TclList2AsString)))
				Try(plusOrMinusDropDown <- .Tk.subwin(ttContrasts))
				Try(combo3 <- c(combo3,plusOrMinusDropDown))
				Try(.Tcl(paste("ComboBox",.Tk.ID(plusOrMinusDropDown),"-editable false -values",plusOrMinusTclListAsString)))
				Try(tcl(.Tk.ID(plusOrMinusDropDown),"setvalue","first"))
				Try(
					if(.affylmGUIglobals$affylmGUIpresentation==TRUE){
						Try(tkconfigure(FirstDropDownColumn,width=10))
						Try(tkconfigure(SecondDropDownColumn,width=10))
						Try(tkconfigure(plusOrMinusDropDown,width=10))
					} #end of if(.affylmGUIglobals$affylmGUIpresentation==TRUE)
				) #end of Try
				#
				Try(dropdownLabel <- paste("Contrast",contrastIndex, "  ")	)
				#
				Try(
					tkgrid(
						tklabel(
							ttContrasts,
							text=dropdownLabel,
							font=.affylmGUIglobals$affylmGUIfont2
						),
						row=2+contrastIndex,
						column=0,
						sticky="w"
					) #end of tkgrid
				) #end of Try
				Try(tkconfigure(FirstDropDownColumn,font=.affylmGUIglobals$affylmGUIfont2))
				Try(tkconfigure(SecondDropDownColumn,font=.affylmGUIglobals$affylmGUIfont2))
				Try(tkconfigure(plusOrMinusDropDown,font=.affylmGUIglobals$affylmGUIfont2))
				Try(tkgrid(FirstDropDownColumn,row=2+contrastIndex,column=2,columnspan=1,rowspan=1))
				Try(tkgrid(plusOrMinusDropDown,row=2+contrastIndex,column=4,columnspan=1,rowspan=1))
				Try(tkgrid(SecondDropDownColumn,row=2+contrastIndex,column=6,columnspan=1,rowspan=1))
				#
				Try(tkgrid(tklabel(ttContrasts,text="    "),row=2+contrastIndex,column=7))
			} #end of for (contrastIndex in (1:NumCols))
		} #end of if(NumCols>0)
	) #end of Try
	Try(tkgrid(tklabel(ttContrasts,text="                                                "),rowspan=1,columnspan=4))
	#
	Try(ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c()))
	#
	OnAdvanced <- function(){
		Try(contrastsFromDropDowns <- GetContrastsFromDropDowns())
		Try(ReturnValcontrastsMatrixTable <- GetContrastsTable(contrastsFromDropDowns)) # Returns contrastsMatrix list object including contrastsMatrix matrix as data.frame
		NumRows <- nrow(ReturnValcontrastsMatrixTable$contrasts)
		if(NumRows>0 ){ # OK was clicked, not Cancel
			Try(tkgrab.release(ttContrasts))
			Try(tkdestroy(ttContrasts))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			ReturnVal <<- ReturnValcontrastsMatrixTable	 # List contains contrastsMatrix matrix as data.frame
		} #end of if(NumRows>0 )
	} #end of OnAdvanced <- function()
	#
	GetContrastsFromDropDowns <- function(){
		NumRows <- NumParameters
		NumCols <- NumContrasts
		Param1 <-c()
		Param2 <-c()
		NoParameter <- 99999
		if(NumCols>0){
			for (contrastIndex in (1:NumCols)){
				# I think I wrote this code when I was an R Tcl/Tk beginner.	Check and update!
				# I think combo1 and combo2 should really be lists, not vectors!!!
				# *2 below, because the c() combines the tkwin objects which are acutally
				# lists with 2 components: window ID and environment.
				selection1 <- tclvalue(.Tcl(paste(.Tk.ID(combo1[contrastIndex*2-1]),"getvalue")))
				selection2 <- tclvalue(.Tcl(paste(.Tk.ID(combo2[contrastIndex*2-1]),"getvalue")))
				selection3 <- tclvalue(.Tcl(paste(.Tk.ID(combo3[contrastIndex*2-1]),"getvalue")))
				Try(if((selection1!="-1"))
					Try(Param1 <- c(Param1,as.numeric(selection1)+1))
				else
					Try(Param1 <- c(Param1,NoParameter)))
				Try(if((selection2!="-1"))
					Try(Param2 <- c(Param2,as.numeric(selection2)+1))
				else
					Try(Param2 <- c(Param2,NoParameter)))
			} #end of for (contrastIndex in (1:NumCols))
		} #end of if(NumCols>0)
		#
		contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols))
		Try(rownames(contrastsMatrix) <- ParameterNamesVec)
		#
		Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
		ContrastNamesVec <- vector(length=NumContrasts)
		if(NumContrasts>0)
			for (j in (1:NumContrasts))
				ContrastNamesVec[j] <- SimplifyContrastsExpression(paste("(",ParameterNamesVec[Param1[j]],")-(",ParameterNamesVec[Param2[j]],")",sep=""))
		colnames(contrastsMatrix) <- ContrastNamesVec
		#
		Try(
			for (i in (1:NumParameters)){
				for (j in (1:NumContrasts)){
					Try(contrastsMatrix[i,j] <- 0)
					Try(
						if(Param1[j]==i){
							contrastsMatrix[i,j] <- 1
						}
					)
					Try(
						if(Param2[j]==i){
							contrastsMatrix[i,j] <- -1
						}
					)
				} #end of for (j in (1:NumContrasts))
			} #end of for (i in (1:NumParameters))
		) #end of Try
		#
		Try(
			if(max(abs(contrastsMatrix))==0){
				Try(return(list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c())))
			}
		)
		#
		#		# Go through from the right hand column to the left and check for all zeros (i.e. check if max(abs(...)) == 0). If so try to reduce the
		#		# number of columns (contrasts).
		#		Try(while(max(abs(contrastsMatrix[,NumContrasts]))==0 && NumContrasts > 1)
		#			NumContrasts <<- NumContrasts - 1)
		#		Try(contrastsMatrix <- contrastsMatrix[,1:NumContrasts,drop=FALSE])
		#		Try(Param1 <- Param1[1:NumContrasts])
		#		Try(Param2 <- Param2[1:NumContrasts])
		#
		return(list(contrasts=contrastsMatrix,contrastsCreatedFromDropDowns=TRUE,Param1=Param1,Param2=Param2))
	} #end of GetContrastsFromDropDowns <- function()
	#
	onOK <- function(){
		Try(contrastsMatrixList <- GetContrastsFromDropDowns())
		Try(
			if(nrow(contrastsMatrixList$contrasts)==0){
				Try(
					tkmessageBox(
						title="Contrasts",
						message=paste(
							"Error in creating contrasts matrix from drop-down selection. ",
							"Make sure you have selected a parameter pair for each contrast."
						),
						type="ok",
						icon="error"
					) #end of tkmessageBox
				) #end of Try
				Try(ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c()))
				return()
			}else{
				Try(tkgrab.release(ttContrasts))
				Try(tkdestroy(ttContrasts))
				Try(tkfocus(.affylmGUIglobals$ttMain))
				Try(ReturnVal <<- contrastsMatrixList)
				Try(tkfocus(.affylmGUIglobals$ttMain))
				return()
			} #end of else/if(nrow(contrastsMatrixList$contrasts)==0)
		) #end of Try
	} #end of onOK <- function()
	onCancel <- function(){
		Try(tkgrab.release(ttContrasts))
		Try(tkdestroy(ttContrasts))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c())
	} #end of onCancel <- function()
	Advanced.but <- tkbutton(ttContrasts,text="Advanced...",command=OnAdvanced,font=.affylmGUIglobals$affylmGUIfont2)
	Try(OK.but <-tkbutton(ttContrasts,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(ttContrasts,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(OK.but,column=2,row=9+NumParameters))
	Try(tkgrid(Cancel.but,column=4,row=9+NumParameters))
	Try(tkgrid(Advanced.but,column=6,row=9+NumParameters))
	Try(tkgrid(tklabel(ttContrasts,text="    ")))
	#
	Try(tkfocus(ttContrasts))
	#
	Try(tkbind(ttContrasts, "<Destroy>", function() {Try(tkgrab.release(ttContrasts));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttContrasts))
	return (ReturnVal)
}#end of GetContrasts
#
#
#
# Actually in the two functions below, RNA should really be Param, but it doesn't really make a difference.
# This came from the cDNA version.
#
SimplifyContrastsExpression <- function(string){
	RNATypesAndSign <- GetRNATypesFrom.ContrastsFromDropDowns.String(string)
	RNA1 <- RNATypesAndSign$RNA1
	RNA2 <- RNATypesAndSign$RNA2
	RNA3 <- RNATypesAndSign$RNA3
	RNA4 <- RNATypesAndSign$RNA4
	plusOrMinusSign <- RNATypesAndSign$plusOrMinusSign
	ReturnVal <- string
	#
	if(RNA1==RNA3&&plusOrMinusSign=='-')
		ReturnVal <- paste("(",RNA4,")-(",RNA2,")",sep="")
	if(RNA2==RNA4&&plusOrMinusSign=='-')
		ReturnVal <- paste("(",RNA1,")-(",RNA3,")",sep="")
	if(RNA1==RNA4&&plusOrMinusSign=='+')
		ReturnVal <- paste("(",RNA3,")-(",RNA2,")",sep="")
	if(RNA2==RNA3&&plusOrMinusSign=='+')
		ReturnVal <- paste("(",RNA1,")-(",RNA4,")",sep="")
	return(ReturnVal)
} #end of SimplifyContrastsExpression <- function(string)
#
#
#
GetRNATypesFrom.ContrastsFromDropDowns.String <- function(string){
	len <- nchar(string)
	string <- substr(string,3,len)
	# string == "a)-(b))-((b)-(c"
	len <- nchar(string)
	i <- 1
	while (substr(string,i,i)!=")" && (i<=len))
		i <- i + 1
	RNA1 <- substr(string,1,i-1)
	len <- nchar(string)
	string <- substr(string,i+3,len)
	len <- nchar(string)
	i<-1
	while (substr(string,i,i)!=")" && (i<=len))
		i <- i + 1
	RNA2 <- substr(string,1,i-1)
	len <- nchar(string)
	plusOrMinusSign <- substr(string,i+2,i+2)
	string <- substr(string,i+5,len)
	len <- nchar(string)
	i<-1
	while (substr(string,i,i)!=")" && (i<=len))
		i <- i + 1
	RNA3 <- substr(string,1,i-1)
	len <- nchar(string)
	string <- substr(string,i+3,len)
	len <- nchar(string)
	i<-1
	while (substr(string,i,i)!=")" && (i<=len))
		i <- i + 1
	RNA4 <- substr(string,1,i-1)
	list(RNA1=RNA1,RNA2=RNA2,RNA3=RNA3,RNA4=RNA4,plusOrMinusSign=plusOrMinusSign)
} #end of GetRNATypesFrom.ContrastsFromDropDowns.String <- function(string)
#
#
#
ViewContrastsMatrixInTable <- function(contrastsMatrixList,contrastParameterizationIndex=NULL){
	Try(design <- get("design",envir=affylmGUIenvironment))
	Try(ParameterNamesVec <- colnames(design))
	Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	#
	Try(TclRequire("Tktable"))
	Try(ttViewContrastsMatrixTable <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttViewContrastsMatrixTable))
	Try(tkgrab.set(ttViewContrastsMatrixTable))
	Try(tkfocus(ttViewContrastsMatrixTable))
	Try(tkwm.title(ttViewContrastsMatrixTable,paste("Contrasts matrix for contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex])))
	Try(contrastsMatrix <- contrastsMatrixList$contrasts)
	Try(NumContrasts <- ncol(contrastsMatrix))
	#
	onClose <- function() {
		Try(.Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<Leave>"))));
		Try(tkgrab.release(ttViewContrastsMatrixTable));
		Try(tkdestroy(ttViewContrastsMatrixTable));
		Try(tkfocus(.affylmGUIglobals$ttMain))
	} #end of onClose <- function()
	#
	Try(NumRows <- NumParameters)
	Try(NumCols <- NumContrasts)
	#
	Try(
		if(is.null(colnames(contrastsMatrix))){
			Try(ColumnNamesVec <- c())
			if(NumCols>0){
				for (i in (1:NumCols)){
					Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i)))
				} #end of for
			} #end of if(NumCols>0)
		}else{
			Try(ColumnNamesVec <- colnames(contrastsMatrix))
		} #end of else/if(is.null(colnames(contrastsMatrix)))
	) #end of Try
	#
	Try(RowNamesVec <- c())
	Try(myRarray <- "    ")
	Try(
		for (i in (1:NumRows)){
			Try(
				if(is.null(rownames(contrastsMatrix))){
					Try(RowName <- paste("Param",i))
				}else{
					Try(RowName <- rownames(contrastsMatrix)[i])
				} #end of else/if(is.null(rownames(contrastsMatrix)))
			)# end of Try
			Try(RowNamesVec <- c(RowNamesVec,RowName))
			Try(myRarray <- c(myRarray,paste(RowName)))
		} #end of for (i in (1:NumRows))
	) #end of Try
	#
	if(NumCols>0){
		for (j in (1:NumCols)){
			Try(myRarray <- c(myRarray,paste(ColumnNamesVec[j])))
			for (i in (1:NumRows))
			{
					if(nrow(contrastsMatrix)==0)
							Try(myRarray <- c(myRarray,"0"))
					else
							Try(myRarray <- c(myRarray,paste(contrastsMatrix[i,j])))
			}
		} #end of for (j in (1:NumCols))
	} #end of if(NumCols>0)
	Try(tclArrayVar1 <- tclArrayVar())
	Try(tclArrayName <- ls(tclArrayVar1$env))
	#
	Try(dim(myRarray) <- c(NumRows+1,NumCols+1))
	#
	# This will give an error if tclArray doesn't exist.
	# .Tcl("unset tclArray")
	if(NumCols>0){
		for (i in (0:NumRows)){
			for(j in (0:NumCols)){
				Try(tcl("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))
			} #end of for(j in (0:NumCols))
		} #end of for (i in (0:NumRows))
	} #end of if(NumCols>0)
	Try(table1 <- tkwidget(ttViewContrastsMatrixTable,"table"))
	Try(
		tkconfigure(
			table1,
			variable=tclArrayName,
			rows=paste(NumRows+1),
			cols=paste(NumCols+1),
			titlerows="0",
			titlecols="0",
			selectmode="extended",
			colwidth="13",
			background="white",
			rowseparator="\"\n\"",
			colseparator="\"\t\"",
			resizeborders="col",
			multiline="0",
			xscrollcommand=function(...) tkset(xscr,...),
			yscrollcommand=function(...) tkset(yscr,...),
			state="disabled"
		) #end of tkconfigure
	) #end of Try
	Try(xscr <- tkscrollbar(ttViewContrastsMatrixTable,orient="horizontal", command=function(...)tkxview(table1,...)))
	Try(yscr <- tkscrollbar(ttViewContrastsMatrixTable,command=function(...)tkyview(table1,...)))
	Try(tkgrid(table1,yscr))
	Try(tkgrid.configure(yscr,sticky="nsw"))
	Try(tkconfigure(table1,font=.affylmGUIglobals$affylmGUIfontTable))
	Try(tkgrid(xscr,sticky="new"))
	Try(copyFcn <-			function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))
	#
	Try(tcl(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownames(contrastsMatrix)))+2))))
	Try(
		for (j in (1:NumCols)){
			Try(tcl(.Tk.ID(table1),"width",paste(j),paste(max(4,nchar(colnames(contrastsMatrix)[j])+2,max(nchar(contrastsMatrix[,j]))+2))))
		}
	) #end of TRy
	#
	onSaveContrastsMatrixAs <- function(){
		Try(contrastsMatrixFileName <- tclvalue(tkgetSaveFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}")))
		Try(if(!nchar(contrastsMatrixFileName)) return())
		Try(len <- nchar(contrastsMatrixFileName))
		if(len<=4){
			Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
		}else if(substring(contrastsMatrixFileName,len-3,len)!=".txt"){
			Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
		}
		Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
		Try(rownamescontrastsMatrix <- c())
		Try(
			for (i in (1:NumRows)){
				rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep=""))
			}
		)
		Try(colnamescontrastsMatrix <- c())
		if(NumParameters>0){
			Try(
				for (j in (1:NumCols)){
					colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep=""))
				}
			)
		} #end of if(NumParameters>0)
		Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
		Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
		if(NumParameters>0){
			Try(
				for (i in (1:NumRows)){
					for (j in (1:NumParameters)){
						contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep="")))
					} #end of for (j in (1:NumParameters))
				} #end of for (i in (1:NumRows))
			)
		} #end of if(NumParameters>0)
		#
		Try(write.table(contrastsMatrix,file=contrastsMatrixFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
	} #end of onSaveContrastsMatrixAs <- function()
	#
	Try(topMenu <- tkmenu(ttViewContrastsMatrixTable, tearoff=FALSE))
	#
	Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(tkadd(fileMenu, "command", label="Save As",		command=onSaveContrastsMatrixAs))
	Try(tkadd(fileMenu, "command", label="Close",			command=onClose))
	Try(tkadd(topMenu,	"cascade", label="File",menu=fileMenu))
	Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",			command=copyFcn))
	Try(tkadd(topMenu,	"cascade", label="Edit",menu=editMenu))
	#
	Try(tkconfigure(ttViewContrastsMatrixTable,menu=topMenu))
	#
	Try(tkfocus(ttViewContrastsMatrixTable))
	Try(tkbind(ttViewContrastsMatrixTable, "<Destroy>", function () {Try(tkgrab.release(ttViewContrastsMatrixTable));Try(tkfocus(.affylmGUIglobals$ttMain))}))
	Try(tkwait.window(ttViewContrastsMatrixTable))
}#end of ViewContrastsMatrixInTable <- function(contrastsMatrixList,contrastParameterizationIndex=NULL)
#
#
#
ViewContrastsMatrixAsPairs <- function(contrastsMatrix,contrastsMatrixList,contrastParameterizationIndex=NULL){
	Try(SlideNamesVec <- get("SlideNamesVec",envir=affylmGUIenvironment))
	Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	#
	Try(contrastsMatrix <- contrastsMatrixList$contrasts)
	Try(NumContrasts <- ncol(contrastsMatrix))
	NumRows <- NumParameters
	NumCols <- NumContrasts
	#
	ttViewContrastsMatrixAsPairs<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttViewContrastsMatrixAsPairs)
	tkgrab.set(ttViewContrastsMatrixAsPairs)
	tkfocus(ttViewContrastsMatrixAsPairs)
	Try(tkwm.title(ttViewContrastsMatrixAsPairs,paste("Contrasts in contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex],".",sep="")))
	Try(TitleLabel<-tklabel(ttViewContrastsMatrixAsPairs,text=paste("Contrasts in contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex],sep=""),font=.affylmGUIglobals$affylmGUIfont2b))
	#
	Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))
	Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),TitleLabel))
	Try(tkgrid.configure(TitleLabel,columnspan=4))
	Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))
	Try(ParameterOrContrastLabel <- tklabel(ttViewContrastsMatrixAsPairs,text="Contrast",font=.affylmGUIglobals$affylmGUIfont2b))
	## Note that plusOrMinus IS A VECTOR (can be different for each contrast).
	## Try(plusOrMinus <- contrastsMatrixList$plusOrMinus)
	#
	Try(plusOrMinus <- rep("-",NumContrasts))
	#
	Try(
		tkgrid(
			tklabel(
				ttViewContrastsMatrixAsPairs,text="    "
			),
			ParameterOrContrastLabel
		) #end of tkgrid
	) #end of Try
	#
	if(is.null(colnames(contrastsMatrix))){
		Try(ColumnNamesVec <- c())
		if(NumCols>0){
			for (i in (1:NumCols)){
				Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i)))
			}
		} #end of if(NumCols>0)
	}else{
		Try(ColumnNamesVec <- colnames(contrastsMatrix))
	} #end of else/if(is.null(colnames(contrastsMatrix)))
	#
	Try(RowNamesVec <- c())
	Try(
		for (i in (1:NumRows)){
			Try(
				if(is.null(rownames(contrastsMatrix))){
					Try(RowName <- paste("Param",i))
				}else{
					Try(RowName <- rownames(contrastsMatrix)[i])
				}
			) #end of Try
			Try(RowNamesVec <- c(RowNamesVec,RowName))
		} #end of for (i in (1:NumRows))
	) #end of Try
	#
	if(NumCols>0){
		for (i in (1:NumCols)){
			Try(FirstItemOfPair	<- paste(RowNamesVec[contrastsMatrixList$Param1[i]]))
			Try(SecondItemOfPair <- paste(RowNamesVec[contrastsMatrixList$Param2[i]]))
			Try(if(plusOrMinus[i]=="+") plusOrMinusText <- "plus" else plusOrMinusText <- "minus")
			Try(
				tkgrid(
					tklabel(ttViewContrastsMatrixAsPairs,text="    "),
					tklabel(ttViewContrastsMatrixAsPairs,text=ColumnNamesVec[i],background="white",font=.affylmGUIglobals$affylmGUIfont2),
					tklabel(ttViewContrastsMatrixAsPairs,text="    "),
					tklabel(ttViewContrastsMatrixAsPairs,text=FirstItemOfPair,background="white",font=.affylmGUIglobals$affylmGUIfont2),
					tklabel(ttViewContrastsMatrixAsPairs,text="    "),
					tklabel(ttViewContrastsMatrixAsPairs,text=plusOrMinusText,font=.affylmGUIglobals$affylmGUIfont2,bg="white"),
					tklabel(ttViewContrastsMatrixAsPairs,text="    "),
					tklabel(ttViewContrastsMatrixAsPairs,text=SecondItemOfPair,background="white",font=.affylmGUIglobals$affylmGUIfont2),
					tklabel(ttViewContrastsMatrixAsPairs,text="    ")
				) #end of tkgrid
			) #end of Try
			Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))
		} #end of for (i in (1:NumCols))
	} #end of if(NumCols>0)
	#
	tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="     "))
	#
	Advanced.but <- tkbutton(
		ttViewContrastsMatrixAsPairs,
		text="Advanced...",
		command=function() {ViewContrastsMatrixInTable(contrastsMatrixList,contrastParameterizationIndex)},
		font=.affylmGUIglobals$affylmGUIfont2
	) #end of Advanced.but <- tkbutton
	onOK <- function() {
		Try(tkgrab.release(ttViewContrastsMatrixAsPairs));
		Try(tkdestroy(ttViewContrastsMatrixAsPairs));
		Try(tkfocus(.affylmGUIglobals$ttMain))
	}
	OK.but <-tkbutton(
		ttViewContrastsMatrixAsPairs,text="   OK   ",
		command=onOK,
		font=.affylmGUIglobals$affylmGUIfont2
	) #end of OK.but <-tkbutton
	tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),OK.but,Advanced.but)
	tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "))
	#
	Try(tkfocus(ttViewContrastsMatrixAsPairs))
	#
	Try(tkbind(ttViewContrastsMatrixAsPairs, "<Destroy>", function() {Try(tkgrab.release(ttViewContrastsMatrixAsPairs));Try(tkfocus(.affylmGUIglobals$ttMain))}))
	Try(tkwait.window(ttViewContrastsMatrixAsPairs))
} #end of ViewContrastsMatrixAsPairs <- function(contrastsMatrix,contrastsMatrixList,contrastParameterizationIndex=NULL)
#
#
#
ViewExistingContrastParameterization <- function(){
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	if(NumContrastParameterizations==0)
	{
		Try(tkmessageBox(title="View Existing Contrasts Parameterization",message="There are no contrast parameterizations loaded.	Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	}
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	Try(if(contrastParameterizationIndex==0) return())
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	#
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	Try(ContrastParameterizationTREEIndexVec <- ContrastParameterizationList[[ContrastParameterizationNameNode]]$ContrastParameterizationTREEIndexVec)
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	Try(ContrastsContrastParameterizationListNode <- paste("ContrastParameterizations.",.affylmGUIglobals$ContrastParameterizationTREEIndex,".",.affylmGUIglobals$ContrastParameterizationTREEIndex, sep=""))
	Try(contrastsList <- ContrastParameterizationList[[ContrastParameterizationNameNode]]$contrastsMatrixInList)
	Try(
		if(contrastsList$contrastsCreatedFromDropDowns==FALSE){
			Try(ViewContrastsMatrixInTable(contrastsMatrix="Contrasts",contrastsList,contrastParameterizationIndex))
		}else{
			Try(ViewContrastsMatrixAsPairs(contrastsMatrix="Contrasts",contrastsList,contrastParameterizationIndex))
		}
	)
} #end of ViewExistingContrastParameterization <- function()
#
#
#
ViewRNATargets <- function(){
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	Try(Targets <- get("Targets",envir=affylmGUIenvironment))
	Try(ArraysLoaded	<- get("ArraysLoaded", envir=affylmGUIenvironment))
	#
	if(ArraysLoaded==FALSE){
		Try(tkmessageBox(title="RNA Targets",message="No arrays have been loaded.	Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	} #end of if(ArraysLoaded==FALSE)
	if(nrow(Targets)==0){
		Try(tkmessageBox(title="RNA Targets",message="No RNA targets have been loaded.	Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
	} #end of if(nrow(Targets)==0)
	#
	Try(TclRequire("Tktable"))
	Try(ttViewRNATargets <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttViewRNATargets))
	##Try(tkgrab.set(ttViewRNATargets))
	Try(tkfocus(ttViewRNATargets))
	Try(tkwm.title(ttViewRNATargets,"RNA Targets"))
	#
	##Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
	##Try(tclArrayName <- paste("::RTcl", n, sep = ""))
	Try(tclArrayVar1 <- tclArrayVar())
	Try(tclArrayName <- ls(tclArrayVar1$env))
	#
	##onClose <- function() {Try(tkgrab.release(ttViewRNATargets));Try(tkdestroy(ttViewRNATargets));Try(tkfocus(.affylmGUIglobals$ttMain))}
	onClose <- function() {Try(tkdestroy(ttViewRNATargets));Try(tkfocus(.affylmGUIglobals$ttMain))}
	#
	Try(NumCols <- ncol(Targets))
	Try(NumRows <- nrow(Targets))
	#
	Try(myRarray <- c())
	#
	if(NumCols>0){
		for (j in (1:NumCols)){
			Try(myRarray <- c(myRarray,paste(colnames(Targets)[j])))
			for (i in (1:NumRows)){
				Try(myRarray <- c(myRarray,paste(Targets[i,j])))
			} #end of for (i in (1:NumRows))
		} #end of for (j in (1:NumCols))
	} #end of if(NumCols>0)
	#
	Try(dim(myRarray) <- c(NumRows+1,NumCols))
	# This will give an error if tclArray doesn't exist.
	# .Tcl("unset tclArray")
	if(NumRows>0 && NumCols>0){
		for (i in (0:NumRows)){
			for (j in (1:NumCols)){
				Try(tcl("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(myRarray[i+1,j])))
			} #end of for (j in (1:NumCols))
		} #end of for (i in (0:NumRows))
	} #end of if(NumRows>0 && NumCols>0)
	# Below, I should just use tkwidget(ttViewRNATargets,"table",...)
	Try(table1 <- .Tk.subwin(ttViewRNATargets))
	Try(
		.Tcl(
			paste(
				"table",
				.Tk.ID(table1),
				.Tcl.args(
					variable=tclArrayName,
					rows=paste(NumRows+1),
					cols=paste(NumCols),
					titlerows="0",
					titlecols="0",
					selectmode="extended",
					colwidth="13",
					background="white",
					rowseparator="\"\n\"",
					colseparator="\"\t\"",
					resizeborders="col",
					multiline="0",
					titlerows=1,
					colstretchmode="unset",
					xscrollcommand=function(...) tkset(xscr,...),
					yscrollcommand=function(...) tkset(yscr,...),
					state="disabled"
				) #end of .Tcl.args
			) #end of paste
		) #end of .Tcl
	) #end of Try
	Try(xscr <- tkscrollbar(ttViewRNATargets,orient="horizontal", command=function(...)tkxview(table1,...)))
	Try(yscr <- tkscrollbar(ttViewRNATargets,command=function(...)tkyview(table1,...)))
	Try(tkgrid(table1,yscr))
	Try(tkgrid.configure(yscr,sticky="nsw"))
	Try(tkgrid(xscr))
	Try(tkgrid.configure(xscr,sticky="new"))
	Try(tkconfigure(table1,font=.affylmGUIglobals$affylmGUIfontTable))
	#
	for (j in (1:NumCols)){
		Try(tcl(.Tk.ID(table1),"width",paste(j-1),paste(max(4,nchar(colnames(Targets)[j])+2,max(nchar(Targets[,j]))+2))))
	}
	#
	Try(copyFcn <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))
	#
	saveTargetsFile <- function(){
		Try(TargetsFileNameText <- tclvalue(tkgetSaveFile(filetypes="{{RNA Targets Files} {.txt}} {{All files} *}")))
		Try(if(!nchar(TargetsFileNameText)) return())
		Try(len <- nchar(TargetsFileNameText))
		if(len<=4){
			Try(	TargetsFileNameText <- paste(TargetsFileNameText,".txt",sep=""))
		}else if(substring(TargetsFileNameText,len-3,len)!=".txt"){
			Try(TargetsFileNameText <- paste(TargetsFileNameText,".txt",sep=""))
		}
		Try(Targets <- matrix(nrow=NumRows,ncol=NumCols))
		Try(colnamesTargets <- c())
		if(NumCols>0){
			Try(
				for (j in (1:NumCols)){
					colnamesTargets[j] <- tclvalue(paste(tclArrayName,"(0,",j-1,")",sep=""))
				}
			)
		} #end of if(NumCols>0)
		Try(colnames(Targets) <- colnamesTargets)
		if(NumRows>0 && NumCols>0){
			Try(
				for (i in (1:NumRows)){
					for (j in (1:NumCols)){
						Targets[i,j] <- tclvalue(paste(tclArrayName,"(",i,",",j-1,")",sep=""))
					} #end of for (j in (1:NumCols))
				} #end of for (i in (1:NumRows))
			) #end of Try
		} #end of if(NumRows>0 && NumCols>0)
		Try(write.table(Targets,file=TargetsFileNameText,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
	} #end of saveTargetsFile <- function()
	#
	Try(topMenu  <- tkmenu(ttViewRNATargets, tearoff=FALSE))
	Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(tkadd(fileMenu, "command", label="Save As", command=saveTargetsFile)) # ) # ,font=affylmGUIfontMenu))
	Try(tkadd(fileMenu, "command", label="Close",   command=onClose))         # ) # ,font=affylmGUIfontMenu))
	Try(tkadd(topMenu,	"cascade", label="File",    menu=fileMenu))           # ) # ,font=affylmGUIfontMenu))
	#
	Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>", command=copyFcn))   # ) # ,font=affylmGUIfontMenu))
	Try(tkadd(topMenu,	"cascade", label="Edit",          menu=editMenu))     # ) # ,font=affylmGUIfontMenu))
	#
	Try(tkconfigure(ttViewRNATargets,menu=topMenu))
	#
	Try(tkfocus(ttViewRNATargets))
	Try(tkbind(ttViewRNATargets, "<Destroy>", function() {Try(tkfocus(.affylmGUIglobals$ttMain))}))
}# end of function ViewContrastsMatrixAsPairs
#
########################################################################################################
#
# Some C-style string searching functions, because I'm not very good at using regular expressions ;-)
#
# Returns the index where needle is found in haystack or zero if not found.
nstrstr <- function(haystack,needle){
	lenHaystack    <- nchar(haystack)
	lenNeedle      <- nchar(needle)
	#
	if(lenHaystack < lenNeedle){
		return (0)
	}
	if(lenHaystack == lenNeedle){
		return(haystack==needle)
	}
	lenDiff <- lenHaystack-lenNeedle
	for (i in (1:lenDiff)){
		if(needle==substr(haystack,i,i+lenNeedle-1)){
			return(i)
		}
	} #end of for
	#
	return (0)
} #end of OK.but <-tkbutton
#
#
#
strstr <- function(haystack,needle){
	strIndex <- nstrstr(haystack,needle)
	if(strIndex==0)
		return ("")
	return (substr(haystack,strIndex,nchar(haystack)))
} #end of strstr <- function(haystack,needle)
#
#
#
ComputeContrasts <- function(){
	# For now, we will assume that the number of contrasts is one less than the number of parameters,
	# e.g. with 4 treatments, we estimate 4 parameters, then 3 contrasts.
	#
	Try(NumParameters                         <- get("NumParameters",                       envir=affylmGUIenvironment))
	Try(Targets                               <- get("Targets",                             envir=affylmGUIenvironment))
	Try(NumContrastParameterizations          <- get("NumContrastParameterizations",        envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec      <- get("ContrastParameterizationNamesVec",    envir=affylmGUIenvironment))
	Try(ContrastParameterizationList          <- get("ContrastParameterizationList",        envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec  <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(ArraysLoaded	                        <- get("ArraysLoaded",                        envir=affylmGUIenvironment))
	Try(LinearModelFit.Available              <- get("LinearModelFit.Available",            envir=affylmGUIenvironment))
	#
	if(ArraysLoaded==FALSE){
		Try(tkmessageBox(title="Compute Contrasts",message="No arrays have been loaded.	Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	}#end of if(ArraysLoaded==FALSE)
	#
	if(LinearModelFit.Available==FALSE){
		Try(ComputeLinearModelFit())
		Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
		#Try(tkmessageBox(title="Compute Contrasts",message="There is no linear model fit available.	Select \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
		#Try(tkfocus(.affylmGUIglobals$ttMain))
		#return()
	}#end of if(LinearModelFit.Available==FALSE)
	Try(fit    <- get("fit",	 envir=affylmGUIenvironment))
	Try(design <- get("design",envir=affylmGUIenvironment))
	#
	Try(ParameterNamesVec	<- colnames(design))
	#
	Try(NumContrasts <- NumParameters - 1)
	#
	Try(
		if(NumContrasts<=0){
			tkmessageBox(title="Compute Contrasts",message=paste("You need to have two or more treatments in order to compute contrasts."),type="ok",icon="error")
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}#end of if(NumContrasts<=0)
	)#end of Try
	Try(NumContrasts <- min(NumContrasts,10))
	#
	Try(contrastsMatrixInList <- GetContrasts(NumContrasts=NumContrasts))
	Try(if(nrow(contrastsMatrixInList$contrasts)==0) return())
	Try(contrastsMatrix <- as.matrix(contrastsMatrixInList$contrasts))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(contrastsFit <- contrasts.fit(fit,contrastsMatrix))
	#
	# NEW
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(
		if(min(contrastsFit$df)==0){
			Try(tkmessageBox(title="No degrees of freedom",message="Empirical Bayes statistics will not be available because of the lack of replicate arrays.",icon="warning"))
			Try(ebayesAvailable <- FALSE)
		}else{
			Try(ebayesAvailable <- TRUE)
		}
	)#end of Try
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(if(ebayesAvailable==TRUE)
		Try(contrastsEbayes <- eBayes(contrastsFit)))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(ContrastParameterizationNameText <- GetContrastParameterizationName())
	Try(if(ContrastParameterizationNameText=="GetContrastParameterizationName.CANCEL") return())
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(
		while (nchar(ContrastParameterizationNameText)==0){
			Try(tkmessageBox(title="Contrasts Name",message="Please enter a name for this set of contrasts",type="ok",icon="error"))
			Try(ContrastParameterizationNameText <- GetContrastParameterizationName())
			if(ContrastParameterizationNameText=="GetContrastParameterizationName.CANCEL"){
				Try(tkfocus(.affylmGUIglobals$ttMain))
				return()
			}#end of if(ContrastParameterizationNameText=="GetContrastParameterizationName.CANCEL")
		}#end of while (nchar(ContrastParameterizationNameText)==0)
	)#end of Try
	#
	Try(contrastParameterizationIndex <- 0)
	Try(newContrastParameterization <- 1)
	Try(
		if(ContrastParameterizationNameText %in% ContrastParameterizationNamesVec){
			Try(contrastParameterizationIndex <- match(ContrastParameterizationNameText,ContrastParameterizationNamesVec))
			Try(mbVal<-tclvalue(tkmessageBox(title="Contrasts Parameterization Name",message="This contrasts parameterization name already exists.	Replace?",type="yesnocancel",icon="question")))
			Try(if(mbVal=="cancel") return())
			Try(if(mbVal=="yes") newContrastParameterization <- 0)
			Try(if(mbVal=="no") newContrastParameterization <- 1)
		}else{
			Try(newContrastParameterization <- 1)
		}#end of else/if(ContrastParameterizationNameText %in% ContrastParameterizationNamesVec)
	)#end of Try
	#
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(NumContrastParameterizations         <- get("NumContrastParameterizations",        envir=affylmGUIenvironment))
	#
	if(newContrastParameterization==1){
		Try(
			if(length(ContrastParameterizationTREEIndexVec)!=NumContrastParameterizations){
				Try(tkmessageBox(title="Contrasts Parameterizations","Length of ContrastParameterizationTREEIndexVec is not equal to NumContrastParameterizations.",type="ok",icon="error"))
				Try(tkfocus(.affylmGUIglobals$ttMain))
				return()
			}#end of if(length(ContrastParameterizationTREEIndexVec)!=NumContrastParameterizations)
		)#end of Try
		Try(NumContrastParameterizations <- NumContrastParameterizations + 1)
		Try(contrastParameterizationIndex <- NumContrastParameterizations)
		Try(
			if(length(ContrastParameterizationTREEIndexVec)==0){
				Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- 1)
			}else{
				Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- max(ContrastParameterizationTREEIndexVec)+1)
			}
		)#end of Try
		Try(ContrastParameterizationTREEIndexVec[contrastParameterizationIndex] <- .affylmGUIglobals$ContrastParameterizationTREEIndex)
		Try(ContrastParameterizationNamesVec <- c(ContrastParameterizationNamesVec,ContrastParameterizationNameText))
		Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
		Try(ContrastParameterizationList[[ContrastParameterizationNameNode]] <- ContrastParameterizationNameText)
	}else{ # Replace existing contrasts parameterization with the same name.
		Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
		Try(tkdelete(.affylmGUIglobals$ContrastParameterizationTREE,paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep="")))
	}#end of else/if(newContrastParameterization==1)
	#
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]] <- list())
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)
	###Try(NormalizedAffyData <- get("NormalizedAffyData",affylmGUIenvironment))
	Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",affylmGUIenvironment))
	Try(contrastsFit$Amean <- rowMeans(NormalizedAffyData.exprs))
	#
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$fit <- contrastsFit)
	Try(
		if(ebayesAvailable==TRUE){
			Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$eb	<- contrastsEbayes)
		}else{
			Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$eb	<- list())
		}
	)#end of Try
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$contrastsMatrixInList <- contrastsMatrixInList)
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$ContrastParameterizationNameText <- ContrastParameterizationNameText)
	#
	if(NumContrastParameterizations>0){
		Try(ContrastsNames <- colnames(contrastsMatrix))
	}
	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=.affylmGUIglobals$affylmGUIfontTree))
	Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
	#
	Try(ContrastsNode <- paste("ContrastsNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,ContrastsNode,text="Contrasts",font=.affylmGUIglobals$affylmGUIfontTree))
	#
	Try(
		for (j in (1:NumContrastsInContrastParameterization)){
			Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastsNode,paste("Contrasts.",.affylmGUIglobals$ContrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=.affylmGUIglobals$affylmGUIfontTree))
		}
	)#end of Try
	#
	Try(LinearModelFitNode			 <- paste("LinearModelFitNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
	Try(LinearModelFitStatusNode <- paste("LinearModelFitStatusNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,LinearModelFitNode,text="Linear Model Fit",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",LinearModelFitNode,LinearModelFitStatusNode,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(EmpiricalBayesNode			 <- paste("EmpiricalBayesNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
	Try(EmpiricalBayesStatusNode <- paste("EmpiricalBayesStatusNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,EmpiricalBayesNode,text="Empirical Bayes Statistics",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(if(ebayesAvailable==TRUE)
		Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
	else
		Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))			)
	Try(assign("ContrastParameterizationList",ContrastParameterizationList,affylmGUIenvironment))
	Try(assign("NumContrastParameterizations",NumContrastParameterizations,affylmGUIenvironment))
	Try(assign("ContrastParameterizationTREEIndexVec",ContrastParameterizationTREEIndexVec,affylmGUIenvironment))
	Try(assign("ContrastParameterizationNamesVec",ContrastParameterizationNamesVec,affylmGUIenvironment))
	#
	Try(
		if(NumContrastParameterizations>0){
			Try(for (i in (1:NumContrastParameterizations))
			Try(tkdelete(.affylmGUIglobals$mainTree,paste("ContrastParameterizations.Status.",i,sep=""))))
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"ContrastParameterizations.Status.1"))
		}
	)#end of Try
	Try(
		if(NumContrastParameterizations>0){
			for (contrastParameterizationIndex in (1:NumContrastParameterizations)){
				Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
				Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(tkinsert(.affylmGUIglobals$mainTree,"end","ContrastParameterizations",ContrastParameterizationsStatusNameNode ,text=ContrastParameterizationNamesVec[contrastParameterizationIndex],font=.affylmGUIglobals$affylmGUIfontTree))
			}#end of for (contrastParameterizationIndex in (1:NumContrastParameterizations))
		}else{
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
		}#end of else/if(NumContrastParameterizations>0)
	)#end of Try
	#
	tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow")
	Try(
		tkmessageBox(
			title="Contrasts Fit Complete",
			message=paste("Calculation of the contrasts fit is complete. ",
			"You can now view list(s) of differentially expressed genes, using the TopTable menu.")
		)#end of tkmessageBox
	)#end of Try
}#end of function ComputeContrasts
#
#
#
ComputeLinearModelFit <- function(){
	Try(ArraysLoaded	<- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if(ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Linear Model",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		} #end of if(ArraysLoaded==FALSE)
	)
	Try(NormalizedAffyData.Available   <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
	Try(if(NormalizedAffyData.Available==FALSE)NormalizeNow())
	Try(NormalizedAffyData.Available   <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
	Try(
		if(NormalizedAffyData.Available==FALSE){
			tkmessageBox(title="Linear Model",message="An error occured while trying to normalize the data.")
			return()
		} #end of if(NormalizedAffyData.Available==FALSE)
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	###Try(NormalizedAffyData        <- get("NormalizedAffyData",         envir=affylmGUIenvironment))
	Try(NormalizedAffyData.exprs     <- get("NormalizedAffyData.exprs",   envir=affylmGUIenvironment))
	Try(NormalizedAffyData.se.exprs  <- get("NormalizedAffyData.se.exprs",envir=affylmGUIenvironment))
	Try(Targets                      <- get("Targets",                    envir=affylmGUIenvironment))
	Try(design                       <- as.matrix(as.data.frame(model.matrix(~ -1 + factor(Targets$Target)))))
	Try(NumParameters                <- ncol(design))
	Try(assign("NumParameters",         NumParameters,                    affylmGUIenvironment))
	Try(colnames(design) <- gsub("factor\\(Targets\\$Target\\)","",colnames(design)))
	Try(rownames(design) <- Targets$FileName)
	Try(assign("design",                design,                           affylmGUIenvironment))
	#
	Try(
		if(exists("NormMethod",envir=affylmGUIenvironment)){
			Try(NormMethod <- get("NormMethod",envir=affylmGUIenvironment))
		}else{
			Try(NormMethod <- "RMA")
			Try(assign("NormMethod",NormMethod,affylmGUIenvironment))
		} #end of else/if(exists("NormMethod",envir=affylmGUIenvironment))
	)
	#
	Try(
		if(NormMethod=="PLM"){
			Try(
				if(length(NormalizedAffyData.se.exprs)>0){
					Try(weights <- 1/pmax(NormalizedAffyData.se.exprs, 1e-05)^2)
				}
			) #end of Try
			Try(fit <- lm.series(NormalizedAffyData.exprs,design,weights=weights))
		}else{
			Try(fit <- lm.series(NormalizedAffyData.exprs,design))
		}
	)#end of Try
	Try(assign("LinearModelFit.Available",TRUE,affylmGUIenvironment))
	Try(assign("fit",fit,affylmGUIenvironment))
	Try(tkdelete(.affylmGUIglobals$mainTree,"LinearModelFit.Status"))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(NumParameters <- get("NumParameters" , envir=affylmGUIenvironment))
	Try(
		if(NumParameters>0){
			Try(
				for (i in (1:NumParameters)){
					Try(tkdelete(.affylmGUIglobals$mainTree,paste("Parameters.Status.",i,sep="")))
				}
			)
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"Parameters.Status.1"))
		}
	)
	Try(
		for (i in (1:ncol(design))){
			Try(
				tkinsert(
					.affylmGUIglobals$mainTree,
					"end",
					"Parameters",
					paste("Parameters.Status.",i,sep="") ,
					text=colnames(design)[i],
					font=.affylmGUIglobals$affylmGUIfontTree
				) #end of tkinsert
			) #end of Try
		} #end of for (i in (1:ncol(design)))
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	#
	Try(
		tkmessageBox(
			title="Linear Model Fit Complete",
			message=paste(
				"Calculation of the linear model fit is complete. ",
				"Contrasts can now be computed (from the Linear Model menu)."
			) #end of message=paste
		) #end of tkmessageBox
	) #end of Try
}#end of ComputeLinearModelFit <- function()
#
#
#
GetContrast <- function(contrastParameterizationIndex){
	Try(ttGetContrast<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetContrast))
	Try(tkgrab.set(ttGetContrast)	)
	Try(tkfocus(ttGetContrast))
	Try(tkwm.title(ttGetContrast,"Choose a contrast"))
	Try(scr <- tkscrollbar(ttGetContrast, repeatinterval=5,command=function(...)tkyview(tl,...)))
	Try(xscr <- tkscrollbar(ttGetContrast, repeatinterval=5,command=function(...)tkxview(tl,...) ,orient="horizontal"))
	## Safest to make sure scr exists before setting yscrollcommand
	Try(tl<-tklistbox(ttGetContrast,height=4,width=30,selectmode="single",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),background="white",font=.affylmGUIglobals$affylmGUIfont2)	 )
	Try(lbl2<-tklabel(ttGetContrast,text="Which contrast is this for?",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttGetContrast,text="       "),row=0,column=1,columnspan=1))
	Try(tkgrid(tklabel(ttGetContrast,text="       "),row=0,column=4,columnspan=1))
	Try(tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1))
	Try(tkgrid.configure(lbl2,sticky="w"))
	Try(tkgrid(tklabel(ttGetContrast,text="         "),row=2,column=1))
	Try(tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew"))
	Try(tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns"))
	Try(tkgrid(xscr,row=6,column=2,columnspan=2,sticky="wne"))
	#
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	#
	Try(ContrastNamesVec	<- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
	#
	Try(NumContrasts <- length(ContrastNamesVec))
	#
	coefIndexList <- list()
	#
	if(NumContrasts>0){
		Try(
			for (i in (1:NumContrasts)){
				Try(tkinsert(tl,"end",ContrastNamesVec[i]))
			}
		)
	} #end of if(NumContrasts>0)
	#
	Try(tkselection.set(tl,0))
	#
	Try(ReturnVal <- list(coefIndex=0)) # Other attributes can be added later if necessary.
	#
	onOK <- function(){
		Try(contrastNum <- as.numeric(tclvalue(tkcurselection(tl)))+1)
		Try(tkgrab.release(ttGetContrast));
		Try(tkdestroy(ttGetContrast));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		Try(ReturnVal <<- list(contrastIndex=contrastNum))
	} #end of onOK <- function()
	#
	onCancel <- function() {
		Try(tkgrab.release(ttGetContrast));
		Try(tkdestroy(ttGetContrast));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		Try(ReturnVal <<- list(contrastIndex=0))
	} #end of onCancel <- function()
	#
	Try(OK.but <-tkbutton(ttGetContrast,text="   OK  ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(ttGetContrast,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttGetContrast,text="    ")))
	Try(tkgrid(tklabel(ttGetContrast,text="    "),tklabel(ttGetContrast,text="    "),OK.but,Cancel.but))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	#
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(tl, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	Try(tkgrid(tklabel(ttGetContrast,text="    ")))
	Try(tkfocus(ttGetContrast))
	Try(tkbind(ttGetContrast, "<Destroy>", function() {Try(tkgrab.release(ttGetContrast));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttGetContrast))
	#
	return (ReturnVal)
}#end of GetContrast <- function(contrastParameterizationIndex)
#
#
#
ExportTopTable <- function() showTopTable(export=TRUE)
#
#
#
showTopTable <- function(...,export=FALSE){
	Try(NumContrastParameterizations         <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList         <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(ArraysLoaded                         <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if(ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Top Table",message="No arrays have been loaded.	Please try New or Open from the File menu.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		} #end of if(ArraysLoaded==FALSE)
	) #end of Try
	Try(
		if(NumContrastParameterizations==0){
		Try(tkmessageBox(title="Top Table",message="There are no contrast parameterizations available.	Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
		} #end of if(NumContrastParameterizations==0)
	) #end of Try
	#Select which contrast parameterization the user wants
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	#If cancel pressed, then exit from this routine
	Try(if(contrastParameterizationIndex==0) return()) # Cancel
	#Store locally (as a dot variable), the index of chosen parametrization
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	#
	Try(ContrastNamesVec	<- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
	Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
	Try(if(GetContrastReturnVal$contrastIndex==0) return()) # Cancel
	Try(contrast <- GetContrastReturnVal$contrastIndex)
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	#Now have the contrast that we are going to use
	#get the fit object
	Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
	#	Try(eb	<- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$eb)
	#Check eb is available
	Try(
		if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
			length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0){
			Try(ebayesAvailable <- TRUE)
		}else{
			Try(ebayesAvailable <- FALSE)
		}
	) #end of Try
	#
	# This is a bit silly, calculating it again.	This should be tidied up later.	But basically, we're
	# checking whether we had degrees of freedom > 0 from the linear model fit (i.e. were there any
	# replicate arrays?)	If so, eBayes should work, and we can use Gordon's new method (adding new
	# attributes to the fit object rather than using eb), because this seems to work best with topTable,
	# affy data etc.
	Try(
		if(ebayesAvailable==TRUE)
			Try(fit <- eBayes(fit))
	)
	Try(ttToptableDialog<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttToptableDialog))
	Try(tkgrab.set(ttToptableDialog))
	Try(tkfocus(ttToptableDialog))
	Try(tkwm.title(ttToptableDialog,"Toptable Options"))
	Try(tkgrid(tklabel(ttToptableDialog,text="    ")))
	#
	Try(frame1 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
	Try(HowManyQuestion1 <- tklabel(frame1,text=
		"Number of genes in table:",font=.affylmGUIglobals$affylmGUIfont2)
	)
	Try(tkgrid(HowManyQuestion1))
	Try(tkgrid.configure(HowManyQuestion1,columnspan=2,sticky="w"))
	#
	Try(
		if(export){
			Try(numberOfGenesTcl <- tclVar("5"))
		}else{
			Try(numberOfGenesTcl <- tclVar("3"))
		}
	) #end of Try
	Try(Ten.but       <- tkradiobutton(frame1,text="10",        variable=numberOfGenesTcl,value="1",font=.affylmGUIglobals$affylmGUIfont2))
	Try(Thirty.but    <- tkradiobutton(frame1,text="30",        variable=numberOfGenesTcl,value="2",font=.affylmGUIglobals$affylmGUIfont2))
	Try(Fifty.but     <- tkradiobutton(frame1,text="50",        variable=numberOfGenesTcl,value="3",font=.affylmGUIglobals$affylmGUIfont2))
	Try(Hundred.but   <- tkradiobutton(frame1,text="100",       variable=numberOfGenesTcl,value="4",font=.affylmGUIglobals$affylmGUIfont2))
	Try(AllGenes.but  <- tkradiobutton(frame1,text="All genes", variable=numberOfGenesTcl,value="5",font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(Ten.but,     sticky="w"))
	Try(tkgrid(Thirty.but,  sticky="w"))
	Try(tkgrid(Fifty.but,   sticky="w"))
	Try(tkgrid(Hundred.but, sticky="w"))
	Try(tkgrid(AllGenes.but,sticky="w"))
	Try(tkgrid.configure(HowManyQuestion1,Ten.but,Thirty.but,Fifty.but,Hundred.but,AllGenes.but,sticky="w"))
	#
	Try(frame2 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
	Try(sortByLabel <- tklabel(frame2,text="Sort by:",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(sortByLabel,sticky="w"))
	Try(tkgrid.configure(sortByLabel,sticky="w"))
	Try(
		if(ebayesAvailable==TRUE)
			Try(sortByTcl <- tclVar("B"))
		else
			Try(sortByTcl <- tclVar("M"))
	)
	#
	Try(M.but <- tkradiobutton(frame2,text="M",           variable=sortByTcl,value="M",font=.affylmGUIglobals$affylmGUIfont2))
	Try(A.but <- tkradiobutton(frame2,text="A",           variable=sortByTcl,value="A",font=.affylmGUIglobals$affylmGUIfont2))
	Try(T.but <- tkradiobutton(frame2,text="t statistic", variable=sortByTcl,value="T",font=.affylmGUIglobals$affylmGUIfont2))
	Try(P.but <- tkradiobutton(frame2,text="P value",     variable=sortByTcl,value="P",font=.affylmGUIglobals$affylmGUIfont2))
	Try(B.but <- tkradiobutton(frame2,text="B statistic", variable=sortByTcl,value="B",font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(M.but,sticky="w"))
	Try(tkgrid(A.but,sticky="w"))
	Try(tkgrid(T.but,sticky="w"))
	Try(tkgrid(P.but,sticky="w"))
	Try(tkgrid(B.but,sticky="w"))
	#
	Try(
		if(ebayesAvailable==FALSE){
			Try(tkconfigure(T.but,state="disabled"))
			Try(tkconfigure(P.but,state="disabled"))
			Try(tkconfigure(B.but,state="disabled"))
		}
	) #end of Try
	#
	Try(frame3            <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
	Try(adjustMethodLabel <- tklabel(frame3,text="Adjust method:",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid          (adjustMethodLabel,sticky="w"))
	Try(tkgrid.configure(adjustMethodLabel,sticky="w"))
	Try(
		if(ebayesAvailable==TRUE)
			Try(adjustMethodTcl <- tclVar("BH"))
		else
			Try(adjustMethodTcl <- tclVar("none"))
	) #end of Try
	Try(none.but <- tkradiobutton(frame3,text="None",variable=adjustMethodTcl,value="none",font=.affylmGUIglobals$affylmGUIfont2))
	Try(bh.but	 <- tkradiobutton(frame3,text="BH"  ,variable=adjustMethodTcl,value="BH"  ,font=.affylmGUIglobals$affylmGUIfont2))
	Try(by.but	 <- tkradiobutton(frame3,text="BY"  ,variable=adjustMethodTcl,value="BY"  ,font=.affylmGUIglobals$affylmGUIfont2))
	Try(holm.but <- tkradiobutton(frame3,text="Holm",variable=adjustMethodTcl,value="holm",font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(none.but,sticky="w"))
	Try(tkgrid(bh.but	, sticky="w"))
	Try(tkgrid(by.but	, sticky="w"))
	Try(tkgrid(holm.but,sticky="w"))
	#
	Try(
		if(ebayesAvailable==FALSE){
			Try(tkconfigure(none.but,state="disabled"))
			Try(tkconfigure(bh.but	,state="disabled"))
			Try(tkconfigure(by.but	,state="disabled"))
			Try(tkconfigure(holm.but,state="disabled"))
		} #end of if(ebayesAvailable==FALSE)
	) #end of Try
	Try(totalGenes    <- nrow(fit$coefficients))
	Try(Abort         <- 1)
	Try(numberOfGenes <- 50)
	Try(sortBy        <- "B")
	Try(adjustMethod  <- "BH")
	Try(
		onOK <- function(){
			Try(NumGenesChoice <- as.numeric(tclvalue(numberOfGenesTcl)))
			Try(tkgrab.release(ttToptableDialog))
			Try(tkdestroy(ttToptableDialog))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			Try(NumbersOfGenes <- c(10,30,50,100,totalGenes))
			Try(numberOfGenes  <<- NumbersOfGenes[NumGenesChoice])
			Try(sortBy         <<- tclvalue(sortByTcl))
			Try(adjustMethod	 <<- tclvalue(adjustMethodTcl))
			Try(Abort          <<- 0)
			#store these values for the HTML report
			Try(assign("numberOfGenes", numberOfGenes, affylmGUIenvironment))
			Try(assign("sortBy",        sortBy,        affylmGUIenvironment))
			Try(assign("adjustMethod",  adjustMethod,  affylmGUIenvironment))
		} #end of onOK <- function()
	) #end of Try
	Try(onHelp     <- function() Try(help("topTable",htmlhelp=TRUE)))
	Try(frame4     <- tkframe(ttToptableDialog,borderwidth=2))
	Try(onCancel   <- function() {Try(tkgrab.release(ttToptableDialog));Try(tkdestroy(ttToptableDialog));Try(tkfocus(.affylmGUIglobals$ttMain));Abort <<- 1})
	Try(OK.but     <-tkbutton(frame4,text="   OK   ",command=onOK,    font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(frame4,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Help.but   <-tkbutton(frame4,text=" Help ",  command=onHelp,  font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(frame4,text="    "),OK.but,Cancel.but,Help.but,tklabel(frame4,text="    ")))
	#
	Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame1,frame2,tklabel(ttToptableDialog,text="  ")))
	Try(tkgrid(tklabel(ttToptableDialog,text="    ") ))
	Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame3,frame4,tklabel(ttToptableDialog,text="  ")))
	Try(tkgrid(tklabel(ttToptableDialog,text="    ") ))
	Try(tkgrid.configure(frame1,frame3,sticky="w"  ) )
	#
	Try(tkfocus(ttToptableDialog))
	Try(tkbind(ttToptableDialog, "<Destroy>", function() {Try(tkgrab.release(ttToptableDialog));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttToptableDialog))
	#
	Try(
		if(Abort==1){
			return()
		}
	) #end of Try
	#
	Try(
		if(numberOfGenes==totalGenes){
			tkconfigure(.affylmGUIglobals$ttMain,cursor="watch")
			Try(tkfocus(.affylmGUIglobals$ttMain))
		} #end of if(numberOfGenes==totalGenes)
	) #end of Try
	#
	Try(options(digits=3))
	#
	Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
	Try(cdfName <- strsplit(cleancdfname(cdfName(RawAffyData)),"cdf")[[1]])
	if(!(cdfName %in% .packages(all.available=TRUE))){
		Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))
		Try(assign("cdfName",cdfName,affylmGUIenvironment))
	} #end of if(!(cdfName %in% .packages(all.available=TRUE)))
	Try(cdfenv      <- getCdfInfo(RawAffyData))
	Try(genelist    <- data.frame(ID=I(ls(cdfenv))))
	Try(geneNames   <- get("geneNames",  envir=affylmGUIenvironment))
	Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))
	Try(
		if(length(geneNames)==0|| length(geneSymbols)==0){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
			Try(cdfName <- strsplit(cleancdfname(cdfName(RawAffyData)),"cdf")[[1]])
			if(!(cdfName %in% .packages(all.available=TRUE))){
				Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))###inserted by keith
			}
			Try(
				if( (cdfName %in% .packages(all.available=TRUE)) ){
					Require(cdfName)
					Try(code2eval <- paste("Try(geneNames <- as.character(unlist(lapply(mget(ls(cdfenv),env=",cdfName,"GENENAME),function(nm) return(paste(nm,collapse=\"; \"))))))",sep=""))
					Try(eval(parse(text=code2eval)))
					Try(assign("geneNames",geneNames,affylmGUIenvironment))
					Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(lapply(mget(ls(cdfenv),env=",cdfName,"SYMBOL),function(sym) return(paste(sym,collapse=\"; \"))))))",sep=""))
					Try(eval(parse(text=code2eval)))
					Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))
					Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
					Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
					Try(colnames(genelist) <- c("ID","Symbol","Name"))
				}else{
					Try(genelist <- data.frame(ID=I(ls(cdfenv))))
					Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				}
			)
		}else{
			Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
			Try(colnames(genelist) <- c("ID","Symbol","Name"))
		}
	)
	Try(fit$genes <- genelist)
	###Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
	Try(NormalizedAffyData.exprs    <- get("NormalizedAffyData.exprs",   envir=affylmGUIenvironment))
	Try(NormalizedAffyData.se.exprs <- get("NormalizedAffyData.se.exprs",envir=affylmGUIenvironment))
	Try(if(!("Amean" %in% names(fit)))fit$Amean <- rowMeans(NormalizedAffyData.exprs))
	#
	# Note that it is difficult to use the limma toptable/topTable functions if you don't have ebayes statistics, so
	# in the case of no replicate arrays (no residual degrees of freedom) we will just do our own sorting.
	#
	Try(
		if(ebayesAvailable==FALSE){
			Try(M <- as.matrix(fit$coef)[,contrast])
			Try(A <- fit$Amean)
			Try(ord <- switch(sortBy, M = order(abs(M), decreasing = TRUE), A = order(A, decreasing = TRUE)))
			Try(top <- ord[1:numberOfGenes])
			Try(table1 <- data.frame(genelist[top, ,drop=FALSE], M = M[top], A=A[top]))
			Try(rownames(table1) <- as.character(1:length(M))[top])
		}
	)
	#
	# The 2's in front of toptables mean that they use the drop=FALSE option (even if the user hasn't upgraded limma since the 1.3 BioC release.)
	#	Try(table1 <- toptable2(coef=contrast,number=numberOfGenes,fit=fit,eb=eb,genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy))
	Try(
		if(ebayesAvailable==TRUE){
			Try(table1 <- topTable2(coef=contrast,number=numberOfGenes,fit=fit,genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy))
		}
	)
	#Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))
	#
	Try(nrows <- nrow(table1))
	Try(ncols <- ncol(table1))
	#
	SaveTopTable <- function(){
		Try(FileName <- tclvalue(tkgetSaveFile(initialfile=paste("toptable", contrast,".xls",sep=""),filetypes="{{Tab-Delimited Text Files} {.txt .xls}} {{All files} *}")))
		Try(if(!nchar(FileName))
				return())
		Try(write.table(table1,file=FileName,quote=FALSE,col.names=NA,sep="\t"))
	} #end of SaveTopTable <- function()
	#
	Try(
		if(export){
			Try(SaveTopTable())
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		}
	)
	#
	Try(
		if(nrows <=100){
			Try(ttToptableTable <- tktoplevel(.affylmGUIglobals$ttMain))
			Try(
				tkwm.title(
					ttToptableTable,
					paste(
						"Top",
						numberOfGenes,
						"Candidate Genes for Differential Expression for",
						ContrastNamesVec[contrast],
						", Adjust Method = ",
						adjustMethod,
						".",
						sep=" "
					) #end of paste
				) #end of tkwm.title
			) #end of Try
			TclRequire("Tktable")
			Try(
				toptableTable <- tkwidget(
					ttToptableTable,"table",
					xscrollcommand=function(...) tkset(xscr,...),
					yscrollcommand=function(...) tkset(yscr,...),
					rows=nrows+1,
					cols=ncols,
					titlerows=1,
					width=ncols,
					selectmode="extended",
					colwidth="13",
					background="white",
					rowseparator="\"\n\"",
					colseparator="\"\t\"",
					resizeborders="col",
					multiline="0",
					state="disabled",
					font=.affylmGUIglobals$affylmGUIfontTopTable
				) #end of toptableTable <- tkwidget
			) #end of Try
			Try(xscr <-tkscrollbar(ttToptableTable,orient="horizontal", command=function(...)tkxview(toptableTable,...)))
			Try(yscr <- tkscrollbar(ttToptableTable,command=function(...)tkyview(toptableTable,...)))
			Try(tclArrayVar1 <- tclArrayVar())
			Try(tclArrayName <- ls(tclArrayVar1$env))
			Try(tcl("set",paste(tclArrayName,"0,0",sep=""),""))
			Try(
				for (j in (1:ncols)){
					Try(tcl("set",paste(tclArrayName,"(",0,",",j-1,")",sep=""),colnames(table1)[j]))
				}
			)
			Try(
				for (i in (1:nrows)){
					for (j in (1:ncols)){
						Try(
							if(is.numeric(table1[i,j]))
								item <- format(table1[i,j],digits=4)
							else
								item <- table1[i,j]

						) #end of Try
						Try(tcl("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(item)))
					} #end of for (j in (1:ncols))
				} #end of for (i in (1:nrows))
			) #end of Try
			Try(tkgrid(toptableTable,yscr))
			Try(tkgrid.configure(toptableTable,sticky="news"))
			Try(tkgrid.configure(yscr,sticky="nsw"))
			Try(tkgrid(xscr,sticky="new"))
			Try(tkconfigure(toptableTable,bg="white",variable=tclArrayName))
			Try(
				for (i in (1:ncols)){
					if(tolower(colnames(table1)[i]) %in% c("block","row","column","gridrow","gridcolumn","gridcol","grid.row","grid.col","grid.column")){
						Try(
							if(.affylmGUIglobals$affylmGUIpresentation==FALSE){
								Try(tcl(toptableTable,"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i])+2))))
							}else{
								Try(tcl(toptableTable,"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i])))))
							}
						) #end of Try
						next()
					} #end of if(tolower(colnames(table1)[i]) %in% c("block","row","column","gridrow","gridcolumn","gridcol","grid.row","grid.col","grid.column"))
					if(colnames(table1)[i] %in% c("M","A","t","B")){
						Try(tcl(toptableTable,"width",paste(i-1),"6"))
						next()
					} #end of if(colnames(table1)[i] %in% c("M","A","t","B"))
					if(colnames(table1)[i] == "P.Value"){
						Try(tcl(toptableTable,"width",paste(i-1),"8"))
						next()
					} #end of if(colnames(table1)[i] == "P.Value")
					if(tolower(colnames(table1)[i]) == "name"){
						Try(tcl(toptableTable,"width",paste(i-1),paste(30)))
						Try(tcl(toptableTable,"tag","col","namecol",paste(i-1)))
						Try(tcl(toptableTable,"tag","cell","namecolheading",paste("0",i-1,sep=",")))
						Try(tcl(toptableTable,"tag","configure","namecol",anchor="w"))
						Try(tcl(toptableTable,"tag","configure","namecolheading",anchor="center"))
						next()
					} #end of if(tolower(colnames(table1)[i]) == "name")
					if(tolower(colnames(table1)[i]) == "id"){
						Try(tcl(toptableTable,"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),40))))
						next()
					} #end of if(tolower(colnames(table1)[i]) == "id")
					if(tolower(colnames(table1)[i]) == "symbol"){
						Try(tcl(toptableTable,"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),30))))
						next()
					} #end of if(tolower(colnames(table1)[i]) == "symbol")
					Try(tcl(toptableTable,"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),40))))
				} #end of for (i in (1:ncols))
			) #end of Try
			Try(tkfocus(toptableTable))
		}else{ #end of if(nrows <=100)
			Try(tkmessageBox(title="Large Toptable",message="Toptable is too large to display in a table widget, so it will be displayed in a text window instead.	You can save it as a tab-delimited text file and then import it into a spreadsheet program.",icon="info",type="ok"))
			Try(tempfile1 <- tempfile())
			write.table(table1,file=tempfile1,quote=FALSE,col.names=NA,sep="\t")
			ttToptableTable <- tktoplevel(.affylmGUIglobals$ttMain)
			tkwm.title(
				ttToptableTable,
				paste(
					"Top",
					numberOfGenes,
					"Candidate Genes for Differential Expression for",
					ContrastNamesVec[contrast],
					", Adjust Method = ",
					adjustMethod,
					".",
					sep=" "
				) #end of paste
			) #end of tkwm.title
			xscr <-tkscrollbar(
				ttToptableTable,
				repeatinterval=5,
				orient="horizontal",
				command=function(...)tkxview(txt,...)
			) #end of xscr <-tkscrollbar
			scr <- tkscrollbar(
				ttToptableTable,
				repeatinterval=5,
				command=function(...)tkyview(txt,...)
			) #end of scr <- tkscrollbar
			txt <- tktext(
				ttToptableTable,
				bg="white",
				font="courier",
				xscrollcommand=function(...)tkset(xscr,...),
				yscrollcommand=function(...)tkset(scr,...),
				wrap="none",
				width=100
			) #end of txt <- tktext
			copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>")))
			editPopupMenu2 <- tkmenu(txt, tearoff=FALSE)
			tkadd(
				editPopupMenu2,
				"command",
				label="Copy <Ctrl-C>",
				command=copyText2
			) # end of tkadd   # ,font=affylmGUIfontMenu
			RightClick2 <- function(x,y){ # x and y are the mouse coordinates
			 rootx <- as.integer(tkwinfo("rootx",txt))
			 rooty <- as.integer(tkwinfo("rooty",txt))
			 xTxt <- as.integer(x)+rootx
			 yTxt <- as.integer(y)+rooty
			 .Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt)))
			} #end of RightClick2 <- function(x,y)
			tkbind(txt, "<Button-3>",RightClick2)
			tkpack(scr, side="right", fill="y")
			tkpack(xscr, side="bottom", fill="x")
			tkpack(txt, side="left", fill="both", expand="yes")
			chn <- tclvalue(tclopen( tempfile1))
			tkinsert(txt, "end", tclvalue(tclread( chn)))
			tclclose( chn)
			tkconfigure(txt, state="disabled")
			tkmark.set(txt,"insert","0.0")
			tkfocus(txt)
			tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow")
		} #end of else/if(nrows <=100)
	) #end of try
	Try(copyFcn <-			function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(toptableTable),"<<Copy>>"))))
	topMenu2 <- tkmenu(ttToptableTable)
	tkconfigure(ttToptableTable, menu=topMenu2)
	fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
	tkadd(
		fileMenu2,
		"command",
		label="Save As",
		command=SaveTopTable
	) #end of tkadd   # ) # ,font=affylmGUIfontMenu)
	tkadd(
		fileMenu2,
		"command",
		label="Close",
		command=function() tkdestroy(ttToptableTable)
	) #end of tkadd   # ) # ,font=affylmGUIfontMenu)
	tkadd(
		topMenu2,
		"cascade",
		label="File",
		menu=fileMenu2
	) #end of tkadd    # ,font=affylmGUIfontMenu)
	editMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
	tkadd(
		editMenu2,
		"command",
		label="Copy <Ctrl-C>",
		command=copyFcn
	) #end of tkadd    # ,font=affylmGUIfontMenu)
	tkadd(
		topMenu2,
		"cascade",
		label="Edit",
		menu=editMenu2
	) #end of tkadd    # ,font=affylmGUIfontMenu)
}#end of function showTopTable
#
#
#
GetSlideNum <- function(all=FALSE){
	Try(SlideNamesVec <- get("SlideNamesVec",envir=affylmGUIenvironment))
	Try(
		if(min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE){
			SlideNamesVec <- paste("Slide",SlideNamesVec)
		}
	)
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	#
	ttGetSlideNum<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttGetSlideNum)
	tkgrab.set(ttGetSlideNum)
	tkfocus(ttGetSlideNum)
	tkwm.title(ttGetSlideNum,"Please Specify Slide")
	scr <- tkscrollbar(ttGetSlideNum, repeatinterval=5,command=function(...)tkyview(tl,...))
	## Safest to make sure scr exists before setting yscrollcommand
	tl<-tklistbox(ttGetSlideNum,height=4,selectmode="browse",yscrollcommand=function(...)tkset(scr,...),background="white",font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    ")
	)
	lbl2<-tklabel(ttGetSlideNum,text="Choose a slide",font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		lbl2,
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    ")
	)
	tkgrid.configure(lbl2,sticky="w")
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		row=2,
		column=0
	)
	tkgrid(tl,
		row=2,
		column=1,
		columnspan=2,
		rowspan=4,
		sticky="ew"
	)
	tkgrid(
		scr,
		row=2,
		column=3,
		rowspan=4,
		sticky="wns"
	)
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		row=2,
		column=4
	)
	for (i in (1:NumSlides)){
		tkinsert(tl,"end",SlideNamesVec[i])
		#Try(tkmessageBox(title="SlideNamesVec",message=paste("SlideNamesVec[",i,"] = \"",SlideNamesVec[i],"\""),icon="info",type="ok"))
	}
	if(all){
		tkinsert(tl,0,"All Slides")
	}
	tkselection.set(tl,0)
	#
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    ")
	)
	ReturnVal <- 0
	#
	onOK <- function(){
		slidenum <- as.numeric(tclvalue(tkcurselection(tl)))
		#Note that if the all slides option is true, slidenum range is 0,1,2,3...NumSlides
		#If Not all slide option, then slidenum range = 0,1,2,3...(NumSlides-1)
		if(all){
			#now make 1,000,000 to correspond to do all slides selected
			if(slidenum == 0)slidenum <- 1000000
			#slidenum will now return a value from 1,2,3...NumSlides or the value 1,000,000
		}else{
			slidenum <- slidenum + 1
			#slidenum will now return a value from 1,2,3...NumSlides.
		}
		Try(tkgrab.release(ttGetSlideNum));
		Try(tkdestroy(ttGetSlideNum));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- slidenum
	}#end of onOK <- function()
	#
	onCancel <- function() {
		Try(tkgrab.release(ttGetSlideNum));
		Try(tkdestroy(ttGetSlideNum));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		ReturnVal <<- 0
	}
	#
	OK.but <-tkbutton(ttGetSlideNum,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttGetSlideNum,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttGetSlideNum,text="    "),OK.but,Cancel.but,tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
	tkgrid.configure(OK.but,Cancel.but,sticky="w")
	tkgrid(
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    ")
	) #end of tkgrid
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(tl, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	Try(tkfocus(tl))
	Try(tkbind(ttGetSlideNum, "<Destroy>", function() {Try(tkgrab.release(ttGetSlideNum));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttGetSlideNum))
	#
	return (ReturnVal)
}#end of GetSlideNum <- function(all=FALSE)
#
#
#
GetDEcutoff <- function(){
	ttGetDEcutoff<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttGetDEcutoff)
	tkgrab.set(ttGetDEcutoff)
	Try(tkwm.title(ttGetDEcutoff,"Cutoff for Differentially Expressed Genes"))
	Try(cutoffStatisticTcl <- tclVar("abs(t)"))
	Try(tkframe1 <- tkframe(ttGetDEcutoff,borderwidth=2))
	Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
	Try(tkframe4<-tkframe(tkframe1,borderwidth=2))
	#
	Try(tkgrid(tklabel(tkframe1,text="    ")))
	#
	Try(tkgrid(tklabel(tkframe2,text="Choose a cutoff for differentially expressed genes.",font=.affylmGUIglobals$affylmGUIfont2),rowspan=1,columnspan=2,sticky="w"))
	#
	Try(tStatistic.but <- tkradiobutton(tkframe2,text="Abs(t)",variable=cutoffStatisticTcl,value="abs(t)",font=.affylmGUIglobals$affylmGUIfont2))
	Try(BStatistic.but <- tkradiobutton(tkframe2,text="B",variable=cutoffStatisticTcl,value="B",font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tStatistic.but))
	Try(tkgrid(BStatistic.but))
	Try(tkgrid.configure(tStatistic.but,BStatistic.but,sticky="w"))
	Try(tkgrid(tklabel(tkframe2,text="    ")))
	Try(cutoffValueTcl <- tclVar("0"))
	Try(entry.cutoffValue<-tkentry(tkframe2,width=30,font=.affylmGUIglobals$affylmGUIfont2,textvariable=cutoffValueTcl,bg="white"))
	Try(tkgrid(tklabel(tkframe2,text="Cutoff value ",font=.affylmGUIglobals$affylmGUIfont2),entry.cutoffValue,sticky="w"))
	#
	Try(tkgrid(tkframe2))
	Try(ReturnVal <- list())
	#
	onOK <- function(){
		Try(cutoffStatisticVal <- as.character(tclvalue(cutoffStatisticTcl)))
		Try(cutoffValue <- as.numeric(tclvalue(cutoffValueTcl)))
		Try(tkgrab.release(ttGetDEcutoff));Try(tkdestroy(ttGetDEcutoff));Try(tkfocus(.affylmGUIglobals$ttMain))
		Try(ReturnVal <<- list(cutoffStatistic=cutoffStatisticVal,cutoff=cutoffValue))
	} #end of onOK <- function()
	#
	onCancel <- function(){
		tkgrab.release(ttGetDEcutoff);
		tkdestroy(ttGetDEcutoff);
		tkfocus(.affylmGUIglobals$ttMain);
		ReturnVal <<- list()
	}
	#
	Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(tkframe4,text="                    ")))
	Try(tkgrid(OK.but,Cancel.but))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="e"))
	Try(tkgrid(tklabel(tkframe4,text="       ")))
	Try(tkgrid(tkframe4))
	Try(tkgrid(tkframe1))
	#
	Try(tkfocus(ttGetDEcutoff))
	Try(tkbind(ttGetDEcutoff, "<Destroy>", function(){tkgrab.release(ttGetDEcutoff);tkfocus(.affylmGUIglobals$ttMain);} ))
	Try(tkwait.window(ttGetDEcutoff))
	#
	return (ReturnVal)
} #end of GetDEcutoff <- function()
#
#
#
ChooseEbayesStatistic <- function(){
	ttChooseEbayesStatistic<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttChooseEbayesStatistic)
	tkgrab.set(ttChooseEbayesStatistic)
	tkfocus(ttChooseEbayesStatistic)
	tkwm.title(ttChooseEbayesStatistic,"Empirical Bayes Statistic")
	Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    ")))
	#
	Try(EbayesStatisticTcl <- tclVar("t"))
	#
	Try(tStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="t"))
	Try(BStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="lods"))
	Try(PValueRadioButton		 <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="p.value"))
	#
	Try(lbl2 <- tklabel(ttChooseEbayesStatistic,text="Please Choose an Empirical Bayes Statistic",font=.affylmGUIglobals$affylmGUIfont2))
	tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),lbl2)
	Try(tkgrid.configure(lbl2,columnspan=2,sticky="w"))
	tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
	#
	Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="t Statistic",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tStatisticRadioButton,currentLabel))
	Try(tkgrid.configure(tStatisticRadioButton,sticky="e"))
	Try(tkgrid.configure(currentLabel,sticky="w"))
	Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="B Statistic",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),BStatisticRadioButton,currentLabel))
	Try(tkgrid.configure(BStatisticRadioButton,sticky="e"))
	Try(tkgrid.configure(currentLabel,sticky="w"))
	Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="P Value",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),PValueRadioButton,currentLabel))
	Try(tkgrid.configure(PValueRadioButton,sticky="e"))
	Try(tkgrid.configure(currentLabel,sticky="w"))
	#
	tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
	tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
	ReturnVal <- ""
	#
	onOK <- function(){
			Try(ReturnVal <- tclvalue(EbayesStatisticTcl))
			Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkdestroy(ttChooseEbayesStatistic));Try(tkfocus(.affylmGUIglobals$ttMain))
			ReturnVal <<- ReturnVal
	} #end of onOK <- function()
	#
	onCancel <- function(){
		Try(tkgrab.release(ttChooseEbayesStatistic));
		Try(tkdestroy(ttChooseEbayesStatistic));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		ReturnVal <<- ""
	}
	#
	OK.but <-tkbutton(ttChooseEbayesStatistic,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttChooseEbayesStatistic,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	#
	tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),OK.but,Cancel.but,tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "))
	tkgrid.configure(OK.but,		sticky="e")
	tkgrid.configure(Cancel.but,sticky="w")
	tkgrid(
		tklabel(ttChooseEbayesStatistic,text="    "),
		tklabel(ttChooseEbayesStatistic,text="    "),
		tklabel(ttChooseEbayesStatistic,text="    "),
		tklabel(ttChooseEbayesStatistic,text="    "),
		tklabel(ttChooseEbayesStatistic,text="    ")
	) #end of tkgrid
	Try(tkfocus(ttChooseEbayesStatistic))
	Try(tkbind(ttChooseEbayesStatistic, "<Destroy>", function() {Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttChooseEbayesStatistic))
	#
	return (ReturnVal)
} #end of ChooseEbayesStatistic <- function()
#
#
#
GetWtAreaParams <- function(){
	ttWeightingwtArea <- tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttWeightingwtArea)
	tkgrab.set(ttWeightingwtArea)
	tkfocus(ttWeightingwtArea)
	tkwm.title(ttWeightingwtArea,"Good Spot Size")
	tkframe1 <- tkframe(ttWeightingwtArea)
	tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
	tkframe4 <- tkframe(tkframe1)
	tkgrid(tklabel(tkframe1,text="    "))
	tkgrid(tklabel(tkframe1,text="Please enter the area range for good spots",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
	tkgrid(tklabel(tkframe1,text="    "))
	tkgrid(tklabel(tkframe2,text="Area Range in Pixels",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
	AreaLowerLimitTcl <- tclVar(paste(160))
	AreaUpperLimitTcl <- tclVar(paste(170))
	tkgrid(tklabel(tkframe2,text="    "))
	entry.AreaLowerLimit <-tkentry(tkframe2,width="12",font=.affylmGUIglobals$affylmGUIfont2,textvariable=AreaLowerLimitTcl,bg="white")
	entry.AreaUpperLimit <-tkentry(tkframe2,width="12",font=.affylmGUIglobals$affylmGUIfont2,textvariable=AreaUpperLimitTcl,bg="white")
	tkgrid(tklabel(tkframe2,text="Lower area limit in pixels",font=.affylmGUIglobals$affylmGUIfont2),entry.AreaLowerLimit,sticky="w")
	tkgrid(tklabel(tkframe2,text="Upper area limit in pixels",font=.affylmGUIglobals$affylmGUIfont2),entry.AreaUpperLimit,sticky="w")
	tkgrid(tkframe2)
	tkgrid(tklabel(tkframe1,text="    "))
	ReturnVal <- 0
	AreaLowerLimitVal <- 0
	AreaUpperLimitVal <- 0
	#
	onOK <- function(){
		Try(AreaLowerLimitVal <<- as.integer(tclvalue(AreaLowerLimitTcl)))
		Try(AreaUpperLimitVal <<- as.integer(tclvalue(AreaUpperLimitTcl)))
		Try(assign("AreaLowerLimit",AreaLowerLimitVal,affylmGUIenvironment))
		Try(assign("AreaUpperLimit",AreaUpperLimitVal,affylmGUIenvironment))
		Try(assign("WeightingType",paste("wtarea, Ideal=(",AreaLowerLimitVal,",",AreaUpperLimitVal,")",sep=""),affylmGUIenvironment))
		Try(tkgrab.release(ttWeightingwtArea));Try(tkdestroy(ttWeightingwtArea));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- 1
	} #end of onOK <- function()
	#
	onCancel <- function(){
		Try(tkgrab.release(ttWeightingwtArea));
		Try(tkdestroy(ttWeightingwtArea));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		ReturnVal<<-0
	} #end of onCancel <- function()
	#
	OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	#
	tkgrid(OK.but,Cancel.but)
	tkgrid(tklabel(tkframe4,text="    "))
	tkgrid(tkframe4)
	tkgrid(tkframe1)
	#
	Try(tkfocus(ttWeightingwtArea))
	Try(tkbind(ttWeightingwtArea, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtArea));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttWeightingwtArea))
	#
	return (ReturnVal)
} #end of GetWtAreaParams <- function()
#
#
#
GetWtFlagParams <- function(){
	ttWeightingwtFlag <- tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttWeightingwtFlag)
	tkgrab.set(ttWeightingwtFlag)
	tkfocus(ttWeightingwtFlag)
	tkwm.title(ttWeightingwtFlag,"Weighting for Spots with Flag Values Less Than Zero")
	tkframe1 <- tkframe(ttWeightingwtFlag)
	tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
	tkframe4 <- tkframe(tkframe1)
	tkgrid(tklabel(tkframe1,text="    "))
	tkgrid(tklabel(tkframe1,text="Please enter the weighting for spots with flag values less than zero",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
	tkgrid(tklabel(tkframe1,text="    "))
	tkgrid(tklabel(tkframe2,text="Spot Weighting",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
	FlagSpotWeightingTcl <- tclVar(paste(0.1))
	tkgrid(tklabel(tkframe2,text="    "))
	entry.FlagSpotWeighting<-tkentry(tkframe2,width="12",font=.affylmGUIglobals$affylmGUIfont2,textvariable=FlagSpotWeightingTcl,bg="white")
	tkgrid(tklabel(tkframe2,text="Weighting (relative to 1 for all other spots)",font=.affylmGUIglobals$affylmGUIfont2),entry.FlagSpotWeighting,sticky="w")
	tkgrid(tkframe2)
	tkgrid(tklabel(tkframe1,text="    "))
	ReturnVal <- 0
	FlagSpotWeightingVal <- 0
	#
	onOK <- function(){
		Try(FlagSpotWeightingVal <- as.numeric(tclvalue(FlagSpotWeightingTcl)))
		Try(tkgrab.release(ttWeightingwtFlag));
		Try(tkdestroy(ttWeightingwtFlag));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- 1
	} #end of onOK <- function()
	#
	onCancel <- function(){
		Try(tkgrab.release(ttWeightingwtFlag));
		Try(tkdestroy(ttWeightingwtFlag));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		ReturnVal<<-0
	}
	#
	OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	#
	tkgrid(OK.but,Cancel.but)
	tkgrid(tklabel(tkframe4,text="    "))
	tkgrid(tkframe4)
	tkgrid(tkframe1)
	#
	Try(tkfocus(ttWeightingwtFlag))
	Try(tkbind(ttWeightingwtFlag, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtFlag));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttWeightingwtFlag))
	#
	Try(FlagSpotWeighting <- FlagSpotWeightingVal)
	Try(assign("FlagSpotWeighting", FlagSpotWeighting,affylmGUIenvironment))
	#
	Try(assign("WeightingType",paste("wtflag, FlagSpotWeighting = ",FlagSpotWeighting,sep=""),affylmGUIenvironment))
	#
	return (ReturnVal)
} #end of GetWtFlagParams <- function()
#
#
#
evalRcode <- function(){
	Try(wfile <- "")
	Try(ttEvalRcode <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.title(ttEvalRcode ,"Enter R code in this window and then click on Run"))
	Try(scrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,command=function(...)tkyview(txt,...)))
	Try(xscrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,orient="horizontal",command=function(...)tkxview(txt,...)))
	Try(
		txt <- tktext(
			ttEvalRcode,
			height=20,
			yscrollcommand=function(...)tkset(scrCode,...),
			xscrollcommand=function(...)tkset(xscrCode,...),
			wrap="none",width=100,bg="white",
			font=.affylmGUIglobals$affylmGUIfontCourier
		) #end of txt <- tktext
	)
	Try(cutText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Cut>>")))		)
	Try(copyText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))
	Try(pasteText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Paste>>")))	)
	#
	Try(editPopupMenu <- tkmenu(txt, tearoff=FALSE))
	Try(tkadd(editPopupMenu, "command", label="Cut <Ctrl-X>",  command=cutText))   # ) # ,font=affylmGUIfontMenu))
	Try(tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>", command=copyText))  # ) # ,font=affylmGUIfontMenu))
	Try(tkadd(editPopupMenu, "command", label="Paste <Ctrl-V>",command=pasteText)) # ) # ,font=affylmGUIfontMenu))
	#
	RightClick  <- function(x,y){ # x and y are the mouse coordinates
		Try(rootx <- as.integer(tkwinfo("rootx",txt)))
		Try(rooty <- as.integer(tkwinfo("rooty",txt)))
		Try(xTxt  <- as.integer(x)+rootx)
		Try(yTxt  <- as.integer(y)+rooty)
		Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu,xTxt,yTxt))))
	} #end of RightClick <- function(x,y)
	Try(tkbind(txt, "<Button-3>",RightClick))
	#
	Try(tkpack(scrCode, side="right", fill="y"))
	Try(tkpack(xscrCode, side="bottom", fill="x"))
	Try(tkpack(txt, side="left", fill="both", expand="yes"))
	Try(tkfocus(txt))
	#
	SaveRSourceFile <- function(){
		Try(fileName <- tclvalue(tkgetSaveFile(initialfile=tclvalue(tclfile.tail(wfile)),initialdir=tclvalue(tcltk:::tclfile.dir(wfile)),filetypes="{{R Source Files} {.R}} {{All files} *}")))
		if(nchar(fileName)==0) return()
		Try(len <- nchar(fileName))
		if(len<=2){
			Try( fileName <- paste(fileName,".R",sep=""))
		}else if(substring(fileName,len-1,len)!=".R"){
			Try(fileName <- paste(fileName,".R",sep=""))
		}
		Try(chn <- tkopen(fileName, "w"))
		Try(tkputs(chn, tclvalue(tkget(txt,"0.0","end"))))
		Try(tkclose(chn))
		Try(wfile <<- fileName)
		Try(tkfocus(txt))
	} #end of SaveRSourceFile <- function()
	#
	OpenRSourceFile <- function(){
		Try(fileName <- tclvalue(tkgetOpenFile(filetypes="{{R Source Files} {.R}} {{All files} *}")))
		if(nchar(fileName)==0) return()
		Try(chn <- tclopen(fileName))
		Try(tkinsert(txt, "0.0", tclvalue(tclread(chn))))
		Try(tclclose(chn))
		Try(wfile <<- fileName)
		Try(tkfocus(txt))
	} #end of OpenRSourceFile <- function()
	#
	runOverall <- function(runType){
		Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
		Try(tkconfigure(ttEvalRcode,cursor="watch"))
		Try(tkfocus(ttEvalRcode))
		Try(code <- tclvalue(tkget(txt,"0.0","end")))
		if(runType!="runTextOnly"){
			Try(
				if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
					Try(LocalHScale <- .affylmGUIglobals$Myhscale)
					Try(LocalVScale <- .affylmGUIglobals$Myvscale)
					Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
					Try(tkwm.withdraw(ttGraph))
					Try(tkwm.title(ttGraph,"Graphical Results from R Code Evaluation"))
				} #end of if(.affylmGUIglobals$graphicsDevice=="tkrplot")
			) #end of Try
			Try(
				codeGraph <- paste(
					"assign(\"plotFunction\",function () {\nopar<-par(bg=\"white\")\nTry({\n",code,"\n})\n\ntempGraphPar <- par(opar)\n},affylmGUIenvironment)\n",sep=""
				) #end of codeGraph <- paste
			)
		} #end of if(runType!="runTextOnly")
		#
		if(runType!="runGraphicsOnly"){
			Try(tmpEvalRcodeResults <- tempfile())
			Try(RoutFileObject <- file(tmpEvalRcodeResults, open="wt"))
			Try(sink(RoutFileObject))
			Try(sink(RoutFileObject,type="message"))
			Try(e <- try(parse(text=code)))
			if(inherits(e, "try-error")){
				Try(tkmessageBox(message="Syntax error",icon="error"))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				Try(sink(type="message"))
				Try(sink())
				Try(try(close(RoutFileObject),TRUE))
				return()
			} #end of if(inherits(e, "try-error"))
			e2 <- try(print(eval(e,envir=affylmGUIenvironment)))
			if(inherits(e2, "try-error")){
				Try(tkmessageBox(title="An error occured while trying to evaluate your R code",message=as.character(e2),icon="error"))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				Try(sink(type="message"))
				Try(sink())
				Try(try(close(RoutFileObject),TRUE))
				return()
			} #end of if(inherits(e2, "try-error"))
			Try(sink(type="message"))
			Try(sink())
			Try(try(close(RoutFileObject),TRUE))
		} #end of if(runType!="runGraphicsOnly")
		#
		if(runType!="runTextOnly"){
			Try(tmpEvalRcodeResults <- tempfile())
			Try(RoutFileObjectGraph <- file(tmpEvalRcodeResultsGraph,open="wt"))
			Try(sink(RoutFileObjectGraph))
			Try(sink(RoutFileObjectGraph,type="message"))
			Try(e3 <- try(parse(text=codeGraph)))
			if(inherits(e3, "try-error")){
				Try(tkmessageBox(message="Syntax error",icon="error"))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				Try(sink(type="message"))
				Try(sink())
				Try(close(RoutFileObjectGraph))
				return()
			} #end of if(inherits(e3, "try-error"))
			e4 <- try(print(eval(e3,envir=affylmGUIenvironment)))
			if(inherits(e4, "try-error")){
				Try(tkmessageBox(message="An error occured while trying to plot the graph(s) for your R code",icon="error"))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				Try(sink(type="message"))
				Try(sink())
				Try(close(RoutFileObjectGraph))
				return()
			} #end of if(inherits(e4, "try-error"))
			Try(sink(type="message"))
			Try(sink())
			Try(try(close(RoutFileObjectGraph),TRUE))
			#
			Try(
				if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
					Require("tkrplot")
					Try(plotFunction <- get("plotFunction",envir=affylmGUIenvironment))
					Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
					SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
					SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
					#
					Try(tkgrid(imgaffylmGUI))
					Try(
						if(as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){ # Nothing plotted.
							Try(tkdestroy(ttGraph))
						}else{
							Try(tkwm.deiconify(ttGraph))
							Try(tkfocus(imgaffylmGUI))
						} #end of else/if(as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)
					)
					#
					CopyToClip <- function(){
						Try(tkrreplot(imgaffylmGUI))
					} #end of CopyToClip <- function()
				}else{ #not using tkrplot, bur R window
					Try(plot.new())
					Try(plotFunction())
				} #end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
			) #end of Try
		} #end of if(runType!="runTextOnly")
		#
		if(runType!="runGraphicsOnly"){
			Try(tt2 <-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.title(tt2,"Text Results of R Code Evaluation"))
			Try(scr <- tkscrollbar(tt2, repeatinterval=5,command=function(...)tkyview(txt2,...)))
			Try(xscr <- tkscrollbar(tt2, repeatinterval=5,orient="horizontal",command=function(...)tkxview(txt2,...)))
			Try(
				txt2 <- tktext(
					tt2,
					height=20,bg="white",
					yscrollcommand=function(...)tkset(scr,...),
					xscrollcommand=function(...)tkset(xscr,...),
					wrap="none",
					width=100,
					font=.affylmGUIglobals$affylmGUIfontCourier
				) #end of txt2 <- tktext
			)
			#
			Try(copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt2),"<<Copy>>"))))
			#
			Try(editPopupMenu2 <- tkmenu(txt2, tearoff=FALSE))
			Try(tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",
			command=copyText2)) # ) # ,font=affylmGUIfontMenu))
			#
			RightClick2 <- function(x,y){ # x and y are the mouse coordinates
			 Try(rootx  <- as.integer(tkwinfo("rootx",txt2)))
			 Try(rooty  <- as.integer(tkwinfo("rooty",txt2)))
			 Try(xTxt   <- as.integer(x)+rootx)
			 Try(yTxt   <- as.integer(y)+rooty)
			 Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt))))
			} #end of RightClick2 <- function(x,y)
			#
			Try(tkbind(txt2, "<Button-3>",RightClick2))
			#
			Try(tkpack(scr, side="right", fill="y"))
			Try(tkpack(xscr, side="bottom", fill="x"))
			Try(tkpack(txt2, side="left", fill="both", expand="yes"))
			#
			###Replace tkopen, tkread and tkclose - deprecated
			Try(chn <- tclopen(tmpEvalRcodeResults))
			Try(tkinsert(txt2, "0.0", tclvalue(tclread(chn))))
			Try(tclclose(chn))
			###
			Try(tkfocus(tt2))
			#
			SaveTextResults <- function(){
				Try(fileName<- tclvalue(tkgetSaveFile(initialfile="RcodeResults.txt",filetypes="{{Text Files} {.txt}} {{All files} *}")))
				Try(if(!nchar(fileName))return())
				#
				if(nchar(fileName)==0) return()
				Try(len <- nchar(fileName))
				if(len<=4){
					Try( fileName <- paste(fileName,".txt",sep=""))
				}else if(substring(fileName,len-3,len)!=".txt"){
					Try(fileName <- paste(fileName,".txt",sep=""))
				} #end of else/if(len<=4)
				Try(chn <- tkopen(fileName,"w"))
				Try(tkputs(chn, tclvalue(tkget(txt2,"0.0","end"))))
				Try(tkclose(chn))
			} #end of SaveTextResults <- function()
			#
			Try(topMenu2 <- tkmenu(tt2))
			Try(tkconfigure(tt2, menu=topMenu2))
			Try(fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
			Try(editMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
			#
			Try(tkadd(fileMenu2, "command", label="Save As",command=SaveTextResults)) # ) # ,font=affylmGUIfontMenu))
			Try(tkadd(fileMenu2, "command", label="Close",command=function() tkdestroy(tt2))) # ) # ,font=affylmGUIfontMenu))
			Try(tkadd(topMenu2, "cascade", label="File",menu=fileMenu2)) # ) # ,font=affylmGUIfontMenu))
			Try(tkadd(editMenu2, "command", label="Copy <Ctrl-C>",command=copyText2)) # ) # ,font=affylmGUIfontMenu))
			Try(tkadd(topMenu2, "cascade", label="Edit",menu=editMenu2)) # ) # ,font=affylmGUIfontMenu))
			#
		} #end of if(runType!="runGraphicsOnly")
		#
		Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
		Try(tkconfigure(ttEvalRcode,cursor="arrow"))
	} #end of runOverall <- function(runType)
	#
	Try(runTextOnly <- function() runOverall("runTextOnly"))
	Try(runGraphicsOnly <- function() runOverall("runGraphicsOnly"))
	Try(runTextAndGraphics <- function() runOverall("runTextAndGraphics"))
	#
	Try(HTMLhelp <- function() help.start())
	#
	Try(topMenu  <- tkmenu(ttEvalRcode ))
	Try(tkconfigure(ttEvalRcode , menu=topMenu))
	Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(runMenu  <- tkmenu(topMenu, tearoff=FALSE))
	Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
	Try(helpMenu <- tkmenu(topMenu, tearoff=FALSE))
	#
	Try(tkadd(fileMenu, "command", label="Open",                        command=OpenRSourceFile))
	Try(tkadd(fileMenu, "command", label="Save As",                     command=SaveRSourceFile))
	Try(tkadd(fileMenu, "command", label="Close",                       command=function() tkdestroy(ttEvalRcode )))
	Try(tkadd(topMenu,  "cascade", label="File",                        menu=fileMenu))
	Try(tkadd(editMenu, "command", label="Cut <Ctrl-X>",                command=cutText))
	Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",               command=copyText))
	Try(tkadd(editMenu, "command", label="Paste <Ctrl-V>",              command=pasteText))
	Try(tkadd(topMenu,  "cascade", label="Edit",                        menu=editMenu))
	Try(tkadd(runMenu,  "command", label="Show Text Results only",      command=runTextOnly))
	Try(tkadd(runMenu,  "command", label="Show Graphical Results only", command=runGraphicsOnly))
	Try(tkadd(runMenu,  "command", label="Show Text and Graphics",      command=runTextAndGraphics))
	Try(tkadd(topMenu,  "cascade", label="Run",                         menu=runMenu))
	Try(tkadd(helpMenu, "command", label="HTML Help",                   command=HTMLhelp))
	Try(tkadd(topMenu,  "cascade", label="Help",                        menu=helpMenu))
} #end of evalRcode <- function()
#
#
#
OpenCDFandTargetsfiles <- function(){
	Require("affy")
	Try(ttCDFandTargets<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttCDFandTargets))
	Try(tkgrab.set(ttCDFandTargets))
	Try(tkfocus(ttCDFandTargets))
	Try(tkwm.title(ttCDFandTargets,"Targets file"))
	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))

	OpenTargetsFileAndSetCursor <- function()
	{
			Try(tkconfigure(ttCDFandTargets,cursor="watch"))
			Try(tkfocus(ttCDFandTargets))
			Try(OpenTargetsFile())
			Try(tkconfigure(ttCDFandTargets,cursor="arrow"))
			Try(tkfocus(ttCDFandTargets))
	}

	OpenCDFFileAndSetCursor <- function()
	{
			Try(tkconfigure(ttCDFandTargets,cursor="watch"))
			Try(tkfocus(ttCDFandTargets))
			Try(OpenCDFFile())
			Try(tkconfigure(ttCDFandTargets,cursor="arrow"))
			Try(tkfocus(ttCDFandTargets))
	}

	Try(OpenCDFFile.but <- tkbutton(ttCDFandTargets, text="Select CDF File",command=OpenCDFFile,font=.affylmGUIglobals$affylmGUIfont2))
	Try(OpenTargetsFile.but <- tkbutton(ttCDFandTargets, text="Select Targets File",command=OpenTargetsFile,font=.affylmGUIglobals$affylmGUIfont2))

	Try(tclvalue(.affylmGUIglobals$CDFfileName) <- fixSeps(tclvalue(.affylmGUIglobals$CDFfileName)))
	Try(.affylmGUIglobals$CDFfileBoxTitleLabel<-tklabel(ttCDFandTargets,text=as.character(tclvalue(.affylmGUIglobals$CDFfileBoxTitle)),font=.affylmGUIglobals$affylmGUIfont2))
	Try(.affylmGUIglobals$CDFfileNameLabel<-tklabel(ttCDFandTargets,text=as.character(tclvalue(.affylmGUIglobals$CDFfileName)),background="white",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkconfigure(.affylmGUIglobals$CDFfileBoxTitleLabel, textvariable=.affylmGUIglobals$CDFfileBoxTitle))
	Try(tkconfigure(.affylmGUIglobals$CDFfileNameLabel, textvariable=.affylmGUIglobals$CDFfileName))

#	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
#	Try(tkgrid(.affylmGUIglobals$CDFfileBoxTitleLabel,columnspan=4))
#	Try(tkgrid(.affylmGUIglobals$CDFfileNameLabel,columnspan=4))

	Try(tclvalue(.affylmGUIglobals$TargetsfileName) <- fixSeps(tclvalue(.affylmGUIglobals$TargetsfileName)))
	Try(TargetsfileBoxTitleLabel <- tklabel(ttCDFandTargets,text=as.character(tclvalue(.affylmGUIglobals$TargetsfileBoxTitle)),font=.affylmGUIglobals$affylmGUIfont2))
	Try(TargetsfileNameLabel <- tklabel(ttCDFandTargets,text=as.character(tclvalue(.affylmGUIglobals$TargetsfileName)),background="white",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkconfigure(TargetsfileBoxTitleLabel, textvariable=.affylmGUIglobals$TargetsfileBoxTitle))
	Try(tkconfigure(TargetsfileNameLabel, textvariable=.affylmGUIglobals$TargetsfileName))

	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
	Try(tkgrid(TargetsfileBoxTitleLabel,columnspan=4))
	Try(tkgrid(TargetsfileNameLabel,columnspan=4))

	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))

#	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
#	Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OpenCDFFile.but, OpenTargetsFile.but))
	Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OpenTargetsFile.but))
	Try(tkgrid.configure(OpenTargetsFile.but,columnspan=2))
	Try(Abort <- 1)
	onOK <- function(){
		#			Try(cdf		 <- get("cdf",envir=affylmGUIenvironment))
		Try(Targets <- get("Targets",envir=affylmGUIenvironment))
		#			Try(if(length(cdf)==0)
		#			{
		#				Try(tkmessageBox(title="CDF (Chip Definition) File",message=paste("Either you did not specify a valid CDF (Chip Definition File",
		#					"or an error occurred while reading in the CDF file.	It should be in tab-delimited text format and it should include the column headings \"Block\", \"Column\", \"Row\", \"Name\" and \"ID\"."),icon="error"))
		#				onCancel()
		#				return()
		#			})
		Try(
			if(length(Targets)==0){
				Try(
					tkmessageBox(
						title="RNA Targets File",
						message=paste(
							"Either you did not specify a valid RNA Targets File",
							"or an error occurred while reading in the Targets file.	It should be in tab-delimited text format and it should include the column headings \"FileName\", and \"Target\".",icon="error"
						) #end of message=paste
					) #end of tkmessageBox
				) #end of Try
				onCancel()
				return()
			} #end of if(length(Targets)==0)
		) #end of Try
		Try(tkgrab.release(ttCDFandTargets));
		Try(tkdestroy(     ttCDFandTargets));
		Try(tkfocus(       .affylmGUIglobals$ttMain))
		Try(Abort <<- 0)
	} #end of onOK <- function()
	#
	#
	onCancel <- function(){
		Try(tkgrab.release(ttCDFandTargets));
		Try(tkdestroy(ttCDFandTargets));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		Try(Abort<<-1)
	} #end of onCancel <- function()
	#
	#
	Try(OK.but <-tkbutton(ttCDFandTargets,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(ttCDFandTargets,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
	Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OK.but,Cancel.but))
	Try(tkgrid(tklabel(ttCDFandTargets,text="       ")))
	#
	Try(tkfocus(ttCDFandTargets))
	Try(tkbind(ttCDFandTargets, "<Destroy>", function() {Try(tkgrab.release(ttCDFandTargets));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttCDFandTargets))
	#
	if(Abort==1){
		return(0)
	}
	#
	#OK
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	Try(Targets <- get("Targets",affylmGUIenvironment))
	Try(slides <- Targets$FileName)
	Try(filesExist <- file.exists(slides))
	Try(filesWhichDontExist <- slides[!filesExist])
	Try(
		if(length(filesWhichDontExist)>0){
			Try(
				for (i in (1:length(filesWhichDontExist))){
					Try(
						tkmessageBox(
							title="Error opening file",
							message=paste("Failed to open file: \"",filesWhichDontExist[i],"\"",sep=""),
							icon="error"
						) #end of tkmessageBox
					) #end of Try
				} #end of for (i in (1:length(filesWhichDontExist)))
			)
		} #end of if(length(filesWhichDontExist)>0)
	) #end of Try
	Try(
		if(length(filesWhichDontExist)>0){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return(0)
		} #end of if(length(filesWhichDontExist)>0)
	) #end of Try
	#
	Try(RawAffyData <- ReadAffy(filenames=Targets$FileName))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(assign("RawAffyData",RawAffyData,affylmGUIenvironment))
	Try(assign("RawAffyData.Available",TRUE,affylmGUIenvironment))
	Try(SlideNamesVec <- colnames(exprs(RawAffyData)))
	Try(if("Name" %in% colnames(Targets))SlideNamesVec <- Targets$Name)
	Try(assign("SlideNamesVec",SlideNamesVec,affylmGUIenvironment))
	Try(assign("ArraysLoaded",TRUE,affylmGUIenvironment))
	Try(tkdelete(.affylmGUIglobals$mainTree,"RawAffyData.Status"))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(ReturnVal <- GetlimmaDataSetName())
	if(ReturnVal==0) return(0)
	return(1)
} #end of OpenCDFandTargetsfiles <- function()



GetlimmaDataSetName <- function(){
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(ttGetlimmaDataSetName<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetlimmaDataSetName))
	Try(tkgrab.set(ttGetlimmaDataSetName))
	Try(tkfocus(ttGetlimmaDataSetName))
	Try(tkwm.title(ttGetlimmaDataSetName,"Data Set Name"))
	Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
	if(limmaDataSetNameText=="Untitled"){
		Try(limmaDataSetNameText <- "")
	} #end of if(limmaDataSetNameText=="Untitled")
	Try(Local.limmaDataSetName <- tclVar(init=limmaDataSetNameText))
	Try(entry.limmaDataSetName <-tkentry(ttGetlimmaDataSetName,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=Local.limmaDataSetName,bg="white"))
	Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="Please enter a name for this data set.",font=.affylmGUIglobals$affylmGUIfont2)))
	Try(tkgrid(entry.limmaDataSetName))
	#
	#
	onOK <- function(){
		Try(limmaDataSetNameText <- tclvalue(Local.limmaDataSetName))
		if(nchar(limmaDataSetNameText)==0){
			limmaDataSetNameText <- "Untitled"
		} #end of if(nchar(limmaDataSetNameText)==0)
		Try(assign("limmaDataSetNameText",limmaDataSetNameText,affylmGUIenvironment))
		Try(tclvalue(.affylmGUIglobals$limmaDataSetNameTcl) <- limmaDataSetNameText)
		Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkdestroy(ttGetlimmaDataSetName));Try(tkfocus(.affylmGUIglobals$ttMain))
	} #end of onOK <- function()
	#
	#
	Try(OK.but <-tkbutton(ttGetlimmaDataSetName,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
	Try(tkgrid(OK.but))
	Try(tkgrid.configure(OK.but))
	Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="       ")))
	#
	Try(tkfocus(entry.limmaDataSetName))
	Try(tkbind(entry.limmaDataSetName, "<Return>",onOK))
	Try(tkbind(ttGetlimmaDataSetName, "<Destroy>", function(){Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkfocus(.affylmGUIglobals$ttMain));return(0)}))
	Try(tkwait.window(ttGetlimmaDataSetName))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	return (1)
} #end of GetlimmaDataSetName <- function()
#
#
#
GetParameterizationName <- function(){
	Try(ttGetParameterizationName<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetParameterizationName))
	Try(tkgrab.set(ttGetParameterizationName))
	Try(tkwm.title(ttGetParameterizationName,"Parameterization Name"))
	Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
	Try(Local.ParameterizationName <- tclVar(init=""))
	Try(entry.ParameterizationName <-tkentry(ttGetParameterizationName,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=Local.ParameterizationName,bg="white"))
	Try(tkgrid(tklabel(ttGetParameterizationName,text="Please enter a name for this parameterization.",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2))
	Try(tkgrid(entry.ParameterizationName,columnspan=2))
	#
	ReturnVal <- "GetParameterizationName.CANCEL"
	#
	#
	onOK <- function(){
		Try(ParameterizationNameText <- tclvalue(Local.ParameterizationName))
		Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- ParameterizationNameText
	} #end of onOK <- function()
	#
	#
	onCancel <- function(){
		Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- "GetParameterizationName.CANCEL"
	} #end of onCancel <- function()
	#
	#
	Try(OK.but <-tkbutton(ttGetParameterizationName,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(ttGetParameterizationName,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
	Try(tkgrid(OK.but,Cancel.but))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttGetParameterizationName,text="       ")))
	#
	Try(tkfocus(entry.ParameterizationName))
	Try(tkbind(entry.ParameterizationName, "<Return>",onOK))
	Try(tkbind(ttGetParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetParameterizationName));Try(tkfocus(.affylmGUIglobals$ttMain));Try(return("GetParameterizationName.CANCEL"))}))
	Try(tkwait.window(ttGetParameterizationName))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(return (ReturnVal))
} #end of GetParameterizationName <- function()
#
#
#
GetContrastParameterizationName <- function(){
	Try(ttGetContrastParameterizationName<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetContrastParameterizationName))
	Try(tkgrab.set(ttGetContrastParameterizationName))
	Try(tkwm.title(ttGetContrastParameterizationName,"Contrasts Name"))
	Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="    ")))
	Try(Local.ContrastParameterizationName <- tclVar(init=""))
	Try(entry.ContrastParameterizationName <-tkentry(ttGetContrastParameterizationName,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=Local.ContrastParameterizationName,bg="white"))
	Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="Please enter a name for this set of contrasts.",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2))
	Try(tkgrid(entry.ContrastParameterizationName,columnspan=2))
	#
	ReturnVal <- "GetContrastParameterizationName.CANCEL"
	#
	#
	onOK <- function(){
		Try(ContrastParameterizationNameText <- tclvalue(Local.ContrastParameterizationName))
		Try(tkgrab.release(ttGetContrastParameterizationName));Try(tkdestroy(ttGetContrastParameterizationName));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- ContrastParameterizationNameText
	} #end of onOK <- function()
	#
	#
	onCancel <- function(){
		Try(tkgrab.release(ttGetContrastParameterizationName));
		Try(tkdestroy(ttGetContrastParameterizationName));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- "GetContrastParameterizationName.CANCEL"
	} #end of onCancel <- function()
	#
	#
	Try(OK.but <-tkbutton(ttGetContrastParameterizationName,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(ttGetContrastParameterizationName,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="    ")))
	Try(tkgrid(OK.but,Cancel.but))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="       ")))
	#
	Try(tkfocus(entry.ContrastParameterizationName))
	Try(tkbind(entry.ContrastParameterizationName, "<Return>",onOK))
	Try(tkbind(ttGetContrastParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetContrastParameterizationName));Try(tkfocus(.affylmGUIglobals$ttMain));Try(return("GetContrastParameterizationName.CANCEL"))}))
	Try(tkwait.window(ttGetContrastParameterizationName))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(return (ReturnVal))
} #end of GetContrastParameterizationName <- function()
#
#
#
NewLimmaFile <- function(){
	Try(limmaDataSetNameText                 <- get("limmaDataSetNameText",                        envir=affylmGUIenvironment))
	Try(NumParameters                        <- get("NumParameters",                               envir=affylmGUIenvironment))
	Try(NumContrastParameterizations         <- get("NumContrastParameterizations",                envir=affylmGUIenvironment))
	Try(ContrastParameterizationList         <- get("ContrastParameterizationList",                envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",        envir=affylmGUIenvironment))
	Try(LimmaFileName                        <- get("LimmaFileName",envir=affylmGUIenvironment))
	if(limmaDataSetNameText!="Untitled"){
		Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled"){LimmaFileName <- limmaDataSetNameText} )	# Local assignment only
		Try(
			mbVal <- tkmessageBox(
				title="Start New Analysis",
				message=paste("Save changes to ",fixSeps(LimmaFileName),"?",sep=""),
				icon="question",
				type="yesnocancel",
				default="yes"
			) #end of mbval <- tkmessageBox
		) #end of Try(mbVal..
		if(tclvalue(mbVal)=="yes"){Try(SaveLimmaFile())}
		if(tclvalue(mbVal)=="cancel"){
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}#end of if(tclvalue(mbVal)=="cancel")
		Try(limmaDataSetNameText <- "Untitled")
	}#end of if(limmaDataSetNameText!="Untitled")
	#Try(tkmessageBox(title="Working Directory",message="After clicking OK, please select a working directory.",type="ok"))
	Try(WD <- SetWD())
	if(WD=="") return()
	Try(tkdelete(.affylmGUIglobals$mainTree,"RawAffyData.Status"))
	Try(tkdelete(.affylmGUIglobals$mainTree,"NormalizedAffyData.Status"))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(tkdelete(.affylmGUIglobals$mainTree,"LinearModelFit.Status"))
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","LinearModelFit","LinearModelFit.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	Try(
		if(NumContrastParameterizations>0){
			Try(for (i in (1:NumContrastParameterizations))
				Try(tkdelete(.affylmGUIglobals$mainTree,paste("ContrastParameterizations.Status.",i,sep="")))
			)
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"ContrastParameterizations.Status.1"))
		}
	)
	Try(
		tkinsert(.affylmGUIglobals$mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree)
	)#end of Try
	Try(
		if(NumParameters>0){
			Try(for (i in (1:NumParameters))
				Try(tkdelete(.affylmGUIglobals$mainTree,paste("Parameters.Status.",i,sep="")))
			)#end of Try-for..
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"Parameters.Status.1"))
		}
	)#end of Try-if...
	Try(tkinsert(.affylmGUIglobals$mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
	if(NumContrastParameterizations>0){
		for (contrastParameterizationIndex in (1:NumContrastParameterizations)){
			Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
			Try(ParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
			Try(tkdelete(.affylmGUIglobals$ContrastParameterizationTREE,ParameterizationNameNode))
			Try(assign("ContrastParameterizationList", deleteItemFromList(ContrastParameterizationList,ParameterizationNameNode),affylmGUIenvironment))
		}#end of for
	}#end of if
	Try(initGlobals())
	Try(LimmaFileName <- get("LimmaFileName",affylmGUIenvironment))
	Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
	Try(
		if(.Platform$OS.type=="windows"){
			Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",gsub("/","\\\\",LimmaFileName))))
		}else{
			Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",LimmaFileName)))
		} #end of else/if(.Platform$OS.type=="windows")
	)#end of Try
	Try(tclvalue(.affylmGUIglobals$CDFfileBoxTitle)     <- "Please select a Chip Definition (CDF) file.")
	Try(tclvalue(.affylmGUIglobals$CDFfileName)         <- "No filename is selected at the moment.	Press the Select CDF File Button.")
	Try(tclvalue(.affylmGUIglobals$TargetsfileBoxTitle) <- "Please select a tab-delimited file listing the CEL files.")
	Try(tclvalue(.affylmGUIglobals$TargetsfileName)     <- "No filename is selected at the moment.	Press the Select Targets File Button.")
	Try(OpenCDFandTargetsfiles())
	Try(tkfocus(.affylmGUIglobals$ttMain))
}#end of NewLimmaFile <- function()
#
#
#
chooseDir <- function(){
	Try(wd <- tclVar(getwd()))
	Try(
		if(.Platform$OS.type=="windows"){
			Try(tclvalue(wd) <- gsub("/","\\\\",tclvalue(wd)))
		}
	)
	Try(ttChooseDir <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.title(ttChooseDir,"Choose working directory"))
	Try(tkwm.deiconify(ttChooseDir))
	Try(tkgrab.set(ttChooseDir))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(label1 <- tklabel(ttChooseDir,text="Choose working directory (containing input files):",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),label1,sticky="w"))
	Try(tkgrid.configure(label1,columnspan=3))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	#
	#
	Try(
		onBrowse <- function(){
			Try(if(file.exists(gsub("\\\\","/",tclvalue(wd)))) initialdir<-gsub("\\\\","/",tclvalue(wd)) else initialdir<-getwd())
			Try(dir1 <- tclvalue(tkchooseDirectory(title="Please choose a working directory for the Limma Analysis",initialdir=initialdir)))
			Try(if(nchar(dir1)>0) tclvalue(wd) <- dir1)
			Try(
				if(.Platform$OS.type=="windows"){
					Try(tclvalue(wd) <- gsub("/","\\\\",tclvalue(wd)))
				}
			)
		} #end of onBrowse <- function()
	)# end of Try
	#
	#
	Try(ReturnVal <- "")
	#
	#
	Try(
		onOK <- function(){
			Try(DirChosen <- tclvalue(wd))
			Try(tkgrab.release(ttChooseDir))
			Try(tkdestroy(ttChooseDir))
			Try(DirChosen <- gsub("\\\\","/",DirChosen))
			Try(ReturnVal <<- DirChosen)
		} #end of onOK <- function()
	)
	#
	#
	Try(
		onCancel <- function(){
			Try(tkgrab.release(ttChooseDir));
			Try(tkdestroy(ttChooseDir))
		} #end of onCancel <- function()
	) #end of try
	#
	#
	Try(Browse.but <- tkbutton(ttChooseDir,text="Browse",command=onBrowse,font=.affylmGUIglobals$affylmGUIfont2))
	Try(OK.but <- tkbutton(ttChooseDir,text="    OK		",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttChooseDir,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(entry1 <- tkentry(ttChooseDir,textvariable=wd,width=40,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttChooseDir,text="    "),entry1))
	Try(tkgrid.configure(entry1,columnspan=3))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=4))
	Try(tkgrid(Browse.but,row=3,column=5))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=6))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),tklabel(ttChooseDir,text="    "),OK.but,Cancel.but))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	#
	Try(tkfocus(entry1))
	Try(tkbind(ttChooseDir,"<Destroy>",function()tkgrab.release(ttChooseDir)))
	Try(tkbind(entry1,"<Return>",onOK))
	Try(tkwait.window(ttChooseDir))
	#
	return(ReturnVal)
}#end of chooseDir <- function()
#
#
#
SetWD <- function(){
	WD <- chooseDir()
	if(!nchar(WD)){
		tkfocus(.affylmGUIglobals$ttMain)
		return("")
	}
	Try(setwd(WD))
	tkfocus(.affylmGUIglobals$ttMain)
	return(WD)
}#end of SetWD <- function()
#
#
#
onExit <- function(){
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
	if(limmaDataSetNameText!="Untitled"){
		Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")	LimmaFileName <- limmaDataSetNameText)	# Local assignment only
		Try(
			mbVal <- tkmessageBox(
				title="Exit affylmGUI",
				message=paste("Save changes to ",fixSeps(LimmaFileName),"?",sep=""),
				icon="question",
				type="yesnocancel",
				default="yes"
			) #end of mbVal <- tkmessageBox
		) #end of Try
		if(tclvalue(mbVal)=="yes"){
			Try(SaveLimmaFile())
		}
		if(tclvalue(mbVal)=="cancel"){
			return()
		}
	} #end of if(limmaDataSetNameText!="Untitled")
	#
	Try(assign(".JustAskedWhetherToSave",TRUE,.GlobalEnv))
	#	try(tkdestroy(.affylmGUIglobals$ttMain),silent=TRUE)
	try(tkdestroy(.affylmGUIglobals$ttMain),silent=TRUE)
} #end of onExit <- function()
#
#
#
ChooseContrastParameterization <- function(){
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(
		if(NumContrastParameterizations==0){
			Try(tkmessageBox(title="Choose Contrasts Parameterization",message="There are no contrasts parameterizations available.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		} #end of if(NumContrastParameterizations==0)
	)
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
	#
	ttChooseContrastParameterization<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttChooseContrastParameterization)
	tkgrab.set(ttChooseContrastParameterization)
	tkfocus(ttChooseContrastParameterization)
	tkwm.title(ttChooseContrastParameterization,"Choose a Contrasts Parameterization")
	scr <- tkscrollbar(
		ttChooseContrastParameterization,
		repeatinterval=5,
		command=function(...)tkyview(tl,...)
	)
	## Safest to make sure scr exists before setting yscrollcommand
	tl<-tklistbox(
		ttChooseContrastParameterization,
		height=4,
		selectmode="single",
		yscrollcommand=function(...)tkset(scr,...),
		background="white",
		font=.affylmGUIglobals$affylmGUIfont2
	)
	lbl2<-tklabel(
		ttChooseContrastParameterization,
		text="Which contrasts parameterization is this for?",
		font=.affylmGUIglobals$affylmGUIfont2
	)
	tkgrid(tklabel(ttChooseContrastParameterization,text="       "),row=0,column=1,columnspan=1)
	tkgrid(tklabel(ttChooseContrastParameterization,text="       "),row=0,column=4,columnspan=1)
	tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
	tkgrid.configure(lbl2,sticky="w")
	tkgrid(tklabel(ttChooseContrastParameterization,text="         "),row=2,column=1)
	tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
	tkgrid(scr,row=2,column=3,columnspan=1,rowspan=4,sticky="wns")
	#
	if(NumContrastParameterizations>0){
		for (i in (1:NumContrastParameterizations)){
			tkinsert(tl,"end",ContrastParameterizationNamesVec[i])
		}
	} #end of if(NumContrastParameterizations>0)
	tkselection.set(tl,0)
	ReturnVal <- 0
	#
	#
	onOK <- function(){
		Try(contrastParameterizationIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
		Try(tkgrab.release(ttChooseContrastParameterization));Try(tkdestroy(ttChooseContrastParameterization));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- contrastParameterizationIndex
	} #end of onOK <- function()
	#
	#
	onCancel <- function() {
		Try(tkgrab.release(ttChooseContrastParameterization));
		Try(tkdestroy(ttChooseContrastParameterization));
		Try(tkfocus(.affylmGUIglobals$ttMain));
		ReturnVal <<- 0
	} #end of onCancel <- function()
	#
	#
	OK.but <-tkbutton(ttChooseContrastParameterization,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttChooseContrastParameterization,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	#
	tkgrid(tklabel(ttChooseContrastParameterization,text="    "))
	tkgrid(tklabel(ttChooseContrastParameterization,text="    "),tklabel(ttChooseContrastParameterization,text="    "),OK.but,Cancel.but)
	tkgrid.configure(OK.but,		sticky="e")
	tkgrid.configure(Cancel.but,sticky="w")
	tkgrid(tklabel(ttChooseContrastParameterization,text="    "))
	#
	Try(tkfocus(ttChooseContrastParameterization))
	Try(tkbind(ttChooseContrastParameterization, "<Destroy>", function() {Try(tkgrab.release(ttChooseContrastParameterization));Try(tkfocus(.affylmGUIglobals$ttMain))}))
	Try(tkwait.window(ttChooseContrastParameterization))
	#
	return (ReturnVal)
} #end of ChooseContrastParameterization <- function()
#
#
#
DeleteContrastParameterization <- function(){
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	#
	Try(
		if(NumContrastParameterizations==0){
			Try(
				tkmessageBox(
					title="Delete Contrasts Parameterization",
					message="There are no contrast parameterizations loaded.	Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",
					type="ok",
					icon="error"
				) #end of tkmessageBox
			) #end of Try
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		} #end of if(NumContrastParameterizations==0)
	) #end of Try
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	Try(if(contrastParameterizationIndex==0)		return())
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	Try(ContrastParameterizationNameNode<- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	Try(tkdelete(.affylmGUIglobals$ContrastParameterizationTREE,ContrastParameterizationNameNode))
	Try(
		ContrastParameterizationList <- deleteItemFromList(
			ContrastParameterizationList,
			ContrastParameterizationNameNode
		)
	) #end of Try
	Try(tempVec <- c())
	#
	Try(
		if(NumContrastParameterizations>0){
			Try(
				for (i in (1:NumContrastParameterizations)){
					Try(
						if(i!=.affylmGUIglobals$ContrastParameterizationTREEIndex){
							Try(tempVec <- c(tempVec,ContrastParameterizationTREEIndexVec[i]))
						}
					)# end of Try
				} #end of Try
			) #end of Try
		} #end of if(NumContrastParameterizations>0)
	)
	Try(ContrastParameterizationTREEIndexVec <- tempVec)
	#
	Try(tempVec <- c())
	Try(
		if(NumContrastParameterizations>0){
			Try(
				for (i in (1:NumContrastParameterizations)){
					Try(
						if(i!=.affylmGUIglobals$ContrastParameterizationTREEIndex){
							Try(tempVec <- c(tempVec,ContrastParameterizationNamesVec[i]))
						} #end of if(i!=.affylmGUIglobals$ContrastParameterizationTREEIndex)
					) #end of Try
				} #end of for (i in (1:NumContrastParameterizations))
			) #end of Try
		} #end of if(NumContrastParameterizations>0)
	)# end of Try
	Try(ContrastParameterizationNamesVec <- tempVec)
	#
	Try(NumContrastParameterizations <- NumContrastParameterizations - 1)
	Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)
	#	Try(if(ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations==0)
	#	Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"0","root",paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,".1",sep=""),text="none",font=.affylmGUIglobals$affylmGUIfontTree)))
	#
	Try(
		if(NumContrastParameterizations>0){
			Try(
				for (i in (1:NumContrastParameterizations)){
					Try(tkdelete(.affylmGUIglobals$mainTree,paste("ContrastParameterizations.Status.",i,sep="")))
				} #end of for (i in (1:NumContrastParameterizations))
			)
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"ContrastParameterizations.Status.1"))
		} #end of else/if(NumContrastParameterizations>0)
	) #end of Try
	#
	Try(
		if(NumContrastParameterizations>0){
			for (contrastParameterizationIndex in (1:NumContrastParameterizations)){
				Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
				#Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(
					tkinsert(
						.affylmGUIglobals$mainTree,
						"end",
						"ContrastParameterizations",
						ContrastParameterizationsStatusNameNode ,
						text=ContrastParameterizationNamesVec[contrastParameterizationIndex],
						font=.affylmGUIglobals$affylmGUIfontTree
					) #end of tkinsert
				) #end of Try
				#Try(contrastsMatrix <- ContrastParameterizationList[[1]]$contrastsMatrixInList$contrasts)
				#Try(ContrastsNames <- colnames(contrastsMatrix))
				#Try(ContrastParameterizationNameText <- ContrastParameterizationList[[1]]$ContrastParameterizationNameText)
				#Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=.affylmGUIglobals$affylmGUIfontTree))
				#Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
				#Try(for (j in (1:NumContrastsInContrastParameterization))
				#Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,paste("Contrasts.",contrastParameterizationIndex,".",.affylmGUIglobals$ContrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=.affylmGUIglobals$affylmGUIfontTree)))
				#
			} #end of for (contrastParameterizationIndex in (1:NumContrastParameterizations))
		}else{
			Try(
				tkinsert(
					.affylmGUIglobals$mainTree,
					"end",
					"ContrastParameterizations",
					"ContrastParameterizations.Status.1" ,
					text="None",
					font=.affylmGUIglobals$affylmGUIfontTree
				) #end of tkinsert
			) #end of Try
		} #end of else/if(NumContrastParameterizations>0)
	) #end of Try
	Try(assign("ContrastParameterizationList",ContrastParameterizationList,affylmGUIenvironment))
	Try(assign("ContrastParameterizationTREEIndexVec",ContrastParameterizationTREEIndexVec,affylmGUIenvironment))
	Try(assign("NumContrastParameterizations",NumContrastParameterizations,affylmGUIenvironment))
	Try(assign("ContrastParameterizationNamesVec",ContrastParameterizationNamesVec,affylmGUIenvironment))
} #end of DeleteContrastParameterization <- function()
#
#
#
OpenLimmaFile <- function() OpenALimmaFile()
#
#
#
OpenALimmaFile <- function(FileName){
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
	Try(
		if(missing(FileName)){
			Try(tempLimmaFileName <- tclvalue(tkgetOpenFile(filetypes="{{Limma Files} {.lma}} {{All files} *}")))
			if(!nchar(tempLimmaFileName)){
				tkfocus(.affylmGUIglobals$ttMain)
				return()
			}#end of if(!nchar(tempLimmaFileName))
		}else{
			tempLimmaFileName <- FileName
		}#end of else/if(missing(FileName))
	)
	#
	Try(
		if(!nchar(tempLimmaFileName)){
			tkfocus(.affylmGUIglobals$ttMain)
			return()
		}#end of if(!nchar(tempLimmaFileName))
	)#end of Try
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(
		if(limmaDataSetNameText!="Untitled"){
			Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")	LimmaFileName <- limmaDataSetNameText)	# Local assignment only
			Try(
				mbVal <- tkmessageBox(
					title="Open File",
					message=paste("Save changes to ",fixSeps(LimmaFileName),"?",sep=""),
					icon="question",
					type="yesnocancel",
					default="yes"
				)#end of mbVal <- tkmessageBox
			)#end of Try
			Try(if(tclvalue(mbVal)=="yes")SaveLimmaFile())
			Try(if(tclvalue(mbVal)=="cancel")return())
		}#end of if(limmaDataSetNameText!="Untitled")
	)#end of Try
	Try(LimmaFileName <- tempLimmaFileName)
	Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
	#
	Try(recentFilesFileName <- system.file("etc/recent-files.txt",package="affylmGUI"))
	Try(recentFiles <- readLines(recentFilesFileName))
	Try(recentFiles <- gsub("\\\\","/",recentFiles))
	# Remove any blank lines:
	Try(blanks <- grep("^[ \t\n]*$",recentFiles))
	Try(if(length(blanks)>0)recentFiles <- recentFiles[-blanks])
	Try(numRecentFiles <- length(recentFiles))
	#
	Try(if(length(grep(LimmaFileName,recentFiles))==0)recentFiles <- c(LimmaFileName,recentFiles))
	Try(if(length(recentFiles)>4)recentFiles <- recentFiles[1:4])
	try(writeLines(con=recentFilesFileName,recentFiles),TRUE)
	Try(numRecentFiles <- length(recentFiles))
	#
	Try(
		if(numRecentFiles>0){
			Try(fileMenu <- .affylmGUIglobals$menus$fileMenu)
			Try(workingDirIndex <- as.numeric(tclvalue(tkindex(.affylmGUIglobals$menus$fileMenu,"Working Directory"))))
			Try(exitIndex <- as.numeric(tclvalue(tkindex(.affylmGUIglobals$menus$fileMenu,"Exit"))))
			Try(
				if(exitIndex==workingDirIndex+2){
					Try(numRecentFilesInMenu <- 0)
				}else{
					Try(numRecentFilesInMenu <- exitIndex - workingDirIndex - 3)
					Try(
						for (i in (1:(numRecentFilesInMenu+1))){
							Try(tkdelete(fileMenu,workingDirIndex+2))
						} #end of for (i in (1:(numRecentFilesInMenu+1)))
					) #end of Try
				} #end of else/if(exitIndex==workingDirIndex+2)
			) #end of Try
			Try(tkinsert(fileMenu,workingDirIndex+1,"separator"))
			#
			Try(
				for (i in (numRecentFiles:1)){
					Try(label <- recentFiles[i])
					Try(fileNameOnly <- strsplit(label,"/")[[1]])
					Try(fileNameOnly <- fileNameOnly[length(fileNameOnly)])
					Try(
						if(nchar(recentFiles[i])>60){
							label <- paste(".../",fileNameOnly)
						}
					)
					Try(eval(parse(text=paste("assign(\".OpenALimmaFile_",i,"\",function() OpenALimmaFile(\"",recentFiles[i],"\"),.GlobalEnv)",sep=""))))
					Try(
						if(.Platform$OS.type=="windows"){
							Try(
								tkinsert(
									fileMenu,
									workingDirIndex+2,
									"command",
									label=paste(i,". ",gsub("/","\\\\",label),sep=""),
									command=eval(parse(text=paste(".OpenALimmaFile_",i,sep="")))
								) #end of tkinsert
							) #end of Try
						}else{
							Try(
								tkinsert(
									fileMenu,
									workingDirIndex+2,
									"command",
									label=paste(i,". ",label,sep=""),
									command=eval(parse(text=paste(".OpenALimmaFile_",i,sep="")))
								) #end of tkinsert
							) #end of Try
						} #end of if(.Platform$OS.type=="windows")
					)#end of Try
				} #end of for (i in (numRecentFiles:1))
			) #end of Try
		} #end of if(numRecentFiles>0)
	) #end of Try
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	#
	# Using existing NumContrastParameterizations, NOT the one loaded from the .lma file.
	# (We haven't loaded it yet.)
	Try(OldNumParameters <- NumParameters)
	#
	Try(if(NumContrastParameterizations>0)
		Try(for (contrastParameterizationIndex in (1:NumContrastParameterizations))
		{
			Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
			Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
			Try(tkdelete(.affylmGUIglobals$ContrastParameterizationTREE,ContrastParameterizationNameNode))
			Try(assign("ContrastParameterizationList",deleteItemFromList(ContrastParameterizationList,ContrastParameterizationNameNode),affylmGUIenvironment))
		}))

	# Load the RData File whose name is "LimmaFileName"
	Try(load(LimmaFileName,envir=affylmGUIenvironment))
	#
	# The user may have changed the filename in the operating system since the last save.
	Try(LimmaFileName <- tempLimmaFileName)
	Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
	#
	Try(limmaDataSetNameText <- get("limmaDataSetNameText" , envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec", envir=affylmGUIenvironment))
	Try(NumContrastParameterizations <- get("NumContrastParameterizations", envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(NumParameters <- get("NumParameters" , envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))

	Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
	Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
	Try(if(.Platform$OS.type=="windows")
		Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",gsub("/","\\\\",LimmaFileName))))
	else
		Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",LimmaFileName))))
	Try(assign("limmaDataSetNameText",limmaDataSetNameText,affylmGUIenvironment))
	Try(tclvalue(.affylmGUIglobals$limmaDataSetNameTcl) <- limmaDataSetNameText)
	#
	Try(tkdelete(.affylmGUIglobals$mainTree,"RawAffyData.Status"))
	Try(tkdelete(.affylmGUIglobals$mainTree,"NormalizedAffyData.Status"))
	Try(tkdelete(.affylmGUIglobals$mainTree,"LinearModelFit.Status"))
	Try(
		if(OldNumParameters>0){
			Try(for (i in (1:OldNumParameters))
				Try(tkdelete(.affylmGUIglobals$mainTree,paste("Parameters.Status.",i,sep="")))
			)
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"Parameters.Status.1"))
		}
	)#end of Try
	Try(
		if(NumContrastParameterizations>0){
			Try(
				for (i in (1:NumContrastParameterizations)){
					Try(tkdelete(.affylmGUIglobals$mainTree,paste("ContrastParameterizations.Status.",i,sep="")))
				}
			)#end of Try
		}else{
			Try(tkdelete(.affylmGUIglobals$mainTree,"ContrastParameterizations.Status.1"))
		}
	)
	#
	Try(RawAffyData.Available						<- get("RawAffyData.Available" , envir=affylmGUIenvironment))
	Try(NormalizedAffyData.Available		 <- get("NormalizedAffyData.Available" , envir=affylmGUIenvironment))
	Try(LinearModelFit.Available				 <- get("LinearModelFit.Available" , envir=affylmGUIenvironment))
	Try(
		if( exists("NormalizedAffyData.exprs",envir=affylmGUIenvironment) ){
			Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
		}else{
			Try(NormalizedAffyData.exprs <- NULL) #if not there, then set to NULL.
		} #end of if(exists("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
	)#end of Try
	Try(
		if( exists("NormalizedAffyData.se.exprs",envir=affylmGUIenvironment) ){
			Try(NormalizedAffyData.se.exprs <- get("NormalizedAffyData.se.exprs",envir=affylmGUIenvironment))
		}else{#if not there, then set to NULL.
			Try(NormalizedAffyData.se.exprs <- NULL)
		}#end of if(exists("NormalizedAffyData.se.exprs",envir=affylmGUIenvironment))
	)#end of Try
	#
	#Check if NormalizedAffyData exists in environment. If it does, check if it is class(ExpressionSet). If it is,
	#then if exprs and se.exprs are > 1 long, get exprs and se.exprs from the ExpressionSet object
	#and store them back in the environment as NormalizedAffyData.exprs and NormalizedAffyData.se.exprs.
	#then NULL the ExpressionSet object and store it in the environment
	Try(
		if(exists("NormalizedAffyData",envir=affylmGUIenvironment)){
			Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
		}else{
			Try(NormalizedAffyData <- NULL)#if not there, then set to NULL.
		}#end of if(exists("NormalizedAffyData",envir=affylmGUIenvironment))
	)#end of Try
	Try(
		#Now if length(NormalizedAffyData) is >0, then get the exprs and, if available, se.exprs matrices
		if(length(NormalizedAffyData) > 0){ #if NormalizedAffyData was NULL, then its length would be zero
			if(is(NormalizedAffyData, "ExpressionSet")){
				#if exprs(NormalizedAffyData) data exists and NormalizedAffyData.exprs doesn't exist, then create NormalizedAffyData.exprs
				if( (length(exprs(NormalizedAffyData)) > 1 && length(NormalizedAffyData.exprs)<1) ){
					NormalizedAffyData.exprs <- exprs(NormalizedAffyData)
				}
				#if NormalizedAffyData@se.exprs data exists and NormalizedAffyData.se.exprs doesn't exist, then create NormalizedAffyData.se.exprs
				if( (length(assayDataElement(NormalizedAffyData,"se.exprs")) > 1 && length(NormalizedAffyData.se.exprs)<1) ){
					NormalizedAffyData.se.exprs <- assayDataElement(NormalizedAffyData,"se.exprs")
				}
			}#end of if(is(NormalizedAffyData, "ExpressionSet"))
		}#end of if(length(NormalizedAffyData) != 0)
	)#end of Try
	#Now assign correct values to environment
	Try(assign("NormalizedAffyData",         NormalizedAffyData,         affylmGUIenvironment))
	Try(assign("NormalizedAffyData.exprs",   NormalizedAffyData.exprs,   affylmGUIenvironment))
	Try(assign("NormalizedAffyData.se.exprs",NormalizedAffyData.se.exprs,affylmGUIenvironment))
	#
	Try(
		if(RawAffyData.Available){
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
		}else{
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
		} #end of else/if(RawAffyData.Available)
	) #end of try
	#
	Try(
		if(exists("NormMethod",envir=affylmGUIenvironment))
			Try(NormMethod <- get("NormMethod",envir=affylmGUIenvironment))
		else
			Try(NormMethod <- "RMA")
	)
	Try(
		if(NormalizedAffyData.Available)
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text=paste("Available (",NormMethod,")",sep=""),font=.affylmGUIglobals$affylmGUIfontTree))
		else
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	)
	Try(
		if(LinearModelFit.Available)
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
		else
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
	)
	Try(
		if(LinearModelFit.Available){
			Try(design <- get("design",affylmGUIenvironment))
			Try(
				if(NumParameters>0){
					for (i in (1:NumParameters)){
						Try(
							tkinsert(
								.affylmGUIglobals$mainTree,
								"end",
								"Parameters",
								paste("Parameters.Status.",i,sep=""),
								text=colnames(design)[i],
								font=.affylmGUIglobals$affylmGUIfontTree
							) #end of tkinsert
						) #end of Try
					} #end of for (i in (1:NumParameters))
				}else{
					Try(tkinsert(.affylmGUIglobals$mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
				} #end of else/if(NumParameters>0)
			) #end of try
		}else{
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=.affylmGUIglobals$affylmGUIfontTree))
		} #end of else/if(LinearModelFit.Available)
	) #end of Try
	#
	Try(
		if(NumContrastParameterizations>0){
			for (contrastParameterizationIndex in (1:NumContrastParameterizations)){
				Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
				Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
				Try(tkinsert(.affylmGUIglobals$mainTree,"end","ContrastParameterizations",ContrastParameterizationsStatusNameNode ,text=ContrastParameterizationNamesVec[contrastParameterizationIndex],font=.affylmGUIglobals$affylmGUIfontTree))
				Try(contrastsMatrix <- ContrastParameterizationList[[1]]$contrastsMatrixInList$contrasts)
				Try(ContrastsNames <- colnames(contrastsMatrix))
				Try(ContrastParameterizationNameText <- ContrastParameterizationList[[1]]$ContrastParameterizationNameText)
				Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=.affylmGUIglobals$affylmGUIfontTree))
				Try(ContrastsNode <- paste("ContrastsNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
				#
				Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,ContrastsNode,text="Contrasts",font=.affylmGUIglobals$affylmGUIfontTree))
				#
				Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
				Try(
					for (j in (1:NumContrastsInContrastParameterization)){
						Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastsNode,paste("Contrasts.",.affylmGUIglobals$ContrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=.affylmGUIglobals$affylmGUIfontTree))
					} #end of for (j in (1:NumContrastsInContrastParameterization))
				)#end of Try
				#
				Try(LinearModelFitNode			 <- paste("LinearModelFitNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
				Try(LinearModelFitStatusNode <- paste("LinearModelFitStatusNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
				Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,LinearModelFitNode,text="Linear Model Fit",font=.affylmGUIglobals$affylmGUIfontTree))
				Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",LinearModelFitNode,LinearModelFitStatusNode,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
				#
				Try(
					if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0){
						Try(ebayesAvailable <- TRUE)
					}else{
						Try(ebayesAvailable <- FALSE)
					} #end of else/if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0)
				) #end of Try
				#
				Try(EmpiricalBayesNode			 <- paste("EmpiricalBayesNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
				Try(EmpiricalBayesStatusNode <- paste("EmpiricalBayesStatusNode.",.affylmGUIglobals$ContrastParameterizationTREEIndex))
				Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,EmpiricalBayesNode,text="Empirical Bayes Statistics",font=.affylmGUIglobals$affylmGUIfontTree))
				Try(
					if(ebayesAvailable==TRUE){
						Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Available",font=.affylmGUIglobals$affylmGUIfontTree))
					}else{
						Try(tkinsert(.affylmGUIglobals$ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Not Available",font=.affylmGUIglobals$affylmGUIfontTree))
					} #end of if(ebayesAvailable==TRUE)
				)
			} #end of for (contrastParameterizationIndex in (1:NumContrastParameterizations))
		}else{
			Try(
				tkinsert(
					.affylmGUIglobals$mainTree,
					"end",
					"ContrastParameterizations",
					"ContrastParameterizations.Status.1" ,
					text="None",
					font=.affylmGUIglobals$affylmGUIfontTree
				) #end of tkinsert
			)
		} #end of else/if(NumContrastParameterizations>0)
	) #end of Try
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
} #end of OpenALimmaFile <- function(FileName)
#
#
#
SaveLimmaFile <- function(){
	LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment)
	if(LimmaFileName=="Untitled"){
		SaveAsLimmaFile()
		try(tkfocus(.affylmGUIglobals$ttMain),silent=TRUE)
		return()
	} #end of if(LimmaFileName=="Untitled")
	# Don't give an error if main window has been destroyed.
	try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"),silent=TRUE)
	#	tkmessageBox(message="About to save Limma File! (0)")
	assign("PsetData.Available",FALSE,affylmGUIenvironment)#Always assign this to FALSE. Will always recalculate it in the Plot menu functions
	Try(save(list = ls(envir=affylmGUIenvironment), file=LimmaFileName, envir=affylmGUIenvironment))
	#	tkmessageBox(message="Limma File Saved! (1)")
	try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"),silent=TRUE)
	try(tkfocus(.affylmGUIglobals$ttMain),silent=TRUE)
	#	tkmessageBox(message="Limma File Saved! (2)")
} #end of SaveLimmaFile <- function()
#
#
#
SaveAsLimmaFile <- function(){
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
	Try(if(LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
	Try(tempLimmaFileName <- tclvalue(tkgetSaveFile(initialfile=LimmaFileName,filetypes="{{Limma Files} {.lma}} {{All files} *}")))
	if(!nchar(tempLimmaFileName)){
		try(tkfocus(.affylmGUIglobals$ttMain),silent=TRUE)
		return()
	} #end of if(!nchar(tempLimmaFileName))
	len <- nchar(tempLimmaFileName)
	if(len<=4){
		tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
	}else if(substring(tempLimmaFileName,len-3,len)!=".lma"){
		tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
	}
	Try(LimmaFileName <- tempLimmaFileName)
	Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
	#
	Try(recentFilesFileName <- system.file("etc/recent-files.txt",package="affylmGUI"))
	Try(recentFiles <- readLines(recentFilesFileName))
	#
	Try(recentFiles <- gsub("\\\\","/",recentFiles))
	#
	# Remove any blank lines:
	Try(blanks <- grep("^[ \t\n]*$",recentFiles))
	Try(if(length(blanks)>0){recentFiles <- recentFiles[-blanks]})
	Try(numRecentFiles <- length(recentFiles))
	Try(if(length(grep(LimmaFileName,recentFiles))==0){recentFiles <- c(LimmaFileName,recentFiles)})
	Try(if(length(recentFiles)>4){recentFiles <- recentFiles[1:4]})
	try(writeLines(con=recentFilesFileName,recentFiles),TRUE)
	Try(numRecentFiles <- length(recentFiles))
	#
	Try(
		if(numRecentFiles>0){
			Try(fileMenu <- .affylmGUIglobals$menus$fileMenu)
			Try(workingDirIndex <- as.numeric(tclvalue(tkindex(.affylmGUIglobals$menus$fileMenu,"Working Directory"))))
			Try(exitIndex <- as.numeric(tclvalue(tkindex(.affylmGUIglobals$menus$fileMenu,"Exit"))))
			Try(
				if(exitIndex==workingDirIndex+2){
					Try(numRecentFilesInMenu <- 0)
				}else{
					Try(numRecentFilesInMenu <- exitIndex - workingDirIndex - 3)
					Try(
						for (i in (1:(numRecentFilesInMenu+1))){
							Try(tkdelete(fileMenu,workingDirIndex+2))
						}
					) #end of Try
				} #end of if(exitIndex==workingDirIndex+2)
			)#end of Try
			Try(tkinsert(fileMenu,workingDirIndex+1,"separator"))
			#
			Try(
				for (i in (numRecentFiles:1)){
					Try(label        <- recentFiles[i])
					Try(fileNameOnly <- strsplit(label,"/")[[1]])
					Try(fileNameOnly <- fileNameOnly[length(fileNameOnly)])
					Try(
						if(nchar(recentFiles[i])>60){
							label <- paste(".../",fileNameOnly)
						}
					)
					Try(eval(parse(text=paste("assign(\".OpenALimmaFile_",i,"\",function() OpenALimmaFile(\"",recentFiles[i],"\"),.GlobalEnv)",sep=""))))
					Try(
						if(.Platform$OS.type=="windows"){
							Try(
								tkinsert(
									fileMenu,
									workingDirIndex+2,
									"command",
									label=paste(i,". ",gsub("/","\\\\",label),sep=""),
									command=eval(parse(text=paste(".OpenALimmaFile_",i,sep="")))
								) #end of tkinsert
							) #end of Try
						}else{
							Try(
								tkinsert(
									fileMenu,workingDirIndex+2,"command",label=paste(i,". ",label,sep=""),
								command=eval(parse(text=paste(".OpenALimmaFile_",i,sep=""))))
							)
						} #end of else/if(.Platform$OS.type=="windows")
					)

				} #end of for (i in (numRecentFiles:1))
			)
		} #end of if(numRecentFiles>0)
	) #end of Try

	# .affylmGUIglobals$ttMain may have been destroyed
	e <- try(tkfocus(.affylmGUIglobals$ttMain),silent=TRUE)
	if(!inherits(e, "try-error"))
		Try(if(.Platform$OS.type=="windows")
			Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",gsub("/","\\\\",LimmaFileName))))
		else
			Try(tkwm.title(.affylmGUIglobals$ttMain,paste("affylmGUI -",LimmaFileName))))

	try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"),silent=TRUE)
	assign("PsetData.Available",FALSE,affylmGUIenvironment)#Always assign this to FALSE. Will always recalculate it in the Plot menu functions
	Try(save(list = ls(envir=affylmGUIenvironment), file=LimmaFileName, envir=affylmGUIenvironment))
	try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"),silent=TRUE)
	try(tkfocus(.affylmGUIglobals$ttMain),silent=TRUE)
} #end of SaveAsLimmaFile <- function()
#
#
#
AboutaffylmGUI <- function()
{
		Try(tkmessageBox(title="About affylmGUI",message=paste("This is affylmGUI Version ",getPackageVersion("affylmGUI"),
															", using limma Version ",getPackageVersion("limma"),".	The limma package was developed by Gordon Smyth and the Graphical User Interface (GUI) was developed by James Wettenhall.",sep=""),type="ok",icon="info"))
}


ChooseCDF <- function()
{
	ttChooseCDF<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttChooseCDF)
	tkgrab.set(ttChooseCDF)
	tkfocus(ttChooseCDF)
	tkwm.title(ttChooseCDF,"Choose a CDF")
	scr <- tkscrollbar(ttChooseCDF, repeatinterval=5,
											 command=function(...)tkyview(tl,...))
	## Safest to make sure scr exists before setting yscrollcommand
	tl<-tklistbox(ttChooseCDF,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white",font=.affylmGUIglobals$affylmGUIfont2)
	lbl2<-tklabel(ttChooseCDF,text="Choose a Chip Definition File (CDF)",font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttChooseCDF,text="       "),row=0,column=1,columnspan=1)
	tkgrid(tklabel(ttChooseCDF,text="       "),row=0,column=4,columnspan=1)
	tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
	tkgrid.configure(lbl2,sticky="w")
	tkgrid(tklabel(ttChooseCDF,text="         "),row=2,column=1)
	tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
	tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns")

	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(cdfPackages <- available.packages(contriburl = contrib.url(getOption("repositories2"))))
	Try(len <- nrow(cdfPackages))
	Try(for (i in (1:len)) tkinsert(tl,"end",paste(cdfPackages[i,"Package"],cdfPackages[i,"Version"][[1]])))
	tkselection.set(tl,0)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))

	ReturnVal <- ""
	onOK <- function()
	{
			Try(cdfIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
			Try(tkgrab.release(ttChooseCDF));Try(tkdestroy(ttChooseCDF));Try(tkfocus(.affylmGUIglobals$ttMain))
			Try(ReturnVal <<- cdfDataFrame[cdfIndex,"Package"])
	}
	onCancel <- function() {Try(tkgrab.release(ttChooseCDF));Try(tkdestroy(ttChooseCDF));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- ""}
	OK.but <-tkbutton(ttChooseCDF,text="   OK	 ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttChooseCDF,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttChooseCDF,text="    "))
	tkgrid(tklabel(ttChooseCDF,text="    "),tklabel(ttChooseCDF,text="    "),OK.but,Cancel.but)
	tkgrid.configure(OK.but,		sticky="e")
	tkgrid.configure(Cancel.but,sticky="w")
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(tl, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	tkgrid(tklabel(ttChooseCDF,text="    "))
	Try(tkfocus(ttChooseCDF))
	Try(tkbind(ttChooseCDF, "<Destroy>", function() {Try(tkgrab.release(ttChooseCDF));Try(tkfocus(.affylmGUIglobals$ttMain))}))
	Try(tkwait.window(ttChooseCDF))

	return (ReturnVal)
}

