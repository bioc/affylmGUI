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

if (require(affy)==FALSE)
{
    tkmessageBox(title="An error has occured!",message=paste("Cannot find package affy"),icon="error",type="ok")
    stop("Cannot find package affy")
}


Try <- function(expr) 
{
    require(tcltk)
    if (data.class(result<-try(expr,TRUE))=="try-error")
    {
        tkmessageBox(title="An error has occured!",message=as.character(result),icon="error",type="ok")
    } 
    else 
    {
        return (result)
    }
}

TryReadImgProcFile <- function(expr) 
{
    require(tcltk)
    if (data.class(result<-try(expr,TRUE))=="try-error")
    {
        tkmessageBox(title="Reading Image Processing Files Failed!",
          message="affylmGUI was unable to read the CEL files listed in the Targets file.",icon="error",type="ok")
    } 
    else 
    {
        return (result)
    }
}


Require <- function(pkg) 
{
    require(tcltk)
    if (data.class(result<-try(.find.package(pkg),TRUE))=="try-error")
    {
        tkmessageBox(title="An error has occured!",message=paste("Cannot find package",pkg),icon="error",type="ok")
    } 
    else 
    {
        result <- Try(require(pkg,character.only=TRUE))
    }
    return (result)
}


TclRequire <- function(tclPkg)
{
    require(tcltk)
    if ((data.class(result<-try(tclRequire(tclPkg),TRUE))=="try-error") || (is.logical(result) && result==FALSE))
    {
      Try(winTitle<-"Tcl/Tk Extension(s) Not Found")
      Try(message<-paste("Cannot find Tcl/Tk package \"", tclPkg,
      "\".  Exit affylmGUI (recommended)?\n\n",
      "affylmGUI requires the Tcl/Tk extensions, BWidget and Tktable.  You must have Tcl/Tk installed\n",
      "on your computer, not just the minimal Tcl/Tk installation which comes with R.  If you do have\n",
      "Tcl/Tk installed, including the extensions (e.g. using the ActiveTcl distribution in Windows),\n",
      "make sure that R can find the path to the Tcl library, e.g. C:\\Tcl\\lib (on Windows) or\n",
      "/usr/lib (on Linux/Unix) or /sw/lib on Mac OSX.\n\n",
      "If you don't know how to set environment variables in Windows, one way to make sure the R can\n",
      "find the Tcl/Tk extensions Tktable2.8 and bwidget1.6 is to copy them from your ActiveTcl installation\n",
      "e.g. in C:\\Tcl\\lib into the Tcl subdirectory of your R installation\n\n",
      "If you do understand how to set environment variables...\n",
      "make sure that you have the TCL_LIBRARY environment variable set to the appropriate path, e.g.\n",
      "C:\\Tcl\\lib\\tcl8.4 and the MY_TCLTK environment variable set to a non-empty string, e.g. \"Yes\".\n\n",
      "If using Windows, be sure to read the R for windows FAQ at\nhttp://www.stats.ox.ac.uk/pub/R/rw-FAQ.html\n\n",
      "If your Tcl/Tk extensions still can't be found, try ",
      "addTclPath(\"<path to Tcl library>\").\nThis could be put in $HOME/.Rprofile\n\n",
      "If you need further ",
      "instructions, please contact your system administrator and consider emailing\n ",
      "r-help@stat.math.ethz.ch, or browse through the R-help archives for a similar question.",
      sep=""))
      
      Try(ttTclTkExtension <- tktoplevel(ttMain))
      Try(tkwm.title(ttTclTkExtension,winTitle))
      Try(tkwm.deiconify(ttTclTkExtension))
      Try(tkgrab.set(ttTclTkExtension))
      Try(scr <- tkscrollbar(ttTclTkExtension, repeatinterval=5,
                             command=function(...)tkyview(txt,...)))
      Try(txt <- tktext(ttTclTkExtension,bg="white",yscrollcommand=function(...)tkset(scr,...)))
      Try(tkgrid(txt,scr,columnspan=2))
      Try(tkgrid.configure(scr,columnspan=1,sticky="ns"))
      Try(tkgrid.configure(txt,sticky="nsew"))
      Try(tkinsert(txt,"end",message))
      Try(tkconfigure(txt, state="disabled"))
      Try(tkfocus(txt))
      Try(onYes <- function() 
      {
        Try(tkgrab.release(ttTclTkExtension))
        Try(tkdestroy(ttTclTkExtension))
        try(tkdestroy(ttMain))
        Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))    
        Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
        if (limmaDataSetNameText!="Untitled")
        {
          Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
          Try(mbVal <- tkmessageBox(title="Aborting from affylmGUI",
                message=paste("Save changes to ",LimmaFileName,"?",sep=""),
                icon="question",type="yesno",default="yes"))
          try(if (tclvalue(mbVal)=="yes")
              try(SaveLimmaFile(),silent=TRUE),silent=TRUE)
         }

        quit()                
      })
      Try(onNo <- function()
      {
        Try(tkgrab.release(ttTclTkExtension))
        Try(tkdestroy(ttTclTkExtension))      
      })
      Try(Yes.but <- tkbutton(ttTclTkExtension,text="  Yes  ",command=onYes,font=affylmGUIfont2))  
      Try(No.but <- tkbutton(ttTclTkExtension,text="  No  ",command=onNo,font=affylmGUIfont2)) 
      Try(tkgrid.configure(txt,columnspan=2))
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))
      Try(tkgrid(tklabel(ttTclTkExtension,text="Exit affylmGUI (recommended)?",font=affylmGUIfont2),columnspan=2))
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))      
      Try(tkgrid(Yes.but,No.but))
      Try(tkgrid.configure(Yes.but,sticky="e"))
      Try(tkgrid.configure(No.but,sticky="w"))      
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))    
      Try(tkfocus(Yes.but))
      Try(tkwait.window(ttTclTkExtension))            
    }
}

affylmGUI <- function(BigfontsForaffylmGUIpresentation=FALSE)
{
  assign("affylmGUIenvironment",new.env(),.GlobalEnv)
  assign("Try",get("Try",envir=.GlobalEnv),affylmGUIenvironment)
  # This option is for when I give a Presentation/talk on affylmGUI and want large affylmGUIfonts.  Currently, there are
  # some affylmGUIfonts which affylmGUI can't control, like menus, so as well as changing affylmGUIpresentation to TRUE here, I
  # Right-Click the Windows Desktop, click Properties (to get Display properties which can also be accessed
  # through the Control Panel) then click on Appearance, and then change the affylmGUIfont size for menu,window title, etc.)
  # Rather than change each affylmGUIfont (menu,window title,...) manually each time, I save the changes as a "scheme".  
  Try(if (BigfontsForaffylmGUIpresentation==TRUE)
    Try(assign("affylmGUIpresentation", TRUE, .GlobalEnv))
  else
    Try(assign("affylmGUIpresentation", FALSE, .GlobalEnv)))
  Try(assign("limmaDataSetName",tclVar("Untitled"),.GlobalEnv))
  Try(initGlobals())

  Try(assign("Myhscale", 1, .GlobalEnv))
  Try(assign("Myvscale", 1, .GlobalEnv))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows") 
  {
 
   Try(if (Sys.info()["sysname"]=="Darwin")
   {
     Try(addTclPath("/sw/lib/tcl8.4"))
     Try(addTclPath("/sw/lib"))
     Try(addTclPath("./lib"))
     Try(addTclPath("/sw/lib/tk8.4"))
     Try(addTclPath(paste(Sys.getenv("HOME"),.Platform$file.sep,"TkExtensions",sep="")))
   })    
   Try(addTclPath("/usr/local/lib"))
   Try(addTclPath("/usr/local/Tcl/lib"))
   Try(addTclPath("/usr/local/lib/Tcl"))
   Try(addTclPath("/usr/lib"))
   Try(addTclPath("/usr/lib/Tcl"))
   Try(addTclPath("/usr/local/ActiveTcl/lib"))
   Try(assign("Myhscale", 1, .GlobalEnv))
   Try(assign("Myvscale", 1, .GlobalEnv))
 })

  Try(if (Sys.info()["sysname"] == "Windows") 
  {
    Try(assign("Myhscale",1.6,.GlobalEnv))
    Try(assign("Myvscale",1.6,.GlobalEnv))
  })

  Try(if (Sys.info()["sysname"] == "Darwin" && !exists("X11", env=.GlobalEnv)) 
  {
    Try(addTclPath("/Library/Tcl"))
    Try(addTclPath("/Network/Library/Tcl"))
    Try(addTclPath("/System/Library/Tcl"))
    Try(addTclPath("/Library/Frameworks/Tcl"))
    Try(HOME <- Sys.getenv("HOME"))
    Try(if (nchar(HOME)>0)
    {
      Try(addTclPath(paste(HOME,"/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/Network/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/System/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/Library/Frameworks/Tcl",sep="")))
    })
    Try(assign("Myhscale", 1, .GlobalEnv))
    Try(assign("Myvscale", 1, .GlobalEnv))
  })

  Try(assign("limmaDataSetNameText",get("limmaDataSetNameText",envir=affylmGUIenvironment),.GlobalEnv))

  Try(if (affylmGUIpresentation==TRUE)
    Try(assign("affylmGUIfont1",tkfont.create(family="times",size=48,weight="bold",slant="italic"),.GlobalEnv))
  else
    Try(assign("affylmGUIfont1",tkfont.create(family="times",size=24,weight="bold",slant="italic"),.GlobalEnv)))
  Try(if (affylmGUIpresentation==TRUE)  
    Try(assign("affylmGUIfont2",tkfont.create(family="arial",size=16),.GlobalEnv))
  else
    Try(assign("affylmGUIfont2",tkfont.create(family="arial",size=10),.GlobalEnv)))
  Try(if (affylmGUIpresentation==TRUE)  
    Try(assign("affylmGUIfontTree",tkfont.create(family="arial",size=14),.GlobalEnv))
  else
    Try(assign("affylmGUIfontTree",tkfont.create(family="arial",size=10),.GlobalEnv)))

  Try(if (affylmGUIpresentation==TRUE)  
    Try(assign("affylmGUIfontTable",tkfont.create(family="arial",size=16),.GlobalEnv))
  else
    Try(assign("affylmGUIfontTable",tkfont.create(family="arial",size=10),.GlobalEnv)))
  Try(if (affylmGUIpresentation==TRUE)  
    Try(assign("affylmGUIfontTopTable",tkfont.create(family="arial",size=12,weight="bold"),.GlobalEnv))
  else
    Try(assign("affylmGUIfontTopTable",affylmGUIfontTable,.GlobalEnv)))

  Try(if (affylmGUIpresentation==TRUE)    
    Try(assign("affylmGUIfont2b",tkfont.create(family="arial",size=16,weight="bold"),.GlobalEnv))
  else
    Try(assign("affylmGUIfont2b",tkfont.create(family="arial",size=10,weight="bold"),.GlobalEnv)))

  Try(if (affylmGUIpresentation==TRUE)  
    Try(assign("affylmGUIfontCourier",tkfont.create(family="courier",size=16),.GlobalEnv))
  else
    Try(assign("affylmGUIfontCourier",tkfont.create(family="courier",size=10),.GlobalEnv)))

  Try(assign("mainTreeWidth",30,.GlobalEnv))

  Try(if (affylmGUIpresentation==TRUE)
    Try(assign("ContrastParameterizationTREEWidth",40,.GlobalEnv))
  else
    Try(assign("ContrastParameterizationTREEWidth",30,.GlobalEnv)))

  Try(if (affylmGUIpresentation==TRUE)  
  {
    Try(assign("ContrastParameterizationTREEHeight",20,.GlobalEnv))
    Try(assign("mainTreeHeight",20,.GlobalEnv))
  }
  else
  {
    Try(assign("ContrastParameterizationTREEHeight",15,.GlobalEnv))
    Try(assign("mainTreeHeight",15,.GlobalEnv))  
  })
  # Try(assign("affylmGUIfontMenu",tkfont.create(family="arial",size=10),.GlobalEnv))

  # Try(assign("opar",par(bg="white"),.GlobalEnv))
  Try(assign("oldOptions",options(warn=-1),.GlobalEnv)) # Otherwise R complains that I'm trying to set main in plots, i.e. set a plot title)
# Maybe it would be nice to eventually use the MainFrame widget from BWidget so we can have a nice toolbar etc.
  TclRequire("BWidget")
  Try(assign("ttMain",tktoplevel(),.GlobalEnv))
  Try(if (affylmGUIpresentation==FALSE)
    Try(mainFrame <- tkframe(ttMain,relief="groove",borderwidth="2"))
  else
    Try(mainFrame <- tkframe(ttMain)))    
  Try(if (affylmGUIpresentation==FALSE)
  {
    Try(toolbarFrame <- tkframe(mainFrame,relief="groove",borderwidth="2"))
    Try(tb <- tkframe(toolbarFrame,relief="groove",borderwidth="2"))
    # The Bitmap::get stuff below requires the BWidget package.
    # I think this could be done more simply with something like :
    #   Try(newButton <- tkbutton(tb,image=tkcmd("Bitmap::get"new"),command=NewLimmaFile))
    Try(newButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get new]",.Tcl.args(command=NewLimmaFile))))
    Try(openButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get open]",.Tcl.args(command=OpenLimmaFile))))
    Try(saveButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get save]",.Tcl.args(command=SaveLimmaFile))))  
    Try(tkgrid(newButton,openButton,saveButton,sticky="w"))
    Try(tkgrid(tb,sticky="nw"))
  #  Try(tkgrid(toolbarFrame,sticky="ew"))
    Try(tkgrid(toolbarFrame,sticky="w"))
  #  Try(tkgrid.configure(tb,sticky="w"))
  })
  
  Try(LimmaFileName <- get("LimmaFileName",affylmGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only 
  Try(tkwm.title(ttMain,paste("affylmGUI -",LimmaFileName)))
  Try(assign("done",tclVar(0),.GlobalEnv))
  Try(assign("CDFfileBoxTitle",tclVar("Please select a Chip Definition (CDF) file."),.GlobalEnv))
  Try(assign("CDFfileName",tclVar("No filename is selected at the moment.  Press the Select CDF File Button."),.GlobalEnv))
  Try(assign("TargetsfileBoxTitle",tclVar("Please select a tab-delimited file listing the CEL files."),.GlobalEnv))
  Try(assign("TargetsfileName",tclVar("No filename is selected at the moment.  Press the Select Targets File Button."),.GlobalEnv))
   
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=3))
  Try(if (affylmGUIpresentation==TRUE)
    Try(tkgrid(tklabel(mainFrame,text="affylmGUI",font=affylmGUIfont1),column=1,columnspan=3,sticky="ew"))
  else
    Try(tkgrid(tklabel(mainFrame,text="     affylmGUI ",font=affylmGUIfont1),column=2,sticky="ew")))
  Try(tkgrid(tklabel(mainFrame,text="Welcome to affylmGUI, a package for Linear Modelling of Microarray Data",font=affylmGUIfont2),columnspan=5))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(limmaDataSetName.but <- tkbutton(mainFrame,text="Data Set Name",command=GetlimmaDataSetName,font=affylmGUIfont2))
  Try(tkgrid(limmaDataSetName.but,column=2,columnspan=1))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(TclRequire("BWidget"))
  Try(mainTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(mainTree,...),orient="horizontal"))
  Try(mainTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(mainTree,...)))
  Try(assign("mainTree",tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(mainTreeXScr,...),yscrollcommand=function(...)tkset(mainTreeYScr,...),width=mainTreeWidth,height=mainTreeHeight,bg="white"),.GlobalEnv))
  Try(LinModTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(ContrastParameterizationTREE,...),orient="horizontal"))
  Try(LinModTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(ContrastParameterizationTREE,...)))
  Try(ContrastParameterizationTREE <- tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(LinModTreeXScr,...),yscrollcommand=function(...)tkset(LinModTreeYScr,...),width=ContrastParameterizationTREEWidth,height=ContrastParameterizationTREEHeight,bg="white"))
  Try(assign("ContrastParameterizationTREE",ContrastParameterizationTREE,.GlobalEnv)) 
  Try(assign("limmaDataSetNameTextLabel",tklabel(mainFrame,text=limmaDataSetNameText,font=affylmGUIfont2b),.GlobalEnv))
  Try(tkgrid(tklabel(mainFrame,text="    "),limmaDataSetNameTextLabel,tklabel(mainFrame,text="    "),tklabel(mainFrame,text="CONTRASTS PARAMETERIZATIONS",font=affylmGUIfont2b),tklabel(mainFrame,text="                ")))
  Try(tkgrid(tklabel(mainFrame,text="    "),mainTree,mainTreeYScr,ContrastParameterizationTREE,LinModTreeYScr))
  Try(tkconfigure(limmaDataSetNameTextLabel,textvariable=limmaDataSetName))
  Try(tkgrid.configure(mainTree,rowspan=6,sticky="ns"))
  Try(tkgrid.configure(mainTreeYScr,rowspan=6,sticky="wns"))
  Try(tkgrid.configure(ContrastParameterizationTREE,rowspan=6,sticky="ns"))
  Try(tkgrid.configure(LinModTreeYScr,rowspan=6,sticky="wns"))
  Try(tkgrid(tklabel(mainFrame,text="    "),mainTreeXScr,tklabel(mainFrame,text="    "),LinModTreeXScr))
  Try(tkgrid.configure(mainTreeXScr,sticky="ewn"))
  Try(tkgrid.configure(LinModTreeXScr,sticky="ewn"))

  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  
  Try(tkgrid(mainFrame))

  Try(tkinsert(mainTree,"end","root","RawAffyData" ,text="Raw Affy Data",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","root","NormalizedAffyData" ,text="Normalized Affy Data",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","root","LinearModelFit" ,text="Linear Model Fit",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","LinearModelFit","LinearModelFit.Status" ,text="Not Available",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","root","Parameters" ,text="Parameters",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=affylmGUIfontTree))  
  Try(tkinsert(mainTree,"end","root","ContrastParameterizations" ,text="Contrasts Parameterizations",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=affylmGUIfontTree))


  Try(assign("ArraysLoaded", FALSE,.GlobalEnv))    
  Try(assign("ArraysLoaded",FALSE,affylmGUIenvironment))

  Try(mainMenu <- tkmenu(ttMain, tearoff=FALSE))
  Try(fileMenu <- tkmenu(mainMenu, tearoff=FALSE))
  Try(tkadd(fileMenu, "command", label="New     Ctrl+N",                    command=NewLimmaFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Open    Ctrl+O",                   command=OpenLimmaFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save    Ctrl+S",                   command=SaveLimmaFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As",                   command=SaveAsLimmaFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "separator"))
  Try(tkadd(fileMenu, "command", label="Export HTML Report",                   command=ExportHTMLreport)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "separator"))
  Try(tkadd(fileMenu, "command", label="Working Directory",      command=SetWD)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "separator"))
  Try(tkadd(fileMenu, "command", label="Exit",                   command=onExit)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(mainMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(RNATargetsMenu <- tkmenu(mainMenu,tearoff=FALSE))
  Try(tkadd(RNATargetsMenu, "command", label="RNA Targets", command=ViewRNATargets)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(mainMenu,  "cascade", label="RNA Targets",menu=RNATargetsMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(normalizationMenu <- tkmenu(mainMenu,tearoff=FALSE))
#  Try(tkadd(normalizationMenu, "command", label="Select Background-Correction Method", command=GetBackgroundCorrectionMethod)) # ) # ,font=affylmGUIfontMenu))
#  Try(tkadd(normalizationMenu, "command", label="Select Between-Array Normalization Method", command=GetNormalizationMethod)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(normalizationMenu, "command", label="Normalize", command=NormalizeNow)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(normalizationMenu, "command", label="Export Normalized Expression Values", command=ExportNormalizedExpressionValues)) # ) # ,font=affylmGUIfontMenu))
#  Try(tkadd(normalizationMenu, "command", label="About Normalization", command=AboutNormalization)) # ) # ,font=affylmGUIfontMenu))  
  Try(tkadd(mainMenu,  "cascade", label="Normalization",menu=normalizationMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(linearModelMenu <- tkmenu(mainMenu,tearoff=FALSE))
  Try(tkadd(linearModelMenu, "command", label="Compute Linear Model Fit", command=ComputeLinearModel)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(linearModelMenu, "separator"))
  Try(tkadd(linearModelMenu, "command", label="Compute Contrasts", command=ComputeContrasts)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(linearModelMenu, "command", label="View Existing Contrasts Parameterization", command=ViewExistingContrastParameterization)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(linearModelMenu, "command", label="Delete Contrasts Parameterization", command=DeleteContrastParameterization)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(mainMenu,  "cascade", label="Linear Model",menu=linearModelMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(evaluateMenu <- tkmenu(mainMenu,tearoff=FALSE))
  Try(tkadd(evaluateMenu, "command", label="Evaluate R Code", command=evalRcode)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(mainMenu,  "cascade", label="Evaluate",menu=evaluateMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(toptableMenu <- tkmenu(mainMenu, tearoff=FALSE))
  Try(tkadd(toptableMenu, "command", label="Table of Genes Ranked in order of Differential Expression", 
      command=showTopTable)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(mainMenu,  "cascade", label="TopTable",menu=toptableMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(plotMenu <- tkmenu(mainMenu, tearoff=FALSE))
  Try(tkadd(plotMenu, "command", label="Intensity Histogram",command=IntensityHistogram))
  Try(tkadd(plotMenu, "command", label="Intensity Density Plot",command=DensityPlot))  
  Try(tkadd(plotMenu, "command", label="Raw Intensity Box Plot",command=RawIntensityBoxPlot))
  Try(tkadd(plotMenu, "command", label="Normalized Intensity Box Plot",command=NormalizedIntensityBoxPlot))  
  Try(tkadd(plotMenu, "command", label="M A Plot (for two slides)",command=affyPlotMA))  
  Try(tkadd(plotMenu, "command", label="Image Array Plot", command=ImageArrayPlot))
  Try(tkadd(plotMenu, "command", label="Image Quality Plot", command=ImageQualityPlot))  
#  Try(tkadd(plotMenu, "command", label="Heat Map",command=HeatMap))    
  Try(tkadd(plotMenu, "separator"))
  Try(tkadd(plotMenu, "command", label="M A Plot (for one contrast)",command=affyPlotMAcontrast))  
  Try(tkadd(plotMenu, "command", label="Quantile-Quantile t Statistic Plot (for one contrast)",command=QQTplot))    
  Try(tkadd(plotMenu, "command", label="Log Odds (Volcano) Plot (for one contrast)",command=LogOddsPlot))      
  Try(tkadd(plotMenu, "command", label="Heat Diagram",command=HeatDiagramPlot)) # ) # ,font=affylmGUIfontMenu))  
  Try(tkadd(plotMenu, "command", label="Venn Diagram",command=VennDiagramPlot)) # ) # ,font=affylmGUIfontMenu))  
  Try(tkadd(mainMenu,  "cascade", label="Plot",menu=plotMenu)) # ) # ,font=affylmGUIfontMenu))  

  Try(customMenu <- tkmenu(mainMenu,tearoff=FALSE))
  Try(tkadd(customMenu, "separator"))
  Try(tkadd(customMenu, "command", label="Delete Custom Menu Item",command=DeleteCustomMenuItem))  
  Try(tkadd(customMenu, "command", label="About Custom Menu",command=AboutCustomMenu))
  Try(tkadd(mainMenu, "cascade", label="Custom", menu=customMenu))
  
  Try(assign("customMenu",customMenu,.GlobalEnv))

  Try(helpMenu <- tkmenu(mainMenu, tearoff=FALSE))
  Try(affylmGUIhelp <- function() 
  {
#    Try(tkmessageBox(title="affylmGUI Help",message="Not implemented yet."))
#    Try(return())
    Try(affylmGUIhelpIndex <- file.path(system.file("doc",package="affylmGUI"),"index.html"))
    Try(browseURL(affylmGUIhelpIndex))
    Try(tkmessageBox(title="affylmGUI Help",message=paste("Opening affylmGUI help...\nIf nothing happens, please open :\n",affylmGUIhelpIndex,"\nyourself.",sep="")))
  })
  Try(tkadd(helpMenu, "command", label="affylmGUI Help", command=affylmGUIhelp))
  Try(limmaHelp <- function() 
  {
    Try(limmaHelpIndex <- file.path(system.file("doc",package="limma"),"usersguide.html"))
    Try(browseURL(limmaHelpIndex))
    Try(tkmessageBox(title="limma Help",message=paste("Opening limma help...\nIf nothing happens, please open :\n",limmaHelpIndex,"\nyourself.",sep="")))
  })
  Try(affyHelp <- function() 
  {
    Try(affyHelpIndex <- paste(system.file("doc",package="affy"),"affy.pdf",sep="/"))
    Try(browseURL(affyHelpIndex))
    Try(tkmessageBox(title="affy Help",message=paste("Opening affy help...\nIf nothing happens, please open :\n",affyHelpIndex,"\nyourself.",sep="")))
  })
  Try(tkadd(helpMenu, "command", label="Affy Help", command=affyHelp))
  Try(tkadd(helpMenu, "command", label="Limma Help", command=limmaHelp))
  Try(tkadd(helpMenu, "separator"))
  Try(tkadd(helpMenu, "command", label="About affylmGUI", command=AboutaffylmGUI)) 
  Try(tkadd(mainMenu,  "cascade", label="Help",menu=helpMenu))

  Try(tkconfigure(ttMain,menu=mainMenu))

  Try(assign("mainMenu",mainMenu,.GlobalEnv))

  Try(if (affylmGUIpresentation==FALSE)
  {
    Try(labelStatusBar <- tklabel(ttMain,font=affylmGUIfont2))
    Try(tkgrid(labelStatusBar,sticky="w"))
    Try(CurrentStatus <- tclVar("    "))
    Try(tkconfigure(labelStatusBar,textvariable=CurrentStatus))
    Try(tkbind(saveButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Save the current Limma file."))
    Try(tkbind(saveButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
    Try(tkbind(openButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Open an existing Limma file."))
    Try(tkbind(openButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
    Try(tkbind(newButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Start a new Limma analysis."))
    Try(tkbind(newButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
  })
  
  #Try(tkwm.resizable(ttMain,"true","false"))

  Try(tkbind(ttMain, "<Destroy>", function() tclvalue(done) <- 2))
  Try(tkbind(ttMain, "<Control-N>", NewLimmaFile))
  Try(tkbind(ttMain, "<Control-S>", SaveLimmaFile))
  Try(tkbind(ttMain, "<Control-O>", OpenLimmaFile))
  Try(tkbind(ttMain, "<Control-n>", NewLimmaFile))
  Try(tkbind(ttMain, "<Control-s>", SaveLimmaFile))
  Try(tkbind(ttMain, "<Control-o>", OpenLimmaFile))

  Try(tkfocus(ttMain))
  Try(tkwait.variable(done))

  Try(abortedaffylmGUI <- 0)
  Try(if(tclvalue(done)=="2") 
  {
    Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))    
    Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
    if (limmaDataSetNameText!="Untitled")
    {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
      Try(mbVal <- tkmessageBox(title="Aborting from affylmGUI",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesno",default="yes"))
      try(if (tclvalue(mbVal)=="yes")
          try(SaveLimmaFile(),silent=TRUE),silent=TRUE)
     }     
    abortedaffylmGUI <- 1
    Try(if (Sys.info()["sysname"] == "Windows") 
      bringToTop(-1))
  })    

  try(tkdestroy(ttMain),silent=TRUE)
  Try(temp <- options(oldOptions))
  invisible()
}

getPackageVersion <- function(pkgName)
{
  DESCRIPTION <- readLines(paste(system.file(package=pkgName),"/DESCRIPTION",sep=""))
  lineNum <- grep("Version",DESCRIPTION)
  VersionLineWords <- strsplit(DESCRIPTION[lineNum]," ")[[1]]
  numWords <- length(VersionLineWords)
  VersionLineWords[numWords]
}

initGlobals <- function()
{
  assign("affylmGUIVersion",getPackageVersion("affylmGUI"),affylmGUIenvironment)
  assign("limmaVersion",getPackageVersion("limma"),affylmGUIenvironment)  
  assign("LimmaFileName","Untitled",affylmGUIenvironment)
  assign("maLayout",list(),affylmGUIenvironment)
  assign("MA" , list(M=matrix(data=0,nrow=1,ncol=1),A=matrix(data=0,nrow=1,ncol=1)),affylmGUIenvironment)
  assign("MAraw" , list(),affylmGUIenvironment)
  assign("MAwithinArrays" , list(),affylmGUIenvironment)
  assign("MAbetweenArrays" , list(),affylmGUIenvironment)  
  assign("MAboth" , list(),affylmGUIenvironment)    
  assign("RawAffyData" , 0,affylmGUIenvironment)
  assign("CDFFile" , "",affylmGUIenvironment)
  assign("ContrastParameterizationList" , list(),affylmGUIenvironment)
  assign("cdf" , data.frame(),affylmGUIenvironment)
  assign("NumSlides" , 0,affylmGUIenvironment)
  assign("NumContrastParameterizations", 0, affylmGUIenvironment)
  assign("ContrastParameterizationNamesVec", c(), affylmGUIenvironment)
  assign("ContrastParameterizationTREEIndexVec",c(), affylmGUIenvironment)
  assign("NumParameters" , 0,affylmGUIenvironment)
  assign("SlideNamesVec" , c(),affylmGUIenvironment)
  assign("Targets" , data.frame(),affylmGUIenvironment)
  assign("ndups" , 1,affylmGUIenvironment)
  assign("spacing" , 1,affylmGUIenvironment)
  assign("limmaDataSetNameText" , "Untitled",affylmGUIenvironment)
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(tclvalue(limmaDataSetName) <- "Untitled")
  assign("ArraysLoaded",FALSE,affylmGUIenvironment)
  assign("LinearModelComputed",rep(FALSE,100),affylmGUIenvironment)  # Maximum of 100 parameterizations for now.
  assign("WeightingType","none", affylmGUIenvironment)
  assign("AreaLowerLimit",160, affylmGUIenvironment)
  assign("AreaUpperLimit",170, affylmGUIenvironment)
  assign("FlagSpotWeighting", 0.1, affylmGUIenvironment)
  assign("RawAffyData.Available",FALSE,affylmGUIenvironment)
  assign("NormalizedAffyData.Available",FALSE,affylmGUIenvironment)
  assign("LinearModelFit.Available",FALSE,affylmGUIenvironment)
  assign("Layout.Available",FALSE,affylmGUIenvironment)  
  assign("numConnectedSubGraphs",1,affylmGUIenvironment)
  assign("connectedSubGraphs",list(),affylmGUIenvironment)  
  assign("NumRNATypes",2,affylmGUIenvironment)
  assign("WithinArrayNormalizationMethod","printtiploess",affylmGUIenvironment)
  assign("menuItemNamesVec",c(),.GlobalEnv)
  assign("NormalizedAffyData",0,affylmGUIenvironment)
  assign("geneNames",c(),affylmGUIenvironment)
  assign("geneSymbols",c(),affylmGUIenvironment)  
  assign("NormMethod","RMA",affylmGUIenvironment)
  assign("weightsPLM",data.frame(),affylmGUIenvironment)
}

deleteItemFromList <- function(list1,itemName=NULL,index=NULL)
{
    if (is.null(index))
      index <- match(itemName,attributes(list1)$names)
    if (is.na(index))
        return(list1)
    len <- length(list1)
    newlist <- list()
    count <- 0
    for (i in (1:len))
        if (i!=index)
        {
            count <- count + 1
            if (!is.null(attributes(list1)$names[i]))
            {
              newlist <- c(newlist,list(foo=list1[[i]]))
              attributes(newlist)$names[count] <- attributes(list1)$names[i]
            }
            else
              newlist[[count]] <- list1[[i]]
        }
    return (newlist)    
}


OpenCDFFile <- function()
{
  Try(cdf <- ChooseCDF())
  Try(if (cdf=="") return())
  Try(assign("CDFFile",cdf,affylmGUIenvironment))
  Try(CDFFile <- get("CDFFile",envir=affylmGUIenvironment))
  Try(tclvalue(CDFfileBoxTitle) <- "Chip Definition (CDF) File")
  Try(tclvalue(CDFfileName) <-paste(CDFFile))
  Try(tkconfigure(ttMain,cursor="watch"))
  Require("reposTools")
  Try(cdfRepos <- getReposEntry("http://www.bioconductor.org/data/cdfenvs/repos"))
  Try(install.packages2(cdf,cdfRepos))
  Try(assign("cdf",cdf,affylmGUIenvironment)) # Can then use ls(env=cdf)
  Try(tkconfigure(ttMain,cursor="arrow"))  
  Try(ArraysLoaded <- FALSE)
  Try(assign("ArraysLoaded",ArraysLoaded,affylmGUIenvironment))

  tkfocus(ttMain)
}

OpenTargetsFile <- function()
{
  Try(TargetsFile <- tclvalue(tkgetOpenFile(filetypes="{{Targets Files} {.txt}} {{All files} *}")))
  Try(if (!nchar(TargetsFile)) return())
  Try(assign("TargetsFile",TargetsFile,affylmGUIenvironment))
  Try(tclvalue(TargetsfileBoxTitle) <- paste("Targets File"))
  Try(tclvalue(TargetsfileName) <- paste(TargetsFile))
  Try(Targets <- read.table(TargetsFile,header=TRUE,sep="\t",quote="\"",as.is=TRUE))
  Try(if (!("FileName" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"FileName\" column.",icon="error"))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return()
  })
  Try(if (!("Target" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"Target\" column.",icon="error"))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return()
  })   
  Try(if (!("Name" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"Name\" column.",icon="error"))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return()
  })   
  
  Try(assign("Targets",Targets,affylmGUIenvironment))
  Try(assign("NumSlides",nrow(Targets),affylmGUIenvironment))

  Try(ArraysLoaded <- FALSE)
  Try(assign("ArraysLoaded",ArraysLoaded,affylmGUIenvironment))

  Try(tkfocus(ttMain))
}

tclArrayVar <- function()
{
    Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
    Try(name <- paste("::RTcl", n,sep = ""))
    Try(l <- list(env = new.env()))
    Try(assign(name, NULL, envir = l$env))
    Try(reg.finalizer(l$env, function(env) tkcmd("unset", ls(env))))
    Try(class(l) <- "tclArrayVar")
    Try(.Tcl(paste("set ",name,"(0,0) \"\"",sep="")))
    l
}


GetContrasts <- function(NumContrasts=0)  
{ 
  Try(NumSlides         <- get("NumSlides",        envir=affylmGUIenvironment))
  Try(Targets           <- get("Targets",          envir=affylmGUIenvironment))
  Try(ArraysLoaded      <- get("ArraysLoaded",     envir=affylmGUIenvironment)) 
  Try(LinearModelFit.Available <- get("LinearModelFit.Available", envir=affylmGUIenvironment))
  
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Contrasts Matrix",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))      
      return()
  }

  if (LinearModelFit.Available==FALSE)
  {
    Try(ComputeLinearModel())
#    Try(tkmessageBox(title="Compute Contrasts",message="There is no linear model fit available.  Select \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
#    Try(tkfocus(ttMain))
#    return()  
  }  

  Try(design            <- get("design",           envir=affylmGUIenvironment))
  Try(NumParameters     <- ncol(design))
  Try(ParameterNamesVec <- colnames(design))  
  
  GetContrastsTable <- function(contrastsFromDropDowns)
  {
    Try(TclRequire("Tktable"))
    Try(ttContrastsTable <- tktoplevel(ttMain))
    Try(tkwm.deiconify(ttContrastsTable))
    Try(tkgrab.set(ttContrastsTable))
    Try(tkfocus(ttContrastsTable))
    Try(tkwm.title(ttContrastsTable,"Contrasts Matrix"))
    Try(ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))
    Try(contrastsMatrix <- contrastsFromDropDowns$contrasts)    
    Try(tclArrayVar1 <- tclArrayVar())
    Try(tclArrayName <- ls(tclArrayVar1$env))

    onOK <- function()
    {
        Try(tkcmd("event","generate",.Tk.ID(table1),"<Leave>"))
        NumRows <- NumParameters
        NumCols <- NumContrasts
        Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
        Try(rownamescontrastsMatrix <- c())
        for (i in (1:NumRows))
            Try(rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamescontrastsMatrix <- c())
        if (NumCols>0)
          for (j in (1:NumCols))
            Try(colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
        Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
        if (NumCols>0)
          for (i in (1:NumRows))
            for (j in (1:NumCols))
                Try(contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))
        Try(tkgrab.release(ttContrastsTable))
        Try(tkdestroy(ttContrastsTable))
        Try(tkfocus(ttMain))
        Try(ReturnVal <<- list(contrasts=contrastsMatrix,contrastsCreatedFromDropDowns=FALSE))        
    }
    onCancel <- function() 
    {
      Try(tkgrab.release(ttContrastsTable))
      Try(tkdestroy(ttContrastsTable))
      Try(tkfocus(ttMain))
      ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE)
    }       
    Try(OK.but <-tkbutton(ttContrastsTable,text="   OK   ",command=onOK,font=affylmGUIfont2))
    Try(Cancel.but <-tkbutton(ttContrastsTable,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
    Try(tkgrid(tklabel(ttContrastsTable,text="    ")))
    Try(PleaseEntercontrastsMatrixLabel<-tklabel(ttContrastsTable,text="Please enter the contrasts matrix to be used for linear-modelling.",font=affylmGUIfont2))      
    Try(tkgrid(tklabel(ttContrastsTable,text="    "),PleaseEntercontrastsMatrixLabel))
    Try(tkgrid.configure(PleaseEntercontrastsMatrixLabel,columnspan=2))
    Try(tkgrid(tklabel(ttContrastsTable,text="    ")))
    NumRows <- NumParameters
    NumCols <- NumContrasts
    
    Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
    Try(if (nrow(contrastsMatrix)==0)
    {
      Try(ContrastsNamesVec <- c())
      if (NumContrasts>0)
        for (i in (1:NumContrasts)) 
          Try(ContrastsNamesVec <- c(ContrastsNamesVec,paste("Contrast ",i,sep="")))              
    }
    else
        Try(ContrastsNamesVec <- colnames(contrastsMatrix)))      
    Try(ColNamesVec <- ContrastsNamesVec)
        
    Try(rownamescontrastsMatrix <- c())
    Try(myRarray <- "    ")
    for (i in (1:NumRows))
    {
        Try(RowName <- ParameterNamesVec[i])
        Try(rownamescontrastsMatrix <- c(rownamescontrastsMatrix,RowName))
        Try(myRarray <- c(myRarray,paste(RowName)))
    }
    if (NumCols>0)
      for (j in (1:NumCols))      
      {
        Try(myRarray <- c(myRarray,paste(ColNamesVec[j])))
        for (i in (1:NumRows))
        {          
            if (nrow(contrastsMatrix)==0)
                Try(myRarray <- c(myRarray,"0"))
            else
                Try(myRarray <- c(myRarray,paste(contrastsMatrix[i,j])))
        }      
      } 
      # This will give an error if tclArray doesn't exist.
      # .Tcl("unset tclArray")
      Try(dim(myRarray) <- c(NumRows+1,NumCols+1))
      if (NumCols>0)
        for (i in (0:NumRows))
          for (j in (0:NumCols))
          {
             # Modified to use tkcmd!
             Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))             
          }

      # Below, can I just use tkwidget(ttContrastsTable,"table",...) ?  Yes, of course.
      Try(table1 <- .Tk.subwin(ttContrastsTable))
      Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols+1),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0"))))
      Try(tkgrid(tklabel(ttContrastsTable,text="    "),table1))

      Try(tkcmd(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownamescontrastsMatrix))+2))))
      Try(if (nrow(contrastsMatrix)>0)
      {     

        Try(for (j in (1:NumCols))      
          Try(tkcmd(.Tk.ID(table1),"width",paste(j),paste(max(4,max(nchar(ColNamesVec))+2,max(nchar(contrastsMatrix[,j]))+2)))))
      })

#         Try(tkcmd(.Tk.ID(table1),"width","0","25"))

      Try(tkconfigure(table1,font=affylmGUIfontTable))
      Try(tkgrid.configure(table1,columnspan=2))

      Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))

      openContrastsMatrixFile <- function()
      {
        Try(contrastsMatrixFileName <- tclvalue(tkgetOpenFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}")))
        Try(if (!nchar(contrastsMatrixFileName)) return())
        Try(contrastsMatrixTable <- read.table(contrastsMatrixFileName,header=FALSE,sep="\t",quote="\"",as.is=TRUE))
        # This will give an error if tclArray doesn't exist.
        # .Tcl("unset tclArray")
        if (NumCols>0)
          for (i in (0:NumRows))
            for (j in (0:NumCols))
              Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(contrastsMatrixTable[i+1,j+1])))
      }

      saveContrastsMatrixFile <- function()
      {
        Try(contrastsMatrixFileName <- tclvalue(tkgetSaveFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}")))
        Try(if (!nchar(contrastsMatrixFileName)) return())
        Try(len <- nchar(contrastsMatrixFileName))
        if (len<=4)
          Try(  contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
        else if (substring(contrastsMatrixFileName,len-3,len)!=".txt")
              Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
        Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumSlides,ncol=NumParameters)))
        Try(rownamescontrastsMatrix <- c())
        Try(for (i in (1:NumRows))
            rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamescontrastsMatrix <- c())
        if (NumParameters>0)
          Try(for (j in (1:NumCols))
            colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
        Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
        if (NumParameters>0)
          Try(for (i in (1:NumRows))
            for (j in (1:NumParameters))
                contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))

        Try(write.table(contrastsMatrix,file=contrastsMatrixFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
      }

      Try(topMenu <- tkmenu(ttContrastsTable, tearoff=FALSE))
      Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(fileMenu, "command", label="Open",      command=openContrastsMatrixFile)) # ) # ,font=affylmGUIfontMenu))
      Try(tkadd(fileMenu, "command", label="Save As",      command=saveContrastsMatrixFile)) # ) # ,font=affylmGUIfontMenu))
      Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=affylmGUIfontMenu))  

      Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) # ) # ,font=affylmGUIfontMenu))
      Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) # ) # ,font=affylmGUIfontMenu))  

      Try(tkconfigure(ttContrastsTable,menu=topMenu))

      Try(BlankLabel1<-tklabel(ttContrastsTable,text="    "))
      Try(tkgrid(BlankLabel1))
      Try(BlankLabel2<-tklabel(ttContrastsTable,text="    "))
      Try(tkgrid(BlankLabel2,OK.but,Cancel.but))
      Try(tkgrid.configure(OK.but,sticky="e"))
      Try(tkgrid.configure(Cancel.but,sticky="w"))
      Try(BlankLabel3<-tklabel(ttContrastsTable,text="    "))
      Try(tkgrid(BlankLabel3))

      Try(tkfocus(ttContrastsTable))
      Try(tkbind(ttContrastsTable, "<Destroy>", function() {Try(tkgrab.release(ttContrastsTable));Try(tkfocus(ttMain));}))
      Try(tkwait.window(ttContrastsTable))
      return (ReturnVal)
  }

  if (NumParameters<=0)
  {
    Try(tkmessageBox(title="At Least Two RNA Types Are Required",message="You must have at least two types of RNA in your Targets file.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return(list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))    
  }
  
  Try(NumRows <- NumParameters)
  Try(NumCols <- NumContrasts)

  Try(ttContrasts<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttContrasts))
  Try(tkgrab.set(ttContrasts))
  Try(tkfocus(ttContrasts))
  Try(tkwm.title(ttContrasts,"Contrasts"))

  Try(lbl2<-tklabel(ttContrasts,text="Please specify pairs of parameters for which contrasts will be estimated",font=affylmGUIfont2))
  Try(lbl3<-tklabel(ttContrasts,text="                                                                    "))
  Try(tkgrid(tklabel(ttContrasts,text="       "),row=0,column=1,columnspan=1))
  Try(tkgrid(tklabel(ttContrasts,text="       "),row=0,column=4,columnspan=1))
  Try(tkgrid(lbl2,row=1,column=2,columnspan=4,rowspan=1,sticky="ew"))
  Try(tkgrid.configure(lbl2,sticky="w"))
  Try(tkgrid(tklabel(ttContrasts,text="         "),column=1))
  Try(tkgrid(tklabel(ttContrasts,text="         ")))
  Try(tkgrid(tklabel(ttContrasts,text="         "),column=1))
#  plus<-tklabel(ttContrasts,text="   +   ",font=affylmGUIfont2)
#  minus<-tklabel(ttContrasts,text="   -   ",font=affylmGUIfont2)
#  tkgrid(plus,row=3, column=2,sticky="ew")
#  tkgrid(minus,row=3,column=6,sticky="ew")

  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)

  Try(TclList1AsString <- "{")
  Try(for (i in (1:NumParameters))
    TclList1AsString <- paste(TclList1AsString,"{",ParameterNamesVec[i],"} ",sep=""))
  TclList1AsString <- paste(TclList1AsString,"}",sep="")
  TclList2AsString <- TclList1AsString
  
#  Try(plusOrMinusTclListAsString <- "{{minus} {plus}}")
  Try(plusOrMinusTclListAsString <- "{{minus}}")
  
  Try(TclRequire("BWidget"))      

  Try(combo1 <- c())
  Try(combo2 <- c())
  Try(combo3 <- c())
  Try(if (NumCols>0)
    for (contrastIndex in (1:NumCols))
    {
      Try(FirstDropDownColumn <- .Tk.subwin(ttContrasts))
      combo1 <- c(combo1,FirstDropDownColumn)
      Try(.Tcl(paste("ComboBox",.Tk.ID(FirstDropDownColumn),"-editable false -values",TclList1AsString)))
      Try(SecondDropDownColumn <- .Tk.subwin(ttContrasts))
      Try(combo2 <- c(combo2,SecondDropDownColumn))
      Try(.Tcl(paste("ComboBox",.Tk.ID(SecondDropDownColumn),"-editable false -values",TclList2AsString)))
      Try(plusOrMinusDropDown <- .Tk.subwin(ttContrasts))
      Try(combo3 <- c(combo3,plusOrMinusDropDown))
      Try(.Tcl(paste("ComboBox",.Tk.ID(plusOrMinusDropDown),"-editable false -values",plusOrMinusTclListAsString)))
      Try(tkcmd(.Tk.ID(plusOrMinusDropDown),"setvalue","first"))
      Try(if (affylmGUIpresentation==TRUE)            
      {
        Try(tkconfigure(FirstDropDownColumn,width=10))
        Try(tkconfigure(SecondDropDownColumn,width=10))      
        Try(tkconfigure(plusOrMinusDropDown,width=10))              
      })
      Try(dropdownLabel <- paste("Contrast",contrastIndex, "  ")  )
      
      Try(tkgrid(tklabel(ttContrasts,text=dropdownLabel,font=affylmGUIfont2),row=2+contrastIndex,
                        column=0,sticky="w"))
      Try(tkconfigure(FirstDropDownColumn,font=affylmGUIfont2))
      Try(tkconfigure(SecondDropDownColumn,font=affylmGUIfont2))
      Try(tkconfigure(plusOrMinusDropDown,font=affylmGUIfont2))      
      Try(tkgrid(FirstDropDownColumn,row=2+contrastIndex,column=2,columnspan=1,rowspan=1))
      Try(tkgrid(plusOrMinusDropDown,row=2+contrastIndex,column=4,columnspan=1,rowspan=1))      
      Try(tkgrid(SecondDropDownColumn,row=2+contrastIndex,column=6,columnspan=1,rowspan=1))

      Try(tkgrid(tklabel(ttContrasts,text="    "),row=2+contrastIndex,column=7))
    })
  Try(tkgrid(tklabel(ttContrasts,text="                                                 "),rowspan=1,columnspan=4))

  Try(ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c()))
  
  OnAdvanced <- function()
  {
      Try(contrastsFromDropDowns <- GetContrastsFromDropDowns())
      Try(ReturnValcontrastsMatrixTable <- GetContrastsTable(contrastsFromDropDowns)) # Returns contrastsMatrix list object including contrastsMatrix matrix as data.frame
      NumRows <- nrow(ReturnValcontrastsMatrixTable$contrasts)
      if (NumRows>0 ) # OK was clicked, not Cancel
      {
          Try(tkgrab.release(ttContrasts))
          Try(tkdestroy(ttContrasts))
          Try(tkfocus(ttMain))
          ReturnVal <<- ReturnValcontrastsMatrixTable   # List contains contrastsMatrix matrix as data.frame
      }   
  }

  GetContrastsFromDropDowns <- function()
  {
   
    NumRows <- NumParameters
    NumCols <- NumContrasts
   
    Param1 <-c()
    Param2 <-c()    
    
    NoParameter <- 99999
      
      
    if (NumCols>0)
      for (contrastIndex in (1:NumCols))
      {
        # I think I wrote this code when I was an R Tcl/Tk beginner.  Check and update!
        # I think combo1 and combo2 should really be lists, not vectors!!!
        # *2 below, because the c() combines the tkwin objects which are acutally         
        # lists with 2 components: window ID and environment.

        selection1 <- tclvalue(.Tcl(paste(.Tk.ID(combo1[contrastIndex*2-1]),"getvalue")))
        selection2 <- tclvalue(.Tcl(paste(.Tk.ID(combo2[contrastIndex*2-1]),"getvalue")))        
        selection3 <- tclvalue(.Tcl(paste(.Tk.ID(combo3[contrastIndex*2-1]),"getvalue")))                
        Try(if ((selection1!="-1"))
          Try(Param1 <- c(Param1,as.numeric(selection1)+1))
        else
          Try(Param1 <- c(Param1,NoParameter)))
        Try(if ((selection2!="-1"))          
          Try(Param2 <- c(Param2,as.numeric(selection2)+1))
        else
          Try(Param2 <- c(Param2,NoParameter)))
      }

    contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols))
    Try(rownames(contrastsMatrix) <- ParameterNamesVec)

    Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
    ContrastNamesVec <- vector(length=NumContrasts)    
    if (NumContrasts>0)
      for (j in (1:NumContrasts))          
        ContrastNamesVec[j] <- SimplifyContrastsExpression(paste("(",ParameterNamesVec[Param1[j]],")-(",ParameterNamesVec[Param2[j]],")",sep=""))
    colnames(contrastsMatrix) <- ContrastNamesVec    

    Try(for (i in (1:NumParameters))
      for (j in (1:NumContrasts))
      {
        Try(contrastsMatrix[i,j] <- 0)
        Try(if (Param1[j]==i)  
          contrastsMatrix[i,j] <- 1)
        Try(if(Param2[j]==i)
            contrastsMatrix[i,j] <- -1)         
      })        
      
  
    Try(if (max(abs(contrastsMatrix))==0)
      Try(return(list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c()))))      
      
#    # Go through from the right hand column to the left and check for all zeros (i.e. check if max(abs(...)) == 0). If so try to reduce the
#    # number of columns (contrasts).
#    Try(while(max(abs(contrastsMatrix[,NumContrasts]))==0 && NumContrasts > 1)
#      NumContrasts <<- NumContrasts - 1)
#    Try(contrastsMatrix <- contrastsMatrix[,1:NumContrasts,drop=FALSE])
#    Try(Param1 <- Param1[1:NumContrasts])    
#    Try(Param2 <- Param2[1:NumContrasts])   
    
    return(list(contrasts=contrastsMatrix,contrastsCreatedFromDropDowns=TRUE,Param1=Param1,Param2=Param2))
  }

  onOK <- function()
  {
    Try(contrastsMatrixList <- GetContrastsFromDropDowns())
    Try(if (nrow(contrastsMatrixList$contrasts)==0)    
    {
      Try(tkmessageBox(title="Contrasts",message=paste("Error in creating contrasts matrix from drop-down selection. ",
                       "Make sure you have selected a parameter pair for each contrast."),type="ok",icon="error"))                    
      Try(ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c()))
      return()
    } 
    else
    {
      Try(tkgrab.release(ttContrasts))
      Try(tkdestroy(ttContrasts))
      Try(tkfocus(ttMain))
      Try(ReturnVal <<- contrastsMatrixList)
      Try(tkfocus(ttMain))
      return()
    })
  }
  onCancel <- function() 
  {
    Try(tkgrab.release(ttContrasts))
    Try(tkdestroy(ttContrasts))
    Try(tkfocus(ttMain))
    ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,Param1=c(),Param2=c())    
  }   
  Advanced.but <- tkbutton(ttContrasts,text="Advanced...",command=OnAdvanced,font=affylmGUIfont2)
  Try(OK.but <-tkbutton(ttContrasts,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttContrasts,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(OK.but,column=2,row=9+NumParameters))
  Try(tkgrid(Cancel.but,column=4,row=9+NumParameters))
  Try(tkgrid(Advanced.but,column=6,row=9+NumParameters))
  Try(tkgrid(tklabel(ttContrasts,text="    ")))
      
  Try(tkfocus(ttContrasts))
  
  Try(tkbind(ttContrasts, "<Destroy>", function() {Try(tkgrab.release(ttContrasts));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttContrasts))
  return (ReturnVal)
}


# Actually in the two functions below, RNA should really be Param, but it doesn't really make a difference.
# This came from the cDNA version.

SimplifyContrastsExpression <- function(string)
{
  RNATypesAndSign <- GetRNATypesFrom.ContrastsFromDropDowns.String(string)
  RNA1 <- RNATypesAndSign$RNA1
  RNA2 <- RNATypesAndSign$RNA2
  RNA3 <- RNATypesAndSign$RNA3
  RNA4 <- RNATypesAndSign$RNA4
  plusOrMinusSign <- RNATypesAndSign$plusOrMinusSign
  ReturnVal <- string
  
  if (RNA1==RNA3&&plusOrMinusSign=='-')
    ReturnVal <- paste("(",RNA4,")-(",RNA2,")",sep="")  
  if (RNA2==RNA4&&plusOrMinusSign=='-')  
    ReturnVal <- paste("(",RNA1,")-(",RNA3,")",sep="")  
  if (RNA1==RNA4&&plusOrMinusSign=='+')
    ReturnVal <- paste("(",RNA3,")-(",RNA2,")",sep="")    
  if (RNA2==RNA3&&plusOrMinusSign=='+')  
    ReturnVal <- paste("(",RNA1,")-(",RNA4,")",sep="")      
  return(ReturnVal)
  
}

GetRNATypesFrom.ContrastsFromDropDowns.String <- function(string)
{
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
}

ViewContrastsMatrixInTable <- function(contrastsMatrixList,contrastParameterizationIndex=NULL)
{
  Try(design <- get("design",envir=affylmGUIenvironment))  
  Try(ParameterNamesVec <- colnames(design))
  Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))  
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])

  Try(TclRequire("Tktable"))
  Try(ttViewContrastsMatrixTable <- tktoplevel(ttMain))
  Try(tkwm.deiconify(ttViewContrastsMatrixTable))
  Try(tkgrab.set(ttViewContrastsMatrixTable))
  Try(tkfocus(ttViewContrastsMatrixTable))
  Try(tkwm.title(ttViewContrastsMatrixTable,paste("Contrasts matrix for contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex])))
  Try(contrastsMatrix <- contrastsMatrixList$contrasts)
  Try(NumContrasts <- ncol(contrastsMatrix))
    
  onClose <- function() {Try(.Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<Leave>"))));Try(tkgrab.release(ttViewContrastsMatrixTable));Try(tkdestroy(ttViewContrastsMatrixTable));Try(tkfocus(ttMain))}

  Try(NumRows <- NumParameters)
  Try(NumCols <- NumContrasts)
    
  Try(if (is.null(colnames(contrastsMatrix)))
  {
      Try(ColumnNamesVec <- c())
      if (NumCols>0)
        for (i in (1:NumCols)) 
            Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i)))
  }
  else
      Try(ColumnNamesVec <- colnames(contrastsMatrix)))

  Try(RowNamesVec <- c())
  Try(myRarray <- "    ")
  Try(for (i in (1:NumRows))
  {
      Try(if (contrastsMatrix=="Design")      
      {
        Try(if(is.null(rownames(contrastsMatrix)))
          Try(RowName <- SlideNamesVec[i])
        else
          Try(RowName <- rownames(contrastsMatrix)[i]))        
      }
      else # Contrasts
      {
        Try(if (is.null(rownames(contrastsMatrix)))
          Try(RowName <- paste("Param",i))
        else
          Try(RowName <- rownames(contrastsMatrix)[i]))
      })
      Try(RowNamesVec <- c(RowNamesVec,RowName))
      Try(myRarray <- c(myRarray,paste(RowName)))
  })
  
  if (NumCols>0)
    for (j in (1:NumCols))      
    {
      Try(myRarray <- c(myRarray,paste(ColumnNamesVec[j])))
      for (i in (1:NumRows))
      {          
          if (nrow(contrastsMatrix)==0)
              Try(myRarray <- c(myRarray,"0"))
          else
              Try(myRarray <- c(myRarray,paste(contrastsMatrix[i,j])))
      }      
    }      

#      Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#      Try(tclArrayName <- paste("::RTcl", n, sep = ""))  
#      Try(l <- list(env = new.env()))
#      Try(assign(tclArrayName, NULL, envir = l$env))
#      Try(reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
      Try(tclArrayVar1 <- tclArrayVar())
      Try(tclArrayName <- ls(tclArrayVar1$env))

      Try(dim(myRarray) <- c(NumRows+1,NumCols+1))

      # This will give an error if tclArray doesn't exist.
      # .Tcl("unset tclArray")
      if (NumCols>0)
        for (i in (0:NumRows))
          for (j in (0:NumCols))
             Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))

      Try(table1 <- tkwidget(ttViewContrastsMatrixTable,"table"))
      Try(tkconfigure(table1,variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols+1),
        titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",
        colseparator="\"\t\"",resizeborders="col",multiline="0",
        xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),state="disabled"))
      Try(xscr <- tkscrollbar(ttViewContrastsMatrixTable,orient="horizontal", command=function(...)tkxview(table1,...)))
      Try(yscr <- tkscrollbar(ttViewContrastsMatrixTable,command=function(...)tkyview(table1,...)))               
      Try(tkgrid(table1,yscr))
      Try(tkgrid.configure(yscr,sticky="nsw"))
      Try(tkconfigure(table1,font=affylmGUIfontTable))
      Try(tkgrid(xscr,sticky="new"))
      Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))


      Try(tkcmd(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownames(contrastsMatrix)))+2))))
      Try(for (j in (1:NumCols))      
        Try(tkcmd(.Tk.ID(table1),"width",paste(j),paste(max(4,nchar(colnames(contrastsMatrix)[j])+2,max(nchar(contrastsMatrix[,j]))+2)))))


      onSaveContrastsMatrixAs <- function()
      {
        Try(if (contrastsMatrix=="Design")        
          Try(contrastsMatrixFileName <- tclvalue(tkgetSaveFile(filetypes="{{Design Matrix Files} {.txt}} {{All files} *}")))
        else
          Try(contrastsMatrixFileName <- tclvalue(tkgetSaveFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}"))))         
        Try(if (!nchar(contrastsMatrixFileName)) return())
        Try(len <- nchar(contrastsMatrixFileName))
        if (len<=4)
          Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
        else if (substring(contrastsMatrixFileName,len-3,len)!=".txt")
              Try(contrastsMatrixFileName <- paste(contrastsMatrixFileName,".txt",sep=""))
        Try(contrastsMatrix <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
        Try(rownamescontrastsMatrix <- c())
        Try(for (i in (1:NumRows))
            rownamescontrastsMatrix[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamescontrastsMatrix <- c())
        if (NumParameters>0)
          Try(for (j in (1:NumCols))
            colnamescontrastsMatrix[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(contrastsMatrix) <- rownamescontrastsMatrix)
        Try(colnames(contrastsMatrix) <- colnamescontrastsMatrix)
        if (NumParameters>0)
          Try(for (i in (1:NumRows))
            for (j in (1:NumParameters))
                contrastsMatrix[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))

        Try(write.table(contrastsMatrix,file=contrastsMatrixFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
      }



      Try(topMenu <- tkmenu(ttViewContrastsMatrixTable, tearoff=FALSE))

      Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(fileMenu, "command", label="Save As",    command=onSaveContrastsMatrixAs))       
      Try(tkadd(fileMenu, "command", label="Close",      command=onClose)) 
      Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) 
      Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) 
      Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) 

      Try(tkconfigure(ttViewContrastsMatrixTable,menu=topMenu))

      Try(tkfocus(ttViewContrastsMatrixTable))
      Try(tkbind(ttViewContrastsMatrixTable, "<Destroy>", function () {Try(tkgrab.release(ttViewContrastsMatrixTable));Try(tkfocus(ttMain))}))
      Try(tkwait.window(ttViewContrastsMatrixTable))
}

ViewContrastsMatrixAsPairs <- function(contrastsMatrix,contrastsMatrixList,contrastParameterizationIndex=NULL)
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=affylmGUIenvironment))  
  Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))  
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec)
  Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))    

  Try(contrastsMatrix <- contrastsMatrixList$contrasts)  
  Try(NumContrasts <- ncol(contrastsMatrix))
  NumRows <- NumParameters
  NumCols <- NumContrasts
    
  ttViewContrastsMatrixAsPairs<-tktoplevel(ttMain)
  tkwm.deiconify(ttViewContrastsMatrixAsPairs)
  tkgrab.set(ttViewContrastsMatrixAsPairs)
  tkfocus(ttViewContrastsMatrixAsPairs)
  Try(tkwm.title(ttViewContrastsMatrixAsPairs,paste("Contrasts in contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex],".",sep="")))  
  Try(TitleLabel<-tklabel(ttViewContrastsMatrixAsPairs,text=paste("Contrasts in contrasts parameterization ", ContrastParameterizationNamesVec[contrastParameterizationIndex],sep=""),font=affylmGUIfont2b))     

  Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))    
  Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),TitleLabel))
  Try(tkgrid.configure(TitleLabel,columnspan=4))
  Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))  
  Try(ParameterOrContrastLabel <- tklabel(ttViewContrastsMatrixAsPairs,text="Contrast",font=affylmGUIfont2b))
  # Note that plusOrMinus IS A VECTOR (can be different for each contrast).
  Try(plusOrMinus <- contrastsMatrixList$plusOrMinus)
  Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),
             ParameterOrContrastLabel))

  if (is.null(colnames(contrastsMatrix)))
  {
      Try(ColumnNamesVec <- c())
      if (NumCols>0)
        for (i in (1:NumCols)) 
          Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i)))
  }
  else
      Try(ColumnNamesVec <- colnames(contrastsMatrix))

  Try(RowNamesVec <- c())
  Try(for (i in (1:NumRows))
  {
      Try(if (is.null(rownames(contrastsMatrix)))
        Try(RowName <- paste("Param",i))
      else
        Try(RowName <- rownames(contrastsMatrix)[i]))
      Try(RowNamesVec <- c(RowNamesVec,RowName))
  })

  if (NumCols>0)
    for (i in (1:NumCols))
    {
      Try(FirstItemOfPair  <- paste(RowNamesVec[contrastsMatrixList$Param1[i]]))
      Try(SecondItemOfPair <- paste(RowNamesVec[contrastsMatrixList$Param2[i]]))
      Try(if (plusOrMinus[i]=="+") plusOrMinusText <- "plus" else plusOrMinusText <- "minus")
      Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),
                 tklabel(ttViewContrastsMatrixAsPairs,text=ColumnNamesVec[i],background="white",font=affylmGUIfont2),
                 tklabel(ttViewContrastsMatrixAsPairs,text="    "),
                 tklabel(ttViewContrastsMatrixAsPairs,text=FirstItemOfPair,background="white",font=affylmGUIfont2),
                 tklabel(ttViewContrastsMatrixAsPairs,text="    "),
                 tklabel(ttViewContrastsMatrixAsPairs,text=plusOrMinusText,font=affylmGUIfont2,bg="white"),                 
                 tklabel(ttViewContrastsMatrixAsPairs,text="    "),
                 tklabel(ttViewContrastsMatrixAsPairs,text=SecondItemOfPair,background="white",font=affylmGUIfont2),
                 tklabel(ttViewContrastsMatrixAsPairs,text="    ")                 
                 ))
      Try(tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    ")))
    }
  tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="     "))
  
  Advanced.but <- tkbutton(ttViewContrastsMatrixAsPairs,text="Advanced...",command=function() {ViewContrastsMatrixInTable(contrastsMatrixList,contrastParameterizationIndex)},font=affylmGUIfont2)
  onOK <- function() {Try(tkgrab.release(ttViewContrastsMatrixAsPairs));Try(tkdestroy(ttViewContrastsMatrixAsPairs));Try(tkfocus(ttMain))}
  OK.but <-tkbutton(ttViewContrastsMatrixAsPairs,text="   OK   ",command=onOK,font=affylmGUIfont2)
  tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "),OK.but,Advanced.but)
  tkgrid(tklabel(ttViewContrastsMatrixAsPairs,text="    "))
      
  Try(tkfocus(ttViewContrastsMatrixAsPairs))
  
  Try(tkbind(ttViewContrastsMatrixAsPairs, "<Destroy>", function() {Try(tkgrab.release(ttViewContrastsMatrixAsPairs));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttViewContrastsMatrixAsPairs))
}

ViewExistingContrastParameterization <- function()
{
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="View Existing Contrasts Parameterization",message="There are no contrast parameterizations loaded.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }
  
  Try(contrastParameterizationIndex <- ChooseContrastParameterization())
  Try(if (contrastParameterizationIndex==0)    return())
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])

  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
  
  Try(ContrastParameterizationTREEIndexVec <- ContrastParameterizationList[[ContrastParameterizationNameNode]]$ContrastParameterizationTREEIndexVec)
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])          
  Try(ContrastsContrastParameterizationListNode <- paste("ContrastParameterizations.",ContrastParameterizationTREEIndex,".",ContrastParameterizationTREEIndex, sep=""))  
  Try(contrastsList <- ContrastParameterizationList[[ContrastParameterizationNameNode]]$contrastsMatrixInList)  
  Try(if (contrastsList$contrastsCreatedFromDropDowns==FALSE)
      Try(ViewContrastsMatrixInTable(contrastsMatrix="Contrasts",contrastsList,contrastParameterizationIndex))
  else
      Try(ViewContrastsMatrixAsPairs(contrastsMatrix="Contrasts",contrastsList,contrastParameterizationIndex)))
}

ViewRNATargets <- function()
{
      Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
      Try(Targets <- get("Targets",envir=affylmGUIenvironment))      
      Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment)) 
  
      if (ArraysLoaded==FALSE)
      {
          Try(tkmessageBox(title="RNA Targets",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }
      if (nrow(Targets)==0)
      {
          Try(tkmessageBox(title="RNA Targets",message="No RNA targets have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }

      Try(TclRequire("Tktable"))
      Try(ttViewRNATargets <- tktoplevel(ttMain))
      Try(tkwm.deiconify(ttViewRNATargets))
#      Try(tkgrab.set(ttViewRNATargets))
      Try(tkfocus(ttViewRNATargets))
      Try(tkwm.title(ttViewRNATargets,"RNA Targets"))
      
#      Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#     Try(tclArrayName <- paste("::RTcl", n, sep = ""))
      Try(tclArrayVar1 <- tclArrayVar())
      Try(tclArrayName <- ls(tclArrayVar1$env))
      
#      onClose <- function() {Try(tkgrab.release(ttViewRNATargets));Try(tkdestroy(ttViewRNATargets));Try(tkfocus(ttMain))}
      onClose <- function() {Try(tkdestroy(ttViewRNATargets));Try(tkfocus(ttMain))}
      
      Try(NumCols <- ncol(Targets))
      Try(NumRows <- nrow(Targets))
     
      Try(myRarray <- c())

      if (NumCols>0)
        for (j in (1:NumCols))      
        {
          Try(myRarray <- c(myRarray,paste(colnames(Targets)[j])))
          for (i in (1:NumRows))
          {          
              Try(myRarray <- c(myRarray,paste(Targets[i,j])))
          }      
        }      

          Try(dim(myRarray) <- c(NumRows+1,NumCols))

          # This will give an error if tclArray doesn't exist.
          # .Tcl("unset tclArray")
          if (NumRows>0 && NumCols>0)
            for (i in (0:NumRows))
              for (j in (1:NumCols))
#                 Try(.Tcl(paste("set ",tclArrayName,"(",i,",",j-1,") ",myRarray[i+1,j],sep="")))                 
                 Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(myRarray[i+1,j])))
         
          # Below, I should just use tkwidget(ttViewRNATargets,"table",...) 
          Try(table1 <- .Tk.subwin(ttViewRNATargets))
          Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0",
                titlerows=1,colstretchmode="unset",
                xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),state="disabled"))))
          Try(xscr <- tkscrollbar(ttViewRNATargets,orient="horizontal", command=function(...)tkxview(table1,...)))
          Try(yscr <- tkscrollbar(ttViewRNATargets,command=function(...)tkyview(table1,...)))               
          Try(tkgrid(table1,yscr))
          Try(tkgrid.configure(yscr,sticky="nsw"))
          Try(tkgrid(xscr))
          Try(tkgrid.configure(xscr,sticky="new"))
          Try(tkconfigure(table1,font=affylmGUIfontTable))

          for (j in (1:NumCols))      
            Try(tkcmd(.Tk.ID(table1),"width",paste(j-1),paste(max(4,nchar(colnames(Targets)[j])+2,max(nchar(Targets[,j]))+2))))

          Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))

          saveTargetsFile <- function()
          {
            Try(TargetsFileName <- tclvalue(tkgetSaveFile(filetypes="{{RNA Targets Files} {.txt}} {{All files} *}")))
            Try(if (!nchar(TargetsFileName)) return())
            Try(len <- nchar(TargetsFileName))
            if (len<=4)
              Try(  TargetsFileName <- paste(TargetsFileName,".txt",sep=""))
            else if (substring(TargetsFileName,len-3,len)!=".txt")
                  Try(TargetsFileName <- paste(TargetsFileName,".txt",sep=""))
            Try(Targets <- matrix(nrow=NumRows,ncol=NumCols))
            Try(colnamesTargets <- c())
            if (NumCols>0)
              Try(for (j in (1:NumCols))
                colnamesTargets[j] <- tclvalue(paste(tclArrayName,"(0,",j-1,")",sep="")))
            Try(colnames(Targets) <- colnamesTargets)       
            if (NumRows>0 && NumCols>0)
              Try(for (i in (1:NumRows))
                 for (j in (1:NumCols))
                    Targets[i,j] <- tclvalue(paste(tclArrayName,"(",i,",",j-1,")",sep="")))

            Try(write.table(Targets,file=TargetsFileName,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
          }


          Try(topMenu <- tkmenu(ttViewRNATargets, tearoff=FALSE))

          Try(topMenu <- tkmenu(ttViewRNATargets, tearoff=FALSE))
          Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(fileMenu, "command", label="Save As",   command=saveTargetsFile)) # ) # ,font=affylmGUIfontMenu))
          Try(tkadd(fileMenu, "command", label="Close",   command=onClose)) # ) # ,font=affylmGUIfontMenu))          
          Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=affylmGUIfontMenu))  

          Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) # ) # ,font=affylmGUIfontMenu))
          Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) # ) # ,font=affylmGUIfontMenu))  
                   
          Try(tkconfigure(ttViewRNATargets,menu=topMenu))

          Try(tkfocus(ttViewRNATargets))
#          Try(tkbind(ttViewRNATargets, "<Destroy>", function() {Try(tkgrab.release(ttViewRNATargets));Try(tkfocus(ttMain))}))
          Try(tkbind(ttViewRNATargets, "<Destroy>", function() {Try(tkfocus(ttMain))}))
#          Try(tkwait.window(ttViewRNATargets))
}

########################################################################################################
# Some C-style string searching functions, because I'm not very good at using regular expressions ;-) 

# Returns the index where needle is found in haystack or zero if not found.
nstrstr <- function(haystack,needle)
{
  lenHaystack <- nchar(haystack)
  lenNeedle   <- nchar(needle)
  if (lenHaystack < lenNeedle)
    return (0)
  if (lenHaystack == lenNeedle)
    return(haystack==needle)
  lenDiff <- lenHaystack-lenNeedle
  for (i in (1:lenDiff))
    if (needle==substr(haystack,i,i+lenNeedle-1))
      return(i)

  return (0)
}

strstr <- function(haystack,needle)
{
  strIndex <- nstrstr(haystack,needle)
  if (strIndex==0)
    return ("")
  return (substr(haystack,strIndex,nchar(haystack)))
}


ComputeContrasts <- function()
{
  # For now, we will assume that the number of contrasts is one less than the number of parameters,
  # e.g. with 4 treatments, we estimate 4 parameters, then 3 contrasts.
  
  Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
  Try(Targets <- get("Targets",envir=affylmGUIenvironment))
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment)) 
  Try(LinearModelFit.Available <- get("LinearModelFit.Available", envir=affylmGUIenvironment))

  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Compute Contrasts",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  }
  
  if (LinearModelFit.Available==FALSE)
  {
    Try(ComputeLinearModel())
    Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
#    Try(tkmessageBox(title="Compute Contrasts",message="There is no linear model fit available.  Select \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
#    Try(tkfocus(ttMain))
#    return()  
  }  
  Try(fit    <- get("fit",   envir=affylmGUIenvironment))
  Try(design <- get("design",envir=affylmGUIenvironment))

  Try(ParameterNamesVec  <- colnames(design))
  
  Try(NumContrasts <- NumParameters - 1)

  Try(if (NumContrasts<=0)
  {
    tkmessageBox(title="Compute Contrasts",message=paste("You need to have two or more treatments in order to compute contrasts."),type="ok",icon="error")
    Try(tkfocus(ttMain))
    return()
  })
  
  Try(NumContrasts <- min(NumContrasts,10))
  
  Try(contrastsMatrixInList <- GetContrasts(NumContrasts=NumContrasts))
  Try(if (nrow(contrastsMatrixInList$contrasts)==0) return())
  Try(contrastsMatrix <- as.matrix(contrastsMatrixInList$contrasts))
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(contrastsFit <- contrasts.fit(fit,contrastsMatrix))
  
  # NEW
  
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(if (min(contrastsFit$df)==0)
  {
    Try(tkmessageBox(title="No degrees of freedom",message="Empirical Bayes statistics will not be available because of the lack of replicate arrays.",icon="warning"))
    Try(ebayesAvailable <- FALSE)
  }
  else
    Try(ebayesAvailable <- TRUE))
  
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(if (ebayesAvailable==TRUE)
    Try(contrastsEbayes <- eBayes(contrastsFit)))
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(ContrastParameterizationNameText <- GetContrastParameterizationName())  
  Try(if (ContrastParameterizationNameText=="GetContrastParameterizationName.CANCEL") return())
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(while (nchar(ContrastParameterizationNameText)==0)
  {
      Try(tkmessageBox(title="Contrasts Name",message="Please enter a name for this set of contrasts",type="ok",icon="error"))
      Try(ContrastParameterizationNameText <- GetContrastParameterizationName())
      if (ContrastParameterizationNameText=="GetContrastParameterizationName.CANCEL") 
      {
          Try(tkfocus(ttMain))          
          return()
      }
  })
  
  Try(contrastParameterizationIndex <- 0)
  Try(newContrastParameterization <- 1)
  Try(if (ContrastParameterizationNameText %in% ContrastParameterizationNamesVec)
  {
      Try(contrastParameterizationIndex <- match(ContrastParameterizationNameText,ContrastParameterizationNamesVec))
      Try(mbVal<-tclvalue(tkmessageBox(title="Contrasts Parameterization Name",message="This contrasts parameterization name already exists.  Replace?",type="yesnocancel",icon="question")))
      Try(if (mbVal=="cancel") return())
      Try(if (mbVal=="yes") newContrastParameterization <- 0)
      Try(if (mbVal=="no") newContrastParameterization <- 1)
  }
  else
      Try(newContrastParameterization <- 1))

  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))

  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))

  if (newContrastParameterization==1)
  {
      Try(if (length(ContrastParameterizationTREEIndexVec)!=NumContrastParameterizations)
      {
          Try(tkmessageBox(title="Contrasts Parameterizations","Length of ContrastParameterizationTREEIndexVec is not equal to NumContrastParameterizations.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      })
      Try(NumContrastParameterizations <- NumContrastParameterizations + 1)
      Try(contrastParameterizationIndex <- NumContrastParameterizations)
      Try(if (length(ContrastParameterizationTREEIndexVec)==0)
          Try(ContrastParameterizationTREEIndex <- 1)
      else
          Try(ContrastParameterizationTREEIndex <- max(ContrastParameterizationTREEIndexVec)+1))
      Try(ContrastParameterizationTREEIndexVec[contrastParameterizationIndex] <- ContrastParameterizationTREEIndex)

      Try(ContrastParameterizationNamesVec <- c(ContrastParameterizationNamesVec,ContrastParameterizationNameText))     
      
      Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
      Try(ContrastParameterizationList[[ContrastParameterizationNameNode]] <- ContrastParameterizationNameText)
  }
  else # Replace existing contrasts parameterization with the same name.
  {
      Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
      Try(tkdelete(ContrastParameterizationTREE,paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep="")))
  }  
  
  Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
 
  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]] <- list())

  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)
  
  Try(NormalizedAffyData <- get("NormalizedAffyData",affylmGUIenvironment))
  Try(contrastsFit$Amean <- rowMeans(exprs(NormalizedAffyData)))
  
  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$fit <- contrastsFit)
  Try(if (ebayesAvailable==TRUE)  
    Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$eb  <- contrastsEbayes)
  else
    Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$eb  <- list()))
  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$contrastsMatrixInList <- contrastsMatrixInList)
  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$ContrastParameterizationNameText <- ContrastParameterizationNameText)  
 
  if (NumContrastParameterizations>0)
    Try(ContrastsNames <- colnames(contrastsMatrix))
  Try(tkinsert(ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=affylmGUIfontTree))
  Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
  
  Try(ContrastsNode <- paste("ContrastsNode.",ContrastParameterizationTREEIndex))
  
  Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,ContrastsNode,text="Contrasts",font=affylmGUIfontTree))  
  
  Try(for (j in (1:NumContrastsInContrastParameterization))
    Try(tkinsert(ContrastParameterizationTREE,"end",ContrastsNode,paste("Contrasts.",ContrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=affylmGUIfontTree)))          

  Try(LinearModelFitNode       <- paste("LinearModelFitNode.",ContrastParameterizationTREEIndex))
  Try(LinearModelFitStatusNode <- paste("LinearModelFitStatusNode.",ContrastParameterizationTREEIndex))  
  Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,LinearModelFitNode,text="Linear Model Fit",font=affylmGUIfontTree))    
  Try(tkinsert(ContrastParameterizationTREE,"end",LinearModelFitNode,LinearModelFitStatusNode,text="Available",font=affylmGUIfontTree))      
  Try(EmpiricalBayesNode       <- paste("EmpiricalBayesNode.",ContrastParameterizationTREEIndex))
  Try(EmpiricalBayesStatusNode <- paste("EmpiricalBayesStatusNode.",ContrastParameterizationTREEIndex))
  Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,EmpiricalBayesNode,text="Empirical Bayes Statistics",font=affylmGUIfontTree))    
  Try(if (ebayesAvailable==TRUE)
    Try(tkinsert(ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Available",font=affylmGUIfontTree))      
  else
    Try(tkinsert(ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Not Available",font=affylmGUIfontTree))      )
  Try(assign("ContrastParameterizationList",ContrastParameterizationList,affylmGUIenvironment))
  Try(assign("NumContrastParameterizations",NumContrastParameterizations,affylmGUIenvironment))  
  Try(assign("ContrastParameterizationTREEIndexVec",ContrastParameterizationTREEIndexVec,affylmGUIenvironment))
  Try(assign("ContrastParameterizationNamesVec",ContrastParameterizationNamesVec,affylmGUIenvironment))
  
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
      Try(tkdelete(mainTree,paste("ContrastParameterizations.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"ContrastParameterizations.Status.1")))

  Try(if (NumContrastParameterizations>0)
    for (contrastParameterizationIndex in (1:NumContrastParameterizations))
    {
      Try(contrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
      Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",contrastParameterizationTREEIndex,sep=""))
      Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",contrastParameterizationTREEIndex,sep=""))
      Try(tkinsert(mainTree,"end","ContrastParameterizations",ContrastParameterizationsStatusNameNode ,text=ContrastParameterizationNamesVec[contrastParameterizationIndex],font=affylmGUIfontTree))
    }
  else
    Try(tkinsert(mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=affylmGUIfontTree)))  
  
  tkconfigure(ttMain,cursor="arrow")       
}

ComputeLinearModel <- function()
{
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment)) 
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Linear Model",message="Error: No arrays have been loaded.",
        icon="error",default="ok"))
    return()
  })
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
  Try(if (NormalizedAffyData.Available==FALSE)
    NormalizeNow())
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))    
  Try(if (NormalizedAffyData.Available==FALSE)
  {
    tkmessageBox(title="Linear Model",message="An error occured while trying to normalize the data.")
    return()
  
  })
  Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
  Try(Targets <- get("Targets",envir=affylmGUIenvironment))
  Try(design <- as.matrix(as.data.frame(model.matrix(~ -1 + factor(Targets$Target)))))
  Try(NumParameters <- ncol(design))
  Try(assign("NumParameters",NumParameters,affylmGUIenvironment))
  Try(colnames(design) <- gsub("factor\\(Targets\\$Target\\)","",colnames(design)))
  Try(rownames(design) <- Targets$FileName)
  Try(assign("design",design,affylmGUIenvironment))  
  Try(fit <- lm.series(exprs(NormalizedAffyData),design,weights=NULL))
  Try(assign("LinearModelFit.Available",TRUE,affylmGUIenvironment))
  Try(assign("fit",fit,affylmGUIenvironment))
  Try(tkdelete(mainTree,"LinearModelFit.Status"))        
  Try(tkinsert(mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Available",font=affylmGUIfontTree))  
  Try(NumParameters <- get("NumParameters" , envir=affylmGUIenvironment))
  Try(if (NumParameters>0)
    Try(for (i in (1:NumParameters))
      Try(tkdelete(mainTree,paste("Parameters.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"Parameters.Status.1")))
  Try(for (i in (1:ncol(design)))
    Try(tkinsert(mainTree,"end","Parameters",paste("Parameters.Status.",i,sep="") ,
      text=colnames(design)[i],font=affylmGUIfontTree)))
  Try(tkconfigure(ttMain,cursor="arrow"))      
}

GetContrast <- function(contrastParameterizationIndex)
{
  Try(ttGetContrast<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetContrast))
  Try(tkgrab.set(ttGetContrast)  )
  Try(tkfocus(ttGetContrast))
  Try(tkwm.title(ttGetContrast,"Choose a contrast"))
  Try(scr <- tkscrollbar(ttGetContrast, repeatinterval=5,command=function(...)tkyview(tl,...)))
  Try(xscr <- tkscrollbar(ttGetContrast, repeatinterval=5,command=function(...)tkxview(tl,...) ,orient="horizontal"))                       
  ## Safest to make sure scr exists before setting yscrollcommand
  Try(tl<-tklistbox(ttGetContrast,height=4,width=30,selectmode="single",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),background="white",font=affylmGUIfont2)   )
  Try(lbl2<-tklabel(ttGetContrast,text="Which contrast is this for?",font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetContrast,text="       "),row=0,column=1,columnspan=1))
  Try(tkgrid(tklabel(ttGetContrast,text="       "),row=0,column=4,columnspan=1))
  Try(tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1))
  Try(tkgrid.configure(lbl2,sticky="w"))
  Try(tkgrid(tklabel(ttGetContrast,text="         "),row=2,column=1))
  Try(tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew"))
  Try(tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns"))
  Try(tkgrid(xscr,row=6,column=2,columnspan=2,sticky="wne"))
  
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))

  Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
  
  Try(NumContrasts <- length(ContrastNamesVec))
  
  coefIndexList <- list()

  if (NumContrasts>0)
    Try(for (i in (1:NumContrasts))     
      Try(tkinsert(tl,"end",ContrastNamesVec[i])))

  Try(tkselection.set(tl,0))

  Try(ReturnVal <- list(coefIndex=0)) # Other attributes can be added later if necessary.
  onOK <- function()
  {
      Try(contrastNum <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttGetContrast));Try(tkdestroy(ttGetContrast));Try(tkfocus(ttMain))
        Try(ReturnVal <<- list(contrastIndex=contrastNum))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetContrast));Try(tkdestroy(ttGetContrast));Try(tkfocus(ttMain));Try(ReturnVal <<- list(contrastIndex=0))}
  Try(OK.but <-tkbutton(ttGetContrast,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetContrast,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetContrast,text="    ")))
  Try(tkgrid(tklabel(ttGetContrast,text="    "),tklabel(ttGetContrast,text="    "),OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))  
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))      
  Try(tkgrid(tklabel(ttGetContrast,text="    ")))
  Try(tkfocus(ttGetContrast))
  Try(tkbind(ttGetContrast, "<Destroy>", function() {Try(tkgrab.release(ttGetContrast));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGetContrast))

  return (ReturnVal)
}

showTopTable <- function()
{
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment)) 
  
  Try(if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Top Table",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  })
  
  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Top Table",message="There are no contrast parameterizations available.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })  
  Try(contrastParameterizationIndex <- ChooseContrastParameterization())
  Try(if (contrastParameterizationIndex==0) return()) # Cancel
  
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
  Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))

  Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
  Try(if (GetContrastReturnVal$contrastIndex==0) return()) # Cancel
  Try(contrast <- GetContrastReturnVal$contrastIndex)
  Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))

  Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
#  Try(eb  <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$eb)

	Try(if (("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
										length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0)
		Try(ebayesAvailable <- TRUE)
	else
		Try(ebayesAvailable <- FALSE))

  # This is a bit silly, calculating it again.  This should be tidied up later.  But basically, we're
  # checking whether we had degrees of freedom > 0 from the linear model fit (i.e. were there any
  # replicate arrays?)  If so, eBayes should work, and we can use Gordon's new method (adding new
  # attributes to the fit object rather than using eb), because this seems to work best with topTable,
  # affy data etc.
  Try(if (ebayesAvailable==TRUE)
    Try(fit <- eBayes(fit)))
  
  Try(ttToptableDialog<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttToptableDialog))
  Try(tkgrab.set(ttToptableDialog))
  Try(tkfocus(ttToptableDialog))
  Try(tkwm.title(ttToptableDialog,"Toptable Options"))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))
    
  Try(frame1 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
  Try(HowManyQuestion1 <- tklabel(frame1,text=
    "Number of genes in table:",font=affylmGUIfont2))
  Try(tkgrid(HowManyQuestion1))
  Try(tkgrid.configure(HowManyQuestion1,columnspan=2,sticky="w"))
  
  Try(numberOfGenesTcl <- tclVar("3"))
  Try(Ten.but      <- tkradiobutton(frame1,text="10",variable=numberOfGenesTcl,value="1",font=affylmGUIfont2))
  Try(Thirty.but   <- tkradiobutton(frame1,text="30",variable=numberOfGenesTcl,value="2",font=affylmGUIfont2))
  Try(Fifty.but    <- tkradiobutton(frame1,text="50",variable=numberOfGenesTcl,value="3",font=affylmGUIfont2))  
  Try(Hundred.but  <- tkradiobutton(frame1,text="100",variable=numberOfGenesTcl,value="4",font=affylmGUIfont2))    
  Try(AllGenes.but <- tkradiobutton(frame1,text="All genes",variable=numberOfGenesTcl,value="5",font=affylmGUIfont2))      
  
  Try(tkgrid(Ten.but,sticky="w"))
  Try(tkgrid(Thirty.but,sticky="w"))
  Try(tkgrid(Fifty.but,sticky="w"))
  Try(tkgrid(Hundred.but,sticky="w"))  
  Try(tkgrid(AllGenes.but,sticky="w"))  
  Try(tkgrid.configure(HowManyQuestion1,Ten.but,Thirty.but,Fifty.but,Hundred.but,AllGenes.but,sticky="w"))

  Try(frame2 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
  Try(sortByLabel <- tklabel(frame2,text="Sort by:",font=affylmGUIfont2))
  Try(tkgrid(sortByLabel,sticky="w"))  
  Try(tkgrid.configure(sortByLabel,sticky="w"))
  Try(if (ebayesAvailable==TRUE)
    Try(sortByTcl <- tclVar("B"))
  else
    Try(sortByTcl <- tclVar("M")))
    
  Try(M.but <- tkradiobutton(frame2,text="M",variable=sortByTcl,value="M",font=affylmGUIfont2))
  Try(A.but <- tkradiobutton(frame2,text="A",variable=sortByTcl,value="A",font=affylmGUIfont2))
  Try(T.but <- tkradiobutton(frame2,text="t statistic",variable=sortByTcl,value="T",font=affylmGUIfont2))  
  Try(P.but <- tkradiobutton(frame2,text="P value",variable=sortByTcl,value="P",font=affylmGUIfont2))    
  Try(B.but <- tkradiobutton(frame2,text="B statistic",variable=sortByTcl,value="B",font=affylmGUIfont2))      
  
  Try(tkgrid(M.but,sticky="w"))
  Try(tkgrid(A.but,sticky="w"))
  Try(tkgrid(T.but,sticky="w"))
  Try(tkgrid(P.but,sticky="w"))  
  Try(tkgrid(B.but,sticky="w"))

  Try(if (ebayesAvailable==FALSE)
  {
    Try(tkconfigure(T.but,state="disabled"))
    Try(tkconfigure(P.but,state="disabled"))
    Try(tkconfigure(B.but,state="disabled"))
  })

  Try(frame3 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
  Try(adjustMethodLabel <- tklabel(frame3,text="Adjust method:",font=affylmGUIfont2))
  Try(tkgrid(adjustMethodLabel,sticky="w"))
  Try(tkgrid.configure(adjustMethodLabel,sticky="w"))  
  Try(if (ebayesAvailable==TRUE)
    Try(adjustMethodTcl <- tclVar("holm"))
  else
     Try(adjustMethodTcl <- tclVar("none")))
  Try(bonferroni.but <- tkradiobutton(frame3,text="Bonferroni",variable=adjustMethodTcl,value="bonferroni",font=affylmGUIfont2))
  Try(holm.but <- tkradiobutton(frame3,text="Holm",variable=adjustMethodTcl,value="holm",font=affylmGUIfont2))
  Try(hochberg.but <- tkradiobutton(frame3,text="Hochberg",variable=adjustMethodTcl,value="hochberg",font=affylmGUIfont2))  
  Try(hommel.but <- tkradiobutton(frame3,text="Hommel",variable=adjustMethodTcl,value="hommel",font=affylmGUIfont2))    
  Try(fdr.but <- tkradiobutton(frame3,text="FDR",variable=adjustMethodTcl,value="fdr",font=affylmGUIfont2))      
  Try(none.but <- tkradiobutton(frame3,text="None",variable=adjustMethodTcl,value="none",font=affylmGUIfont2))      

  Try(tkgrid(bonferroni.but,sticky="w"))
  Try(tkgrid(holm.but,sticky="w"))
  Try(tkgrid(hochberg.but,sticky="w"))
  Try(tkgrid(hommel.but,sticky="w"))  
  Try(tkgrid(fdr.but,sticky="w"))
  Try(tkgrid(none.but,sticky="w"))  

  Try(if (ebayesAvailable==FALSE)
  {
    Try(tkconfigure(bonferroni.but,state="disabled"))
    Try(tkconfigure(holm.but,state="disabled"))
    Try(tkconfigure(hochberg.but,state="disabled"))
    Try(tkconfigure(hommel.but,state="disabled"))    
    Try(tkconfigure(fdr.but,state="disabled"))        
    Try(tkconfigure(none.but,state="disabled"))            
  })

  Try(totalGenes <- nrow(fit$coefficients))
  Try(Abort <- 1)
  Try(numberOfGenes <- 0)
  Try(sortBy <- "B")
  Try(adjustMethod <- "holm")
  Try(onOK <- function()
  {     
      Try(NumGenesChoice <- as.numeric(tclvalue(numberOfGenesTcl)))
      Try(tkgrab.release(ttToptableDialog))
      Try(tkdestroy(ttToptableDialog))
      Try(tkfocus(ttMain))
      NumbersOfGenes <- c(10,30,50,100,totalGenes)
      numberOfGenes <<- NumbersOfGenes[NumGenesChoice]      
      sortBy <<- tclvalue(sortByTcl)
      adjustMethod <<- tclvalue(adjustMethodTcl)      
      Abort <<- 0
  })

  Try(frame4 <- tkframe(ttToptableDialog,borderwidth=2))
  Try(onCancel <- function() {Try(tkgrab.release(ttToptableDialog));Try(tkdestroy(ttToptableDialog));Try(tkfocus(ttMain));Abort <<- 1})
  Try(OK.but <-tkbutton(frame4,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(frame4,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  
  Try(tkgrid(tklabel(frame4,text="    "),OK.but,Cancel.but,tklabel(frame4,text="    ")))

  Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame1,frame2,tklabel(ttToptableDialog,text="  ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame3,frame4,tklabel(ttToptableDialog,text="  ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))  
  Try(tkgrid.configure(frame1,frame3,sticky="w"))
#  Try(tkgrid.configure(frame4,sticky="s"))
  
  Try(tkfocus(ttToptableDialog))
  Try(tkbind(ttToptableDialog, "<Destroy>", function() {Try(tkgrab.release(ttToptableDialog));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttToptableDialog))
    
  Try(if (Abort==1) 
     return())
  
  Try(if (numberOfGenes==totalGenes) 
  {
      tkconfigure(ttMain,cursor="watch")
      Try(tkfocus(ttMain))
  })

  Try(options(digits=3))
    
  Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
  Try(cdfenv<-getCdfInfo(RawAffyData))

  Try(genelist <- data.frame(ID=I(ls(cdfenv))))

  Try(geneNames <- get("geneNames",envir=affylmGUIenvironment))
  Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))  
  Try(if (length(geneNames)==0|| length(geneSymbols)==0)
  {
    Try(tkconfigure(ttMain,cursor="watch"))  
    Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
    Try(dataName <- strsplit(cleancdfname(RawAffyData@cdfName),"cdf")[[1]])
    Require("reposTools")
    Try(annoPackages <- getReposEntry("http://www.bioconductor.org/data/metaData"))    
    Try(matchIndex <- match(dataName,annoPackages@repdatadesc@repdatadesc[,"Package"]))
    Try(if (!is.na(matchIndex))
    {
      Try(install.packages2(dataName,annoPackages))
      Require(dataName)
      Try(code2eval <- paste("Try(geneNames <- as.character(unlist(multiget(ls(envir=",dataName,"GENENAME),env=",dataName,"GENENAME))))",sep=""))
      Try(eval(parse(text=code2eval)))
      Try(assign("geneNames",geneNames,affylmGUIenvironment))
      Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(multiget(ls(envir=",dataName,"SYMBOL),env=",dataName,"SYMBOL))))",sep=""))
      Try(eval(parse(text=code2eval)))
      Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))      
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
      Try(colnames(genelist) <- c("ID","Symbol","Name"))
    }
    else
    {
      Try(genelist <- data.frame(ID=I(ls(cdfenv))))
      Try(tkconfigure(ttMain,cursor="arrow"))
    })
    
  }
  else
  {
    Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
    Try(colnames(genelist) <- c("ID","Symbol","Name"))
  })

   Try(fit$genes <- genelist)
   Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
	 Try(if (!("Amean" %in% names(fit)))
	    fit$Amean <- rowMeans(exprs(NormalizedAffyData)))
	    
	    
	# Note that it is difficult to use the limma toptable/topTable functions if you don't have ebayes statistics, so
	# in the case of no replicate arrays (no residual degrees of freedom) we will just do our own sorting.
	
  Try(if (ebayesAvailable==FALSE)
  {
	  Try(M <- as.matrix(fit$coef)[,contrast])
	  Try(A <- fit$Amean)
	  Try(ord <- switch(sortBy, M = order(abs(M), decreasing = TRUE), A = order(A, decreasing = TRUE)))
	  Try(top <- ord[1:numberOfGenes])
    Try(table1 <- data.frame(genelist[top, ,drop=FALSE], M = M[top], A=A[top]))
    Try(rownames(table1) <- as.character(1:length(M))[top])
  })
	    
# The 2's in front of toptables mean that they use the drop=FALSE option (even if the user hasn't upgraded limma since the 1.3 BioC release.)
#  Try(table1 <- toptable2(coef=contrast,number=numberOfGenes,fit=fit,eb=eb,genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy))
  Try(if (ebayesAvailable==TRUE)
    Try(table1 <- topTable2(coef=contrast,number=numberOfGenes,fit=fit,genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy)))
#  Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))

  Try(nrows <- nrow(table1))
  Try(ncols <- ncol(table1))  
  
  Try(if (nrows <=100)
  {
    Try(ttToptableTable <- tktoplevel(ttMain))
    Try(tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for",ContrastNamesVec[contrast],".",sep=" ")))
    TclRequire("Tktable") 
    Try(toptableTable <- tkwidget(ttToptableTable,"table",
           xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),
           rows=nrows+1,cols=ncols,titlerows=1,
           width=ncols,selectmode="extended",colwidth="13",background="white",
           rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0",state="disabled",
           font=affylmGUIfontTopTable))
    Try(xscr <-tkscrollbar(ttToptableTable,orient="horizontal", command=function(...)tkxview(toptableTable,...)))
    Try(yscr <- tkscrollbar(ttToptableTable,command=function(...)tkyview(toptableTable,...)))
    Try(tclArrayVar1 <- tclArrayVar())
    Try(tclArrayName <- ls(tclArrayVar1$env))
    Try(tkcmd("set",paste(tclArrayName,"0,0",sep=""),""))
    Try(for (j in (1:ncols))
        Try(tkcmd("set",paste(tclArrayName,"(",0,",",j-1,")",sep=""),colnames(table1)[j]))  )
    Try(for (i in (1:nrows))
      for (j in (1:ncols))
      {
        Try(if (is.numeric(table1[i,j]))
          item <- format(table1[i,j],digits=4)
        else
          item <- table1[i,j])
        Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(item)))        
      })
    Try(tkgrid(toptableTable,yscr))
    Try(tkgrid.configure(toptableTable,sticky="news"))
    Try(tkgrid.configure(yscr,sticky="nsw"))
    Try(tkgrid(xscr,sticky="new"))
    Try(tkconfigure(toptableTable,bg="white",variable=tclArrayName))
    Try(for (i in (1:ncols))
    {
      if (tolower(colnames(table1)[i]) %in% c("block","row","column","gridrow","gridcolumn","gridcol","grid.row","grid.col","grid.column"))
      {
        Try(if (affylmGUIpresentation==FALSE)
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i])+2))))
        else
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i]))))))        
        next()
      }
      if (colnames(table1)[i] %in% c("M","A","t","B"))
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),"6"))
        next()
      }
      if (colnames(table1)[i] == "P.Value")
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),"8"))
        next()
      }     
      if(tolower(colnames(table1)[i]) == "name")
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(30)))
        next()
      }

      if(tolower(colnames(table1)[i]) == "id")
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),40))))
        next()
      }

      if(tolower(colnames(table1)[i]) == "symbol")
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),30))))
        next()
      }

      Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(min(max(4,max(nchar(as.character(table1[,i])))+2),40))))
    })
    Try(tkfocus(toptableTable))
  }
  else
  {
    Try(tkmessageBox(title="Large Toptable",message="Toptable is too big to display in a table widget, so it will be displayed in a text window instead.  You can save it as a tab-delimited text file and then import it into a spreadsheet program.",icon="info",type="ok"))

    write.table(table1,file="temp5317",quote=FALSE,col.names=NA,sep="\t")
    ttToptableTable <- tktoplevel(ttMain)
    tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for",ContrastNamesVec[contrast],".",sep=" "));

    xscr <-tkscrollbar(ttToptableTable, repeatinterval=5,orient="horizontal",
                         command=function(...)tkxview(txt,...))
    scr <- tkscrollbar(ttToptableTable, repeatinterval=5,
                         command=function(...)tkyview(txt,...))
    txt <- tktext(ttToptableTable, bg="white", font="courier",xscrollcommand=function(...)tkset(xscr,...),
        yscrollcommand=function(...)tkset(scr,...),wrap="none",width=100)

    copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>")))

    editPopupMenu2 <- tkmenu(txt, tearoff=FALSE)
    tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",
    command=copyText2) # ,font=affylmGUIfontMenu

    RightClick2 <- function(x,y) # x and y are the mouse coordinates
    {
     rootx <- as.integer(tkwinfo("rootx",txt))
     rooty <- as.integer(tkwinfo("rooty",txt))
     xTxt <- as.integer(x)+rootx
     yTxt <- as.integer(y)+rooty
     .Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt)))
    }
    tkbind(txt, "<Button-3>",RightClick2)            


    tkpack(scr, side="right", fill="y")
    tkpack(xscr, side="bottom", fill="x")
    tkpack(txt, side="left", fill="both", expand="yes")

    chn <- tclvalue(tkcmd("open", file.path(".","temp5317")))
    tkinsert(txt, "end", tclvalue(tkcmd("read", chn)))
    tkcmd("close", chn)
    tkconfigure(txt, state="disabled")
    tkmark.set(txt,"insert","0.0")
    tkfocus(txt)

    tkconfigure(ttMain,cursor="arrow")
        
  })
  
  
  SaveTopTable <- function()
  {
    Try(tmpTopTableFile <- tclvalue(tkgetSaveFile(initialfile=paste("toptable", contrast,".txt",sep=""))))
    Try(if (!nchar(tmpTopTableFile))
        return())
    Try(TopTableFile <- tmpTopTableFile)
    Try(write.table(table1,file=TopTableFile,quote=FALSE,col.names=NA,sep="\t"))
  } 
  
  Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(toptableTable),"<<Copy>>"))))
  
  topMenu2 <- tkmenu(ttToptableTable)
  tkconfigure(ttToptableTable, menu=topMenu2)
  fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
  tkadd(fileMenu2, "command", label="Save As",
  command=SaveTopTable) # ) # ,font=affylmGUIfontMenu)
  tkadd(fileMenu2, "command", label="Close",
  command=function() tkdestroy(ttToptableTable)) # ) # ,font=affylmGUIfontMenu)
  tkadd(topMenu2, "cascade", label="File",
  menu=fileMenu2) # ,font=affylmGUIfontMenu)
  editMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
  tkadd(editMenu2, "command", label="Copy <Ctrl-C>",
  command=copyFcn) # ,font=affylmGUIfontMenu)
  tkadd(topMenu2, "cascade", label="Edit",
  menu=editMenu2) # ,font=affylmGUIfontMenu)
}


GetSlideNum <- function()
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=affylmGUIenvironment))  
  Try(if (min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE)
    SlideNamesVec <- paste("Slide",SlideNamesVec))  
  Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
  ttGetSlideNum<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetSlideNum)
  tkgrab.set(ttGetSlideNum)
  tkfocus(ttGetSlideNum)
  tkwm.title(ttGetSlideNum,"Please Specify Slide")
  scr <- tkscrollbar(ttGetSlideNum, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttGetSlideNum,height=4,selectmode="browse",yscrollcommand=function(...)tkset(scr,...),background="white",font=affylmGUIfont2) 
  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
       tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  lbl2<-tklabel(ttGetSlideNum,text="Choose a slide",font=affylmGUIfont2)
  tkgrid(tklabel(ttGetSlideNum,text="    "),lbl2,tklabel(ttGetSlideNum,text="    "),
     tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttGetSlideNum,text="    "),row=2,column=0)
  tkgrid(tl,row=2,column=1,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=3,rowspan=4,sticky="wns")
  tkgrid(tklabel(ttGetSlideNum,text="    "),row=2,column=4)
  for (i in (1:NumSlides))
    tkinsert(tl,"end",SlideNamesVec[i])
  tkselection.set(tl,0)

  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
     tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  ReturnVal <- 0
  onOK <- function()
  {
      slidenum <- as.numeric(tclvalue(tkcurselection(tl)))+1
      Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(ttMain))
      ReturnVal <<- slidenum
  }
  onCancel <- function() {Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(ttMain)); ReturnVal <<- 0}      
  OK.but <-tkbutton(ttGetSlideNum,text="   OK   ",command=onOK,font=affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetSlideNum,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
  tkgrid(tklabel(ttGetSlideNum,text="    "),OK.but,Cancel.but,tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  tkgrid.configure(OK.but,Cancel.but,sticky="w")
  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
       tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))      
  Try(tkfocus(tl))
  Try(tkbind(ttGetSlideNum, "<Destroy>", function() {Try(tkgrab.release(ttGetSlideNum));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGetSlideNum))

  return (ReturnVal)
}


GetDEcutoff <- function()
{
  ttGetDEcutoff<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetDEcutoff)
  tkgrab.set(ttGetDEcutoff)  
  Try(tkwm.title(ttGetDEcutoff,"Cutoff for Differentially Expressed Genes"))
  Try(cutoffStatisticTcl <- tclVar("abs(t)"))
  Try(tkframe1 <- tkframe(ttGetDEcutoff,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a cutoff for differentially expressed genes.",font=affylmGUIfont2),rowspan=1,columnspan=2,sticky="w"))

  Try(tStatistic.but <- tkradiobutton(tkframe2,text="Abs(t)",variable=cutoffStatisticTcl,value="abs(t)",font=affylmGUIfont2))
  Try(BStatistic.but <- tkradiobutton(tkframe2,text="B",variable=cutoffStatisticTcl,value="B",font=affylmGUIfont2))

  Try(tkgrid(tStatistic.but))
  Try(tkgrid(BStatistic.but))
  Try(tkgrid.configure(tStatistic.but,BStatistic.but,sticky="w"))
  Try(tkgrid(tklabel(tkframe2,text="    ")))
  Try(cutoffValueTcl <- tclVar("0"))
  Try(entry.cutoffValue<-tkentry(tkframe2,width=30,font=affylmGUIfont2,textvariable=cutoffValueTcl,bg="white"))
  Try(tkgrid(tklabel(tkframe2,text="Cutoff value ",font=affylmGUIfont2),entry.cutoffValue,sticky="w"))
  
  Try(tkgrid(tkframe2))
  Try(ReturnVal <- list())
  onOK <- function()
  {
      Try(cutoffStatisticVal <- as.character(tclvalue(cutoffStatisticTcl)))
      Try(cutoffValue <- as.numeric(tclvalue(cutoffValueTcl)))
      Try(tkgrab.release(ttGetDEcutoff));Try(tkdestroy(ttGetDEcutoff));Try(tkfocus(ttMain))
      Try(ReturnVal <<- list(cutoffStatistic=cutoffStatisticVal,cutoff=cutoffValue))
  }
  onCancel <- function(){tkgrab.release(ttGetDEcutoff);tkdestroy(ttGetDEcutoff);tkfocus(ttMain);ReturnVal <<- list()} 
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(ttGetDEcutoff))
  Try(tkbind(ttGetDEcutoff, "<Destroy>", function(){tkgrab.release(ttGetDEcutoff);tkfocus(ttMain);} )) 
  Try(tkwait.window(ttGetDEcutoff))

  return (ReturnVal)
}

ChooseEbayesStatistic <- function()
{
  ttChooseEbayesStatistic<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseEbayesStatistic)
  tkgrab.set(ttChooseEbayesStatistic)
  tkfocus(ttChooseEbayesStatistic)
  tkwm.title(ttChooseEbayesStatistic,"Empirical Bayes Statistic")   
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    ")))

  Try(EbayesStatisticTcl <- tclVar("t"))

  Try(tStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="t"))
  Try(BStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="lods"))  
  Try(PValueRadioButton     <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="p.value"))    
       
  Try(lbl2 <- tklabel(ttChooseEbayesStatistic,text="Please Choose an Empirical Bayes Statistic",font=affylmGUIfont2))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),lbl2)
  Try(tkgrid.configure(lbl2,columnspan=2,sticky="w"))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="t Statistic",font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tStatisticRadioButton,currentLabel))
  Try(tkgrid.configure(tStatisticRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="B Statistic",font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),BStatisticRadioButton,currentLabel))
  Try(tkgrid.configure(BStatisticRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="P Value",font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),PValueRadioButton,currentLabel))
  Try(tkgrid.configure(PValueRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  ReturnVal <- ""
  onOK <- function()
  {      
      Try(ReturnVal <- tclvalue(EbayesStatisticTcl))
      Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkdestroy(ttChooseEbayesStatistic));Try(tkfocus(ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkdestroy(ttChooseEbayesStatistic));Try(tkfocus(ttMain)); ReturnVal <<- ""}      
  OK.but <-tkbutton(ttChooseEbayesStatistic,text="   OK   ",command=onOK,font=affylmGUIfont2)
  Cancel.but <-tkbutton(ttChooseEbayesStatistic,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),OK.but,Cancel.but,tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "))
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "),
       tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "))
  Try(tkfocus(ttChooseEbayesStatistic))
  Try(tkbind(ttChooseEbayesStatistic, "<Destroy>", function() {Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttChooseEbayesStatistic))

  return (ReturnVal)

}

GetWtAreaParams <- function()
{
    ttWeightingwtArea <- tktoplevel(ttMain)
    tkwm.deiconify(ttWeightingwtArea)
    tkgrab.set(ttWeightingwtArea)
    tkfocus(ttWeightingwtArea)
    tkwm.title(ttWeightingwtArea,"Good Spot Size")
    tkframe1 <- tkframe(ttWeightingwtArea)
    tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
    tkframe4 <- tkframe(tkframe1)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe1,text="Please enter the area range for good spots",font=affylmGUIfont2),columnspan=2)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe2,text="Area Range in Pixels",font=affylmGUIfont2),columnspan=2)
    AreaLowerLimitTcl <- tclVar(paste(160))
    AreaUpperLimitTcl <- tclVar(paste(170))
    tkgrid(tklabel(tkframe2,text="    "))
    entry.AreaLowerLimit <-tkentry(tkframe2,width="12",font=affylmGUIfont2,textvariable=AreaLowerLimitTcl,bg="white")
    entry.AreaUpperLimit <-tkentry(tkframe2,width="12",font=affylmGUIfont2,textvariable=AreaUpperLimitTcl,bg="white")
    tkgrid(tklabel(tkframe2,text="Lower area limit in pixels",font=affylmGUIfont2),entry.AreaLowerLimit,sticky="w")
    tkgrid(tklabel(tkframe2,text="Upper area limit in pixels",font=affylmGUIfont2),entry.AreaUpperLimit,sticky="w")
    tkgrid(tkframe2)
    tkgrid(tklabel(tkframe1,text="    "))
    ReturnVal <- 0
    AreaLowerLimitVal <- 0
    AreaUpperLimitVal <- 0
    onOK <- function()
    {
        Try(AreaLowerLimitVal <<- as.integer(tclvalue(AreaLowerLimitTcl)))
        Try(AreaUpperLimitVal <<- as.integer(tclvalue(AreaUpperLimitTcl)))
        Try(assign("AreaLowerLimit",AreaLowerLimitVal,affylmGUIenvironment))
        Try(assign("AreaUpperLimit",AreaUpperLimitVal,affylmGUIenvironment))
        Try(assign("WeightingType",paste("wtarea, Ideal=(",AreaLowerLimitVal,",",AreaUpperLimitVal,")",sep=""),affylmGUIenvironment))        
        Try(tkgrab.release(ttWeightingwtArea));Try(tkdestroy(ttWeightingwtArea));Try(tkfocus(ttMain))
        ReturnVal <<- 1
    }
    onCancel <- function() {Try(tkgrab.release(ttWeightingwtArea));Try(tkdestroy(ttWeightingwtArea));Try(tkfocus(ttMain));ReturnVal<<-0}   
    OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=affylmGUIfont2)   
    Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
    tkgrid(OK.but,Cancel.but)
    tkgrid(tklabel(tkframe4,text="    "))
    tkgrid(tkframe4)
    tkgrid(tkframe1)
    Try(tkfocus(ttWeightingwtArea))
    Try(tkbind(ttWeightingwtArea, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtArea));Try(tkfocus(ttMain));}))
    Try(tkwait.window(ttWeightingwtArea))

    return (ReturnVal)
}

GetWtFlagParams <- function()
{
    ttWeightingwtFlag <- tktoplevel(ttMain)
    tkwm.deiconify(ttWeightingwtFlag)
    tkgrab.set(ttWeightingwtFlag)
    tkfocus(ttWeightingwtFlag)  
    tkwm.title(ttWeightingwtFlag,"Weighting for Spots with Flag Values Less Than Zero")
    tkframe1 <- tkframe(ttWeightingwtFlag)
    tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
    tkframe4 <- tkframe(tkframe1)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe1,text="Please enter the weighting for spots with flag values less than zero",font=affylmGUIfont2),columnspan=2)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe2,text="Spot Weighting",font=affylmGUIfont2),columnspan=2)
    FlagSpotWeightingTcl <- tclVar(paste(0.1))
    tkgrid(tklabel(tkframe2,text="    "))
    entry.FlagSpotWeighting<-tkentry(tkframe2,width="12",font=affylmGUIfont2,textvariable=FlagSpotWeightingTcl,bg="white")
    tkgrid(tklabel(tkframe2,text="Weighting (relative to 1 for all other spots)",font=affylmGUIfont2),entry.FlagSpotWeighting,sticky="w")
    tkgrid(tkframe2)
    tkgrid(tklabel(tkframe1,text="    "))
    ReturnVal <- 0
    FlagSpotWeightingVal <- 0
    onOK <- function()
    {
        Try(FlagSpotWeightingVal <- as.numeric(tclvalue(FlagSpotWeightingTcl)))
        Try(tkgrab.release(ttWeightingwtFlag));Try(tkdestroy(ttWeightingwtFlag));Try(tkfocus(ttMain))
        ReturnVal <<- 1
    }
    onCancel <- function() {Try(tkgrab.release(ttWeightingwtFlag));Try(tkdestroy(ttWeightingwtFlag));Try(tkfocus(ttMain));ReturnVal<<-0}   
    OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=affylmGUIfont2)
    Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
    tkgrid(OK.but,Cancel.but)
    tkgrid(tklabel(tkframe4,text="    "))
    tkgrid(tkframe4)
    tkgrid(tkframe1)
    Try(tkfocus(ttWeightingwtFlag))
    Try(tkbind(ttWeightingwtFlag, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtFlag));Try(tkfocus(ttMain));}))
    Try(tkwait.window(ttWeightingwtFlag))
    
    Try(FlagSpotWeighting <- FlagSpotWeightingVal)
    Try(assign("FlagSpotWeighting", FlagSpotWeighting,affylmGUIenvironment))

    Try(assign("WeightingType",paste("wtflag, FlagSpotWeighting = ",FlagSpotWeighting,sep=""),affylmGUIenvironment))

    return (ReturnVal)
}

evalRcode <- function()
{
  Try(wfile <- "")
  Try(ttEvalRcode <- tktoplevel(ttMain))
  Try(tkwm.title(ttEvalRcode ,"Enter R code in this window and then click on Run"))
  Try(scrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,
                         command=function(...)tkyview(txt,...)))
  Try(xscrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,orient="horizontal",
                         command=function(...)tkxview(txt,...)))
  Try(txt <- tktext(ttEvalRcode , height=20,
            yscrollcommand=function(...)tkset(scrCode,...),
            xscrollcommand=function(...)tkset(xscrCode,...),
            wrap="none",width=100,bg="white",
              font=affylmGUIfontCourier))
  Try(cutText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Cut>>")))    )
  Try(copyText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))
  Try(pasteText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Paste>>")))  )

  Try(editPopupMenu <- tkmenu(txt, tearoff=FALSE))
  Try(tkadd(editPopupMenu, "command", label="Cut <Ctrl-X>",
  command=cutText)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>",
  command=copyText)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(editPopupMenu, "command", label="Paste <Ctrl-V>",
  command=pasteText)) # ) # ,font=affylmGUIfontMenu))

  RightClick <- function(x,y) # x and y are the mouse coordinates
  {
   Try(rootx <- as.integer(tkwinfo("rootx",txt)))
   Try(rooty <- as.integer(tkwinfo("rooty",txt)))
   Try(xTxt <- as.integer(x)+rootx)
   Try(yTxt <- as.integer(y)+rooty)
   Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu,xTxt,yTxt))))
  }
  Try(tkbind(txt, "<Button-3>",RightClick))
  
  Try(tkpack(scrCode, side="right", fill="y"))
  Try(tkpack(xscrCode, side="bottom", fill="x"))
  Try(tkpack(txt, side="left", fill="both", expand="yes"))
  Try(tkfocus(txt))

  SaveRSourceFile <- function() 
  {
    Try(fileName <- tclvalue(tkgetSaveFile(initialfile=tclvalue(tkfile.tail(wfile)),initialdir=tclvalue(tkfile.dir(wfile)),
           filetypes="{{R Source Files} {.R}} {{All files} *}")))
    if (nchar(fileName)==0) return()
    Try(len <- nchar(fileName))
    if (len<=2)
      Try( fileName <- paste(fileName,".R",sep=""))
    else if (substring(fileName,len-1,len)!=".R")
      Try(fileName <- paste(fileName,".R",sep=""))
    Try(chn <- tkopen(fileName, "w"))
    Try(tkputs(chn, tclvalue(tkget(txt,"0.0","end"))))
    Try(tkclose(chn))
    Try(wfile <<- fileName)
    Try(tkfocus(txt))
  }

  OpenRSourceFile <- function() 
  {
    Try(fileName <- tclvalue(tkgetOpenFile(filetypes="{{R Source Files} {.R}} {{All files} *}")))
    if (nchar(fileName)==0) return()
    Try(chn <- tkopen(fileName, "r"))
    Try(tkinsert(txt, "0.0", tclvalue(tkread(chn))))
    Try(tkclose(chn))
    Try(wfile <<- fileName)
    Try(tkfocus(txt))
  }

  runOverall <- function(runType) {
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkconfigure(ttEvalRcode,cursor="watch"))
  Try(tkfocus(ttEvalRcode))
  Try(code <- tclvalue(tkget(txt,"0.0","end")))
  if (runType!="runTextOnly")
  {
     Try(LocalHScale <- get("Myhscale",envir=.GlobalEnv))
     Try(LocalVScale <- get("Myvscale",envir=.GlobalEnv))   

    Try(ttGraph<-tktoplevel(ttMain))
    Try(tkwm.withdraw(ttGraph))
    Try(tkwm.title(ttGraph,"Graphical Results from R Code Evaluation"))
    Try(codeGraph <- paste("assign(\"plotFunction\",function () {\nopar<-par(bg=\"white\")\nTry({\n",code,"\n})\n\ntempGraphPar <- par(opar)\n},affylmGUIenvironment)\n",sep=""))
  }

  if (runType!="runGraphicsOnly")
  {
    Try(RoutFileObject <- file("tmpEvalRcodeResults", open="wt"))
    Try(sink(RoutFileObject))
    Try(sink(RoutFileObject,type="message"))
    Try(e <- try(parse(text=code)))
    if (inherits(e, "try-error")) 
    {
      Try(tkmessageBox(message="Syntax error",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(try(close(RoutFileObject),TRUE))
      return()
    }
    e2 <- try(print(eval(e,envir=affylmGUIenvironment)))
    if (inherits(e2, "try-error"))
    {
      Try(tkmessageBox(title="An error occured while trying to evaluate your R code",message=as.character(e2),icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(try(close(RoutFileObject),TRUE))
      return()      
    }
    Try(sink(type="message"))
    Try(sink())
    Try(try(close(RoutFileObject),TRUE))
  }

  if (runType!="runTextOnly")
  {
    Try(RoutFileObjectGraph <- file("tmpEvalRcodeResultsGraph",open="wt"))
    Try(sink(RoutFileObjectGraph))
    Try(sink(RoutFileObjectGraph,type="message"))
    Try(e3 <- try(parse(text=codeGraph)))
    if (inherits(e3, "try-error")) 
    {
      Try(tkmessageBox(message="Syntax error",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(close(RoutFileObjectGraph))
      return()
    }
    e4 <- try(print(eval(e3,envir=affylmGUIenvironment)))
    if (inherits(e4, "try-error"))
    {
      Try(tkmessageBox(message="An error occured while trying to plot the graph(s) for your R code",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(close(RoutFileObjectGraph))
      return()            
    }
    Try(sink(type="message"))
    Try(sink())
    Try(try(close(RoutFileObjectGraph),TRUE))
    Require("tkrplot")

    Try(plotFunction <- get("plotFunction",envir=affylmGUIenvironment))
    Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
    SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)  

    Try(tkgrid(imgaffylmGUI))
    Try(if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
      Try(tkdestroy(ttGraph))
    else
    {
      Try(tkwm.deiconify(ttGraph))
      Try(tkfocus(imgaffylmGUI))   
    })
    
    CopyToClip <- function()
    {
      Try(tkrreplot(imgaffylmGUI))
    }
  }

  if (runType!="runGraphicsOnly") 
  {
    Try(tt2 <-tktoplevel(ttMain))
    Try(tkwm.title(tt2,"Text Results of R Code Evaluation"))
    Try(scr <- tkscrollbar(tt2, repeatinterval=5,
                         command=function(...)tkyview(txt2,...)))
    Try(xscr <- tkscrollbar(tt2, repeatinterval=5,orient="horizontal",
                           command=function(...)tkxview(txt2,...)))
    Try(txt2 <- tktext(tt2,height=20,bg="white",
    yscrollcommand=function(...)tkset(scr,...),
    xscrollcommand=function(...)tkset(xscr,...),
    wrap="none",width=100,font=affylmGUIfontCourier))

    Try(copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt2),"<<Copy>>"))))

    Try(editPopupMenu2 <- tkmenu(txt2, tearoff=FALSE))
    Try(tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",
    command=copyText2)) # ) # ,font=affylmGUIfontMenu))

    RightClick2 <- function(x,y) # x and y are the mouse coordinates
    {
     Try(rootx <- as.integer(tkwinfo("rootx",txt2)))
     Try(rooty <- as.integer(tkwinfo("rooty",txt2)))
     Try(xTxt <- as.integer(x)+rootx)
     Try(yTxt <- as.integer(y)+rooty)
     Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt))))
    }
    Try(tkbind(txt2, "<Button-3>",RightClick2))

    Try(tkpack(scr, side="right", fill="y"))
    Try(tkpack(xscr, side="bottom", fill="x"))
    Try(tkpack(txt2, side="left", fill="both", expand="yes"))

    Try(chn <- tkopen("tmpEvalRcodeResults", "r"))
    Try(tkinsert(txt2, "0.0", tclvalue(tkread(chn))))
    Try(tkclose(chn))
    Try(tkfocus(tt2))
    SaveTextResults <- function()
    {
      Try(fileName<- tclvalue(tkgetSaveFile(initialfile="RcodeResults.txt",filetypes="{{Text Files} {.txt}} {{All files} *}")))
      Try(if (!nchar(fileName))        return())

      if (nchar(fileName)==0) return()
      Try(len <- nchar(fileName))
      if (len<=4)
        Try( fileName <- paste(fileName,".txt",sep=""))
      else if (substring(fileName,len-3,len)!=".txt")
      Try(fileName <- paste(fileName,".txt",sep=""))    
      Try(chn <- tkopen(fileName,"w"))
      Try(tkputs(chn, tclvalue(tkget(txt2,"0.0","end"))))
      Try(tkclose(chn))
    }

    Try(topMenu2 <- tkmenu(tt2))
    Try(tkconfigure(tt2, menu=topMenu2))
    Try(fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
    Try(editMenu2 <- tkmenu(topMenu2, tearoff=FALSE))

    Try(tkadd(fileMenu2, "command", label="Save As",
    command=SaveTextResults)) # ) # ,font=affylmGUIfontMenu))
    Try(tkadd(fileMenu2, "command", label="Close",
    command=function() tkdestroy(tt2))) # ) # ,font=affylmGUIfontMenu))
    Try(tkadd(topMenu2, "cascade", label="File",
    menu=fileMenu2)) # ) # ,font=affylmGUIfontMenu))

    Try(tkadd(editMenu2, "command", label="Copy <Ctrl-C>",
    command=copyText2)) # ) # ,font=affylmGUIfontMenu))
    Try(tkadd(topMenu2, "cascade", label="Edit",
    menu=editMenu2)) # ) # ,font=affylmGUIfontMenu))

  }
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(tkconfigure(ttEvalRcode,cursor="arrow"))
  }

  Try(runTextOnly <- function() runOverall("runTextOnly"))
  Try(runGraphicsOnly <- function() runOverall("runGraphicsOnly"))
  Try(runTextAndGraphics <- function() runOverall("runTextAndGraphics"))

  MakeaffylmGUIMenu <- function()
  {
    Try(code <- tclvalue(tkget(txt,"0.0","end")))
    Try(codeGraph <- paste("assign(\"plotFunction\",function () {\nopar<-par(bg=\"white\")\nTry({\n",code,"\n})\n\ntempGraphPar <- par(opar)\n},affylmGUIenvironment)\n",sep=""))
    Try(menuNameObject <- GetMenuName())
    Try(if (length(menuNameObject)==0) return())    
    Try(addMenuItem(codeGraph,menuNameObject$MenuName,newMenu=TRUE,menuPosition="end",
      menuNameObject$MenuItemName,newMenuItem=TRUE,menuItemPosition="end",
            outputHasGraphics=TRUE))  
  }

  Try(HTMLhelp <- function() help.start())

  Try(topMenu <- tkmenu(ttEvalRcode ))
  Try(tkconfigure(ttEvalRcode , menu=topMenu))
  Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(runMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(helpMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(tkadd(fileMenu, "command", label="Open",
  command=OpenRSourceFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As",
  command=SaveRSourceFile)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Close",
  command=function() tkdestroy(ttEvalRcode ))) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="File",
  menu=fileMenu)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Cut <Ctrl-X>",
  command=cutText)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",
  command=copyText)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Paste <Ctrl-V>",
  command=pasteText)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Edit",
  menu=editMenu)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Text Results only",
  command=runTextOnly)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Graphical Results only",
  command=runGraphicsOnly)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Text and Graphics",
  command=runTextAndGraphics)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Run",
  menu=runMenu)) # ) # ,font=affylmGUIfontMenu))
  
  Try(menuMenu <- tkmenu(topMenu,tearoff=FALSE))
  Try(tkadd(menuMenu,"command",label="Make affylmGUI Custom Menu Item",command=MakeaffylmGUIMenu))
  Try(tkadd(topMenu,"cascade",label="Make affylmGUI Menu Item",menu=menuMenu))
  
  Try(tkadd(helpMenu,"command",label="HTML Help",
  command=HTMLhelp)) # ) # ,font=affylmGUIfontMenu))
  Try(tkadd(topMenu,"cascade",label="Help",
  menu=helpMenu)) # ) # ,font=affylmGUIfontMenu))
}

OpenCDFandTargetsfiles <- function()
{
  Require("affy")
  Try(ttCDFandTargets<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttCDFandTargets))
  Try(tkgrab.set(ttCDFandTargets))
  Try(tkfocus(ttCDFandTargets))
#  Try(tkwm.title(ttCDFandTargets,"Open CDF and Targets files"))
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

  Try(OpenCDFFile.but <- tkbutton(ttCDFandTargets, text="Select CDF File",command=OpenCDFFile,font=affylmGUIfont2))
  Try(OpenTargetsFile.but <- tkbutton(ttCDFandTargets, text="Select Targets File",command=OpenTargetsFile,font=affylmGUIfont2))

  Try(CDFfileBoxTitleLabel<-tklabel(ttCDFandTargets,text=as.character(tclvalue(CDFfileBoxTitle)),font=affylmGUIfont2))
  Try(CDFfileNameLabel<-tklabel(ttCDFandTargets,text=as.character(tclvalue(CDFfileName)),background="white",font=affylmGUIfont2))
  Try(tkconfigure(CDFfileBoxTitleLabel, textvariable=CDFfileBoxTitle))
  Try(tkconfigure(CDFfileNameLabel, textvariable=CDFfileName))

#  Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
#  Try(tkgrid(CDFfileBoxTitleLabel,columnspan=4))
#  Try(tkgrid(CDFfileNameLabel,columnspan=4))

  Try(TargetsfileBoxTitleLabel <- tklabel(ttCDFandTargets,text=as.character(tclvalue(TargetsfileBoxTitle)),font=affylmGUIfont2))
  Try(TargetsfileNameLabel <- tklabel(ttCDFandTargets,text=as.character(tclvalue(TargetsfileName)),background="white",font=affylmGUIfont2))
  Try(tkconfigure(TargetsfileBoxTitleLabel, textvariable=TargetsfileBoxTitle))
  Try(tkconfigure(TargetsfileNameLabel, textvariable=TargetsfileName))

  Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
  Try(tkgrid(TargetsfileBoxTitleLabel,columnspan=4))
  Try(tkgrid(TargetsfileNameLabel,columnspan=4))

  Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
  
#  Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
#  Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OpenCDFFile.but, OpenTargetsFile.but))
  Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OpenTargetsFile.but))
  Try(tkgrid.configure(OpenTargetsFile.but,columnspan=2))
  Try(Abort <- 1)
  onOK <- function()
  {
#      Try(cdf     <- get("cdf",envir=affylmGUIenvironment))
      Try(Targets <- get("Targets",envir=affylmGUIenvironment))  
#      Try(if (length(cdf)==0)
#      {
#        Try(tkmessageBox(title="CDF (Chip Definition) File",message=paste("Either you did not specify a valid CDF (Chip Definition File",
#          "or an error occurred while reading in the CDF file.  It should be in tab-delimited text format and it should include the column headings \"Block\", \"Column\", \"Row\", \"Name\" and \"ID\"."),icon="error"))        
#        onCancel()
#        return()
#      })
      Try(if (length(Targets)==0)
      {
        Try(tkmessageBox(title="RNA Targets File",message=paste("Either you did not specify a valid RNA Targets File",
          "or an error occurred while reading in the Targets file.  It should be in tab-delimited text format and it should include the column headings \"FileName\", and \"Target\".",icon="error")))
        onCancel()
        return()
      })
      Try(tkgrab.release(ttCDFandTargets));
      Try(tkdestroy(ttCDFandTargets));
      Try(tkfocus(ttMain))
      Try(Abort <<- 0)
  }
  onCancel <- function() {Try(tkgrab.release(ttCDFandTargets));Try(tkdestroy(ttCDFandTargets));Try(tkfocus(ttMain));Try(Abort<<-1)}
  Try(OK.but <-tkbutton(ttCDFandTargets,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttCDFandTargets,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttCDFandTargets,text="    ")))
  Try(tkgrid(tklabel(ttCDFandTargets,text="    "),OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttCDFandTargets,text="       ")))
  Try(tkfocus(ttCDFandTargets))
  Try(tkbind(ttCDFandTargets, "<Destroy>", function() {Try(tkgrab.release(ttCDFandTargets));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttCDFandTargets))
   
  if (Abort==1)
        return(0)

  #OK
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))
  Try(Targets <- get("Targets",affylmGUIenvironment))
  Try(slides <- Targets$FileName)
  Try(filesExist <- file.exists(slides))
  Try(filesWhichDontExist <- slides[!filesExist])
  Try(if (length(filesWhichDontExist)>0)
    Try(for (i in (1:length(filesWhichDontExist)))
      Try(tkmessageBox(title="Error opening file",message=paste("Failed to open file: \"",filesWhichDontExist[i],"\"",sep=""),icon="error"))))
  Try(if (length(filesWhichDontExist)>0) 
  {
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)
  })
    
  Try(RawAffyData <- ReadAffy(filenames=Targets$FileName))
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(assign("RawAffyData",RawAffyData,affylmGUIenvironment))
  Try(assign("RawAffyData.Available",TRUE,affylmGUIenvironment))
  Try(SlideNamesVec <- colnames(RawAffyData@exprs))
  Try(if ("Name" %in% colnames(Targets))
    SlideNamesVec <- Targets$Name)
  Try(assign("SlideNamesVec",SlideNamesVec,affylmGUIenvironment))
  Try(assign("ArraysLoaded",TRUE,affylmGUIenvironment))
  Try(tkdelete(mainTree,"RawAffyData.Status"))
  Try(tkinsert(mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Available",font=affylmGUIfontTree))  
  Try(ReturnVal <- GetlimmaDataSetName())
  if (ReturnVal==0) return(0)        
  return(1)
}



GetlimmaDataSetName <- function()
{
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(ttGetlimmaDataSetName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetlimmaDataSetName))
  Try(tkgrab.set(ttGetlimmaDataSetName))
  Try(tkfocus(ttGetlimmaDataSetName))
  Try(tkwm.title(ttGetlimmaDataSetName,"Data Set Name"))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
  if (limmaDataSetNameText=="Untitled")
      Try(limmaDataSetNameText <- "")
  Try(Local.limmaDataSetName <- tclVar(init=limmaDataSetNameText))
  Try(entry.limmaDataSetName <-tkentry(ttGetlimmaDataSetName,width="20",font=affylmGUIfont2,textvariable=Local.limmaDataSetName,bg="white"))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="Please enter a name for this data set.",font=affylmGUIfont2)))
  Try(tkgrid(entry.limmaDataSetName))
  onOK <- function()
  {    
      Try(limmaDataSetNameText <- tclvalue(Local.limmaDataSetName))
      if (nchar(limmaDataSetNameText)==0)
        limmaDataSetNameText <- "Untitled"
#      Try(tkwm.title(ttMain,paste("affylmGUI -",limmaDataSetNameText)))
      Try(assign("limmaDataSetNameText",limmaDataSetNameText,affylmGUIenvironment))
      Try(tclvalue(limmaDataSetName) <- limmaDataSetNameText)
      Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkdestroy(ttGetlimmaDataSetName));Try(tkfocus(ttMain))
  }
  Try(OK.but <-tkbutton(ttGetlimmaDataSetName,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
  Try(tkgrid(OK.but))
  Try(tkgrid.configure(OK.but))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="       ")))
  Try(tkfocus(entry.limmaDataSetName))
  Try(tkbind(entry.limmaDataSetName, "<Return>",onOK))
  Try(tkbind(ttGetlimmaDataSetName, "<Destroy>", function(){Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkfocus(ttMain));return(0)}))
  Try(tkwait.window(ttGetlimmaDataSetName))
  Try(tkfocus(ttMain))
  return (1)
}

GetParameterizationName <- function()
{
  Try(ttGetParameterizationName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetParameterizationName))
  Try(tkgrab.set(ttGetParameterizationName))
  Try(tkwm.title(ttGetParameterizationName,"Parameterization Name"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
  Try(Local.ParameterizationName <- tclVar(init=""))
  Try(entry.ParameterizationName <-tkentry(ttGetParameterizationName,width="20",font=affylmGUIfont2,textvariable=Local.ParameterizationName,bg="white"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="Please enter a name for this parameterization.",font=affylmGUIfont2),columnspan=2))
  Try(tkgrid(entry.ParameterizationName,columnspan=2))

  ReturnVal <- "GetParameterizationName.CANCEL"
  onOK <- function()
  {
      Try(ParameterizationNameText <- tclvalue(Local.ParameterizationName))
      Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- ParameterizationNameText
  }
  onCancel <- function()
  {
      Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- "GetParameterizationName.CANCEL"
  }
  Try(OK.but <-tkbutton(ttGetParameterizationName,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetParameterizationName,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="       ")))
  Try(tkfocus(entry.ParameterizationName))
  Try(tkbind(entry.ParameterizationName, "<Return>",onOK))
  Try(tkbind(ttGetParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetParameterizationName));Try(tkfocus(ttMain));Try(return("GetParameterizationName.CANCEL"))}))
  Try(tkwait.window(ttGetParameterizationName))
  Try(tkfocus(ttMain))
  Try(return (ReturnVal))
}

GetContrastParameterizationName <- function()
{
  Try(ttGetContrastParameterizationName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetContrastParameterizationName))
  Try(tkgrab.set(ttGetContrastParameterizationName))
  Try(tkwm.title(ttGetContrastParameterizationName,"Contrasts Name"))
  Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="    ")))
  Try(Local.ContrastParameterizationName <- tclVar(init=""))
  Try(entry.ContrastParameterizationName <-tkentry(ttGetContrastParameterizationName,width="20",font=affylmGUIfont2,textvariable=Local.ContrastParameterizationName,bg="white"))
  Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="Please enter a name for this set of contrasts.",font=affylmGUIfont2),columnspan=2))
  Try(tkgrid(entry.ContrastParameterizationName,columnspan=2))

  ReturnVal <- "GetContrastParameterizationName.CANCEL"
  onOK <- function()
  {
      Try(ContrastParameterizationNameText <- tclvalue(Local.ContrastParameterizationName))
      Try(tkgrab.release(ttGetContrastParameterizationName));Try(tkdestroy(ttGetContrastParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- ContrastParameterizationNameText
  }
  onCancel <- function()
  {
      Try(tkgrab.release(ttGetContrastParameterizationName));Try(tkdestroy(ttGetContrastParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- "GetContrastParameterizationName.CANCEL"
  }
  Try(OK.but <-tkbutton(ttGetContrastParameterizationName,text="   OK   ",command=onOK,font=affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetContrastParameterizationName,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetContrastParameterizationName,text="       ")))
  Try(tkfocus(entry.ContrastParameterizationName))
  Try(tkbind(entry.ContrastParameterizationName, "<Return>",onOK))
  Try(tkbind(ttGetContrastParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetContrastParameterizationName));Try(tkfocus(ttMain));Try(return("GetContrastParameterizationName.CANCEL"))}))
  Try(tkwait.window(ttGetContrastParameterizationName))
  Try(tkfocus(ttMain))
  Try(return (ReturnVal))
}



NewLimmaFile <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
  
  if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      Try(mbVal <- tkmessageBox(title="Start New Analysis",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes"))
      if (tclvalue(mbVal)=="yes")
          Try(SaveLimmaFile())
      if (tclvalue(mbVal)=="cancel")
      {
          Try(tkfocus(ttMain))
          return()
      }
      Try(limmaDataSetNameText <- "Untitled")
  }
#  Try(tkmessageBox(title="Working Directory",message="After clicking OK, please select a working directory.",type="ok"))
  Try(WD <- SetWD())
  if (WD=="") return()

  Try(tkdelete(mainTree,"RawAffyData.Status"))
  Try(tkdelete(mainTree,"NormalizedAffyData.Status"))
  Try(tkinsert(mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))
  Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))
  Try(tkdelete(mainTree,"LinearModelFit.Status"))
  Try(tkinsert(mainTree,"end","LinearModelFit","LinearModelFit.Status" ,text="Not Available",font=affylmGUIfontTree))

  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
      Try(tkdelete(mainTree,paste("ContrastParameterizations.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"ContrastParameterizations.Status.1")))
  Try(tkinsert(mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=affylmGUIfontTree))

  Try(if (NumParameters>0)
    Try(for (i in (1:NumParameters))
      Try(tkdelete(mainTree,paste("Parameters.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"Parameters.Status.1")))
  Try(tkinsert(mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=affylmGUIfontTree))

  if (NumContrastParameterizations>0)
    for (contrastParameterizationIndex in (1:NumContrastParameterizations))
    {
      Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
      Try(ParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
      Try(tkdelete(ContrastParameterizationTREE,ParameterizationNameNode))
      Try(assign("ContrastParameterizationList", deleteItemFromList(ContrastParameterizationList,ParameterizationNameNode),affylmGUIenvironment))
    }
  Try(initGlobals())
  Try(LimmaFileName <- get("LimmaFileName",affylmGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only  
  Try(tkwm.title(ttMain,paste("affylmGUI -",LimmaFileName)))
  Try(tclvalue(CDFfileBoxTitle)     <- "Please select a Chip Definition (CDF) file.")
  Try(tclvalue(CDFfileName)         <- "No filename is selected at the moment.  Press the Select CDF File Button.")
  Try(tclvalue(TargetsfileBoxTitle) <- "Please select a tab-delimited file listing the CEL files.")
  Try(tclvalue(TargetsfileName)     <- "No filename is selected at the moment.  Press the Select Targets File Button.")
  Try(OpenCDFandTargetsfiles())
  Try(tkfocus(ttMain))
}

chooseDir <- function()
{
	Try(wd <- tclVar(getwd()))
	Try(ttChooseDir <- tktoplevel(ttMain))
	Try(tkwm.title(ttChooseDir,"Choose working directory"))
	Try(tkwm.deiconify(ttChooseDir))
	Try(tkgrab.set(ttChooseDir))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(label1 <- tklabel(ttChooseDir,text="Choose working directory (containing input files):",font=affylmGUIfont2))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),label1,sticky="w"))
	Try(tkgrid.configure(label1,columnspan=3))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(onBrowse <- function() 
	{
	  Try(if (file.exists(tclvalue(wd))) initialdir<-gsub("/","\\\\",tclvalue(wd)) else initialdir<-gsub("/","\\\\",getwd()))
	  Try(dir1 <- tclvalue(tkchooseDirectory(title="Please choose a working directory for the Limma Analysis",initialdir=initialdir)))
		Try(if (nchar(dir1)>0) tclvalue(wd) <- dir1)
	})
	Try(ReturnVal <- "")
	Try(onOK <- function() {Try(DirChosen <- tclvalue(wd));Try(tkgrab.release(ttChooseDir));Try(tkdestroy(ttChooseDir)); Try(ReturnVal <<- DirChosen)})
	Try(onCancel <- function() {Try(tkgrab.release(ttChooseDir));Try(tkdestroy(ttChooseDir))})
	Try(Browse.but <- tkbutton(ttChooseDir,text="Browse",command=onBrowse,font=affylmGUIfont2))
	Try(OK.but <- tkbutton(ttChooseDir,text="    OK    ",command=onOK,font=affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttChooseDir,text=" Cancel ",command=onCancel,font=affylmGUIfont2))
	Try(entry1 <- tkentry(ttChooseDir,textvariable=wd,width=40))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),entry1))
	Try(tkgrid.configure(entry1,columnspan=3))
  Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=4))
	Try(tkgrid(Browse.but,row=3,column=5))
  Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=6))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),tklabel(ttChooseDir,text="    "),OK.but,Cancel.but))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(tkfocus(entry1))
	Try(tkbind(ttChooseDir,"<Destroy>",function()tkgrab.release(ttChooseDir)))
	Try(tkbind(entry1,"<Return>",onOK))
	Try(tkwait.window(ttChooseDir))
	return(ReturnVal)	
}


SetWD <- function()
{
  WD <- chooseDir()
  if (!nchar(WD)) 
  {
      tkfocus(ttMain)
      return()
  }
  Try(setwd(WD))
  tkfocus(ttMain)
  return(WD)
}

onExit <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
  if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      Try(mbVal <- tkmessageBox(title="Exit affylmGUI",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes"))
      if (tclvalue(mbVal)=="yes")
          Try(SaveLimmaFile())
      if (tclvalue(mbVal)=="cancel")
          return()
  }
  Try(if (Sys.info()["sysname"] == "Windows") 
    bringToTop(-1))
  tclvalue(done)<-1
}

ChooseContrastParameterization <- function()
{
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Choose Contrasts Parameterization",message="There are no contrasts parameterizations available.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })      
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  
  ttChooseContrastParameterization<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseContrastParameterization)
  tkgrab.set(ttChooseContrastParameterization)  
  tkfocus(ttChooseContrastParameterization)
  tkwm.title(ttChooseContrastParameterization,"Choose a Contrasts Parameterization")
  scr <- tkscrollbar(ttChooseContrastParameterization, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttChooseContrastParameterization,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white",font=affylmGUIfont2)   
  lbl2<-tklabel(ttChooseContrastParameterization,text="Which contrasts parameterization is this for?",font=affylmGUIfont2)
  tkgrid(tklabel(ttChooseContrastParameterization,text="       "),row=0,column=1,columnspan=1)
  tkgrid(tklabel(ttChooseContrastParameterization,text="       "),row=0,column=4,columnspan=1)
  tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttChooseContrastParameterization,text="         "),row=2,column=1)
  tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=3,columnspan=1,rowspan=4,sticky="wns")
  if (NumContrastParameterizations>0)
    for (i in (1:NumContrastParameterizations))
       tkinsert(tl,"end",ContrastParameterizationNamesVec[i])
  tkselection.set(tl,0)

  ReturnVal <- 0
  onOK <- function()
  {
      Try(contrastParameterizationIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttChooseContrastParameterization));Try(tkdestroy(ttChooseContrastParameterization));Try(tkfocus(ttMain))
      ReturnVal <<- contrastParameterizationIndex
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseContrastParameterization));Try(tkdestroy(ttChooseContrastParameterization));Try(tkfocus(ttMain));ReturnVal <<- 0}
  OK.but <-tkbutton(ttChooseContrastParameterization,text="   OK   ",command=onOK,font=affylmGUIfont2)
  Cancel.but <-tkbutton(ttChooseContrastParameterization,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
  tkgrid(tklabel(ttChooseContrastParameterization,text="    "))
  tkgrid(tklabel(ttChooseContrastParameterization,text="    "),tklabel(ttChooseContrastParameterization,text="    "),OK.but,Cancel.but)
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  
  tkgrid(tklabel(ttChooseContrastParameterization,text="    "))
  Try(tkfocus(ttChooseContrastParameterization))
  Try(tkbind(ttChooseContrastParameterization, "<Destroy>", function() {Try(tkgrab.release(ttChooseContrastParameterization));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttChooseContrastParameterization))

  return (ReturnVal)
}

DeleteContrastParameterization <- function()
{
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  
  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Delete Contrasts Parameterization",message="There are no contrast parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })  
  Try(contrastParameterizationIndex <- ChooseContrastParameterization())
  Try(if (contrastParameterizationIndex==0)    return())
  Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
  Try(ContrastParameterizationNameNode<- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
  
  Try(tkdelete(ContrastParameterizationTREE,ContrastParameterizationNameNode))
  Try(ContrastParameterizationList <- deleteItemFromList(ContrastParameterizationList,
                            ContrastParameterizationNameNode))
  Try(tempVec <- c())
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
    {
      Try(if (i!=ContrastParameterizationTREEIndex)
          Try(tempVec <- c(tempVec,ContrastParameterizationTREEIndexVec[i])))
    }))
  Try(ContrastParameterizationTREEIndexVec <- tempVec)


  Try(tempVec <- c())
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
    {
      Try(if (i!=ContrastParameterizationTREEIndex)
          Try(tempVec <- c(tempVec,ContrastParameterizationNamesVec[i])))
    }))
  Try(ContrastParameterizationNamesVec <- tempVec)
  
  Try(NumContrastParameterizations <- NumContrastParameterizations - 1)        
  Try(ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)  
#  Try(if (ContrastParameterizationList[[ContrastParameterizationNameNode]]$NumContrastParameterizations==0)
#    Try(tkinsert(ContrastParameterizationTREE,"0","root",paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,".1",sep=""),text="none",font=affylmGUIfontTree)))

  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
      Try(tkdelete(mainTree,paste("ContrastParameterizations.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"ContrastParameterizations.Status.1")))      

  Try(if (NumContrastParameterizations>0)
  {
    for (contrastParameterizationIndex in (1:NumContrastParameterizations))
    {
      Try(contrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
#      Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",contrastParameterizationTREEIndex,sep=""))
      Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",contrastParameterizationTREEIndex,sep=""))
      Try(tkinsert(mainTree,"end","ContrastParameterizations",ContrastParameterizationsStatusNameNode ,text=ContrastParameterizationNamesVec[contrastParameterizationIndex],font=affylmGUIfontTree))
#      Try(contrastsMatrix <- ContrastParameterizationList[[1]]$contrastsMatrixInList$contrasts)
#      Try(ContrastsNames <- colnames(contrastsMatrix))
#      Try(ContrastParameterizationNameText <- ContrastParameterizationList[[1]]$ContrastParameterizationNameText)
#      Try(tkinsert(ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=affylmGUIfontTree))
#      Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
#      Try(for (j in (1:NumContrastsInContrastParameterization))
#        Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,paste("Contrasts.",contrastParameterizationIndex,".",contrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=affylmGUIfontTree)))  
    }
  }
  else
    Try(tkinsert(mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=affylmGUIfontTree)))
  Try(assign("ContrastParameterizationList",ContrastParameterizationList,affylmGUIenvironment))
  Try(assign("ContrastParameterizationTREEIndexVec",ContrastParameterizationTREEIndexVec,affylmGUIenvironment))
  Try(assign("NumContrastParameterizations",NumContrastParameterizations,affylmGUIenvironment))
  Try(assign("ContrastParameterizationNamesVec",ContrastParameterizationNamesVec,affylmGUIenvironment))  
}


OpenLimmaFile <- function()
{
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
  Try(tempLimmaFileName <- tclvalue(tkgetOpenFile(filetypes="{{Limma Files} {.lma}} {{All files} *}")))
  Try(if (!nchar(tempLimmaFileName)) 
  {
    tkfocus(ttMain)
    return()
  })
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      Try(mbVal <- tkmessageBox(title="Open File",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes"))
      Try(if (tclvalue(mbVal)=="yes")
          SaveLimmaFile())
      Try(if (tclvalue(mbVal)=="cancel")
          return())
  })
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
  
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))

  Try(NumParameters <- get("NumParameters",envir=affylmGUIenvironment))        
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))

# Using existing NumContrastParameterizations, NOT the one loaded from the .lma file.
# (We haven't loaded it yet.)
  Try(OldNumParameters <- NumParameters)

  Try(if (NumContrastParameterizations>0)
    Try(for (contrastParameterizationIndex in (1:NumContrastParameterizations))
    {
      Try(ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
      Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",ContrastParameterizationTREEIndex,sep=""))
      Try(tkdelete(ContrastParameterizationTREE,ContrastParameterizationNameNode))
      Try(assign("ContrastParameterizationList",deleteItemFromList(ContrastParameterizationList,ContrastParameterizationNameNode),affylmGUIenvironment))         
    }))

  # Load the RData File whose name is "LimmaFileName"
  Try(load(LimmaFileName,envir=affylmGUIenvironment))

  # The user may have changed the filename in the operating system since the last save.
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
  
  Try(limmaDataSetNameText <- get("limmaDataSetNameText" , envir=affylmGUIenvironment))
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec", envir=affylmGUIenvironment))
  Try(NumContrastParameterizations <- get("NumContrastParameterizations", envir=affylmGUIenvironment))  
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(NumParameters <- get("NumParameters" , envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))

  Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only 
  Try(tkwm.title(ttMain,paste("affylmGUI -",LimmaFileName)))
  Try(assign("limmaDataSetNameText",limmaDataSetNameText,affylmGUIenvironment))
  Try(tclvalue(limmaDataSetName) <- limmaDataSetNameText)  

  Try(tkdelete(mainTree,"RawAffyData.Status"))
  Try(tkdelete(mainTree,"NormalizedAffyData.Status"))
  Try(tkdelete(mainTree,"LinearModelFit.Status"))  
  Try(if (OldNumParameters>0)
    Try(for (i in (1:OldNumParameters))
      Try(tkdelete(mainTree,paste("Parameters.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"Parameters.Status.1")))
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
      Try(tkdelete(mainTree,paste("ContrastParameterizations.Status.",i,sep=""))))
  else
      Try(tkdelete(mainTree,"ContrastParameterizations.Status.1")))      
  
  Try(RawAffyData.Available            <- get("RawAffyData.Available" , envir=affylmGUIenvironment))  
  Try(NormalizedAffyData.Available     <- get("NormalizedAffyData.Available" , envir=affylmGUIenvironment))    
  Try(LinearModelFit.Available         <- get("LinearModelFit.Available" , envir=affylmGUIenvironment))    
  
  Try(if (RawAffyData.Available)  
    Try(tkinsert(mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Available",font=affylmGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","RawAffyData","RawAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))    )
  Try(if (exists("NormMethod",envir=affylmGUIenvironment))
    Try(NormMethod <- get("NormMethod",envir=affylmGUIenvironment))
  else
    Try(NormMethod <- "RMA"))
  Try(if (NormalizedAffyData.Available)  
    Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text=paste("Available (",NormMethod,")",sep=""),font=affylmGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Not Available",font=affylmGUIfontTree))    )    
  Try(if (LinearModelFit.Available)
    Try(tkinsert(mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Available",font=affylmGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","LinearModelFit","LinearModelFit.Status",text="Not Available",font=affylmGUIfontTree)))
  Try(if (LinearModelFit.Available)
  {
    Try(design <- get("design",affylmGUIenvironment))
    Try(if (NumParameters>0)
    {
      for (i in (1:NumParameters))          
        Try(tkinsert(mainTree,"end","Parameters",
          paste("Parameters.Status.",i,sep=""),text=colnames(design)[i],font=affylmGUIfontTree))
    }
    else
      Try(tkinsert(mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=affylmGUIfontTree)))
  }
  else
  {
    Try(tkinsert(mainTree,"end","Parameters","Parameters.Status.1" ,text="None",font=affylmGUIfontTree))
  })

  Try(if (NumContrastParameterizations>0)
  {
    for (contrastParameterizationIndex in (1:NumContrastParameterizations))
    {
      Try(contrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
      Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",contrastParameterizationTREEIndex,sep=""))
      Try(ContrastParameterizationsStatusNameNode <- paste("ContrastParameterizations.Status.",contrastParameterizationTREEIndex,sep=""))
      Try(tkinsert(mainTree,"end","ContrastParameterizations",ContrastParameterizationsStatusNameNode ,text=ContrastParameterizationNamesVec[contrastParameterizationIndex],font=affylmGUIfontTree))
      Try(contrastsMatrix <- ContrastParameterizationList[[1]]$contrastsMatrixInList$contrasts)
      Try(ContrastsNames <- colnames(contrastsMatrix))
      Try(ContrastParameterizationNameText <- ContrastParameterizationList[[1]]$ContrastParameterizationNameText)
      Try(tkinsert(ContrastParameterizationTREE,"end","root",ContrastParameterizationNameNode,text=ContrastParameterizationNameText,font=affylmGUIfontTree))
      Try(ContrastsNode <- paste("ContrastsNode.",contrastParameterizationTREEIndex))
  
      Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,ContrastsNode,text="Contrasts",font=affylmGUIfontTree))    
      
      Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
      Try(for (j in (1:NumContrastsInContrastParameterization))
        Try(tkinsert(ContrastParameterizationTREE,"end",ContrastsNode,paste("Contrasts.",contrastParameterizationTREEIndex,".",j,sep=""),text=ContrastsNames[j],font=affylmGUIfontTree)))          
  
			Try(LinearModelFitNode       <- paste("LinearModelFitNode.",contrastParameterizationTREEIndex))
			Try(LinearModelFitStatusNode <- paste("LinearModelFitStatusNode.",contrastParameterizationTREEIndex))  
			Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,LinearModelFitNode,text="Linear Model Fit",font=affylmGUIfontTree))    
			Try(tkinsert(ContrastParameterizationTREE,"end",LinearModelFitNode,LinearModelFitStatusNode,text="Available",font=affylmGUIfontTree))      
			
			Try(if (("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
			                  length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0)
        Try(ebayesAvailable <- TRUE)
      else
        Try(ebayesAvailable <- FALSE))
			
			Try(EmpiricalBayesNode       <- paste("EmpiricalBayesNode.",contrastParameterizationTREEIndex))
			Try(EmpiricalBayesStatusNode <- paste("EmpiricalBayesStatusNode.",contrastParameterizationTREEIndex))
			Try(tkinsert(ContrastParameterizationTREE,"end",ContrastParameterizationNameNode,EmpiricalBayesNode,text="Empirical Bayes Statistics",font=affylmGUIfontTree))    
			Try(if (ebayesAvailable==TRUE)			
			  Try(tkinsert(ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Available",font=affylmGUIfontTree))      
			else
			  Try(tkinsert(ContrastParameterizationTREE,"end",EmpiricalBayesNode,EmpiricalBayesStatusNode,text="Not Available",font=affylmGUIfontTree)))
  
    }
  }
  else
    Try(tkinsert(mainTree,"end","ContrastParameterizations","ContrastParameterizations.Status.1" ,text="None",font=affylmGUIfontTree)))

  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(tkfocus(ttMain))

}

SaveLimmaFile <- function()
{
  LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment)
  if (LimmaFileName=="Untitled")
  {
    SaveAsLimmaFile()
    try(tkfocus(ttMain),silent=TRUE)
    return()
  }
  # Don't give an error if main window has been destroyed.
  try(tkconfigure(ttMain,cursor="watch"),silent=TRUE)  
#  tkmessageBox(message="About to save Limma File! (0)")  
  Try(save(list = ls(envir=affylmGUIenvironment), file=LimmaFileName, envir=affylmGUIenvironment))  
#  tkmessageBox(message="Limma File Saved! (1)")  
  try(tkconfigure(ttMain,cursor="arrow"),silent=TRUE)
  try(tkfocus(ttMain),silent=TRUE)
#  tkmessageBox(message="Limma File Saved! (2)")    
}    

SaveAsLimmaFile <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=affylmGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
  Try(tempLimmaFileName <- tclvalue(tkgetSaveFile(initialfile=LimmaFileName,filetypes="{{Limma Files} {.lma}} {{All files} *}")))
  if (!nchar(tempLimmaFileName)) 
  {
    try(tkfocus(ttMain),silent=TRUE)
    return()
  }
  len <- nchar(tempLimmaFileName)
  if (len<=4)
      tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
  else if (substring(tempLimmaFileName,len-3,len)!=".lma")
        tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,affylmGUIenvironment))
  # ttMain may have been destroyed
  e <- try(tkfocus(ttMain),silent=TRUE)
  if (!inherits(e, "try-error"))
    Try(tkwm.title(ttMain,paste("affylmGUI -",LimmaFileName)))
  try(tkconfigure(ttMain,cursor="watch"),silent=TRUE)    
  Try(save(list = ls(envir=affylmGUIenvironment), file=LimmaFileName, envir=affylmGUIenvironment))    
  try(tkconfigure(ttMain,cursor="arrow"),silent=TRUE)
  try(tkfocus(ttMain),silent=TRUE)
}

AboutaffylmGUI <- function()
{
    Try(tkmessageBox(title="About affylmGUI",message=paste("This is affylmGUI Version ",getPackageVersion("affylmGUI"),
                              ", using limma Version ",getPackageVersion("limma"),".  The limma package was developed by Gordon Smyth and the Graphical User Interface (GUI) was developed by James Wettenhall.",sep=""),type="ok",icon="info"))
}


ChooseCDF <- function()
{  
  ttChooseCDF<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseCDF)
  tkgrab.set(ttChooseCDF)  
  tkfocus(ttChooseCDF)
  tkwm.title(ttChooseCDF,"Choose a CDF")
  scr <- tkscrollbar(ttChooseCDF, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttChooseCDF,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white",font=affylmGUIfont2)   
  lbl2<-tklabel(ttChooseCDF,text="Choose a Chip Definition File (CDF)",font=affylmGUIfont2)
  tkgrid(tklabel(ttChooseCDF,text="       "),row=0,column=1,columnspan=1)
  tkgrid(tklabel(ttChooseCDF,text="       "),row=0,column=4,columnspan=1)
  tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttChooseCDF,text="         "),row=2,column=1)
  tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns")

  Try(tkconfigure(ttMain,cursor="watch"))
  Require("reposTools")
  Try(cdfPackages <- getReposEntry("http://www.bioconductor.org/data/cdfenvs/repos"))
  Try(cdfDataFrame <- cdfPackages@repdatadesc@repdatadesc)
  Try(len <- nrow(cdfDataFrame))
  Try(for (i in (1:len)) tkinsert(tl,"end",paste(cdfDataFrame[i,"Package"],cdfDataFrame[i,"Version"][[1]])))
  tkselection.set(tl,0)
  Try(tkconfigure(ttMain,cursor="arrow"))
  
  ReturnVal <- ""
  onOK <- function()
  {
      Try(cdfIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttChooseCDF));Try(tkdestroy(ttChooseCDF));Try(tkfocus(ttMain))
      Try(ReturnVal <<- cdfDataFrame[cdfIndex,"Package"])
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseCDF));Try(tkdestroy(ttChooseCDF));Try(tkfocus(ttMain));ReturnVal <<- ""}
  OK.but <-tkbutton(ttChooseCDF,text="   OK   ",command=onOK,font=affylmGUIfont2)
  Cancel.but <-tkbutton(ttChooseCDF,text=" Cancel ",command=onCancel,font=affylmGUIfont2)
  tkgrid(tklabel(ttChooseCDF,text="    "))
  tkgrid(tklabel(ttChooseCDF,text="    "),tklabel(ttChooseCDF,text="    "),OK.but,Cancel.but)
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  Try(tkbind(OK.but, "<Return>",onOK))  
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))    
  tkgrid(tklabel(ttChooseCDF,text="    "))
  Try(tkfocus(ttChooseCDF))
  Try(tkbind(ttChooseCDF, "<Destroy>", function() {Try(tkgrab.release(ttChooseCDF));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttChooseCDF))

  return (ReturnVal)
}

