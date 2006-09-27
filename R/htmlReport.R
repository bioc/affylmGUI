###########################################################################################################################
# R2HTML Plot Function (Modified to accept a plotFunction argument, rather than using the main R Graphics Device)

"HTMLplotUsingFunction" <- function (Caption = "", File = .HTML.file, GraphRelativeDirectory = ".", GraphAbsoluteDirectory = NULL, GraphFileName = "", GraphSaveAs = "png", GraphBorder = 1,  Align = "center", plotFunction = NULL,Width=600,Height=600,PointSize=12,BG="white",res=72,...)
{
    if (is.null(GraphAbsoluteDirectory))
      GraphAbsoluteDirectory <- getwd()
    if (GraphFileName == "") {
        nowd <- date()
        GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7), substring(nowd, 9, 10), "_", substring(nowd, 12, 13), substring(nowd, 15,  16), substring(nowd, 18, 19), sep = "")
    }
    GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")

#    AbsGraphFileName <- paste(GraphRelativeDirectory,.Platform$file.sep,GraphFileName,sep="")
    AbsGraphFileName <- file.path(GraphAbsoluteDirectory, GraphFileName)
    if (GraphSaveAs=="png")
    {
      if (is.null(plotFunction))
        dev.print(png, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
          Try(bitmap(file = AbsGraphFileName,bg=BG,res=res))
        else
          Try(png(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)))
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs=="jpg")
    {
      if (is.null(plotFunction))
        dev.print(jpeg, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
          Try(bitmap(filename = AbsGraphFileName,bg=BG,res=res,type="jpeg"))
        else
          Try(jpeg(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)))
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs=="gif")
    {
      if (is.null(plotFunction))
        dev.print(gif, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        stop("When passing a plot function to HTMLplot, device must be jpg or png.")
      }
    }
    else stop("GraphSaveAs must be either jpg, png or gif")
    cat(paste("<p align=", Align, "><img src='", paste(GraphRelativeDirectory,"/",GraphFileName,sep=""), "' border=", GraphBorder, ">", sep = "", collapse = ""), file = File, append = TRUE, sep = "")
    if (Caption != "") {
        cat(paste("<br><i>", Caption, "</i>"), file = File, append = TRUE, sep = "")
    }
    cat("</P>", file = File, append = TRUE, sep = "\n")
    try(assign(".HTML.graph", TRUE, env = get("HTMLenv", envir = .GlobalEnv)))
    invisible(return())
}

###########################################################################################################################

GetComponentsToExportInHTMLreport <- function(contrastParameterizationIndex=NULL){
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))

  Try(ttHTMLreportDialog<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttHTMLreportDialog))
  Try(tkgrab.set(ttHTMLreportDialog))
  Try(tkfocus(ttHTMLreportDialog))
  Try(tkwm.title(ttHTMLreportDialog,"HTML Report"))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    ")))

  Try(TargetsTcl                    <- tclVar("1"))
  Try(NormalizationMethodTcl        <- tclVar("1"))
  Try(RawIntensityBoxPlotTcl        <- tclVar("1"))
  Try(NormalizedIntensityBoxPlotTcl <- tclVar("1"))
  Try(DesignMatrixTcl               <- tclVar("1"))

  Try(if (NumContrastParameterizations>0)
    Try(ContrastMatrixTcl             <- tclVar("1"))
  else
    Try(ContrastMatrixTcl             <- tclVar("0")))
  Try(if (NumContrastParameterizations>0)
    Try(MAPlotsContrastsTcl           <- tclVar("1"))
  else
    Try(MAPlotsContrastsTcl           <- tclVar("0")))
  Try(if (NumContrastParameterizations>0)
    Try(Top50ToptablesTcl             <- tclVar("1"))
  else
    Try(Top50ToptablesTcl             <- tclVar("0")))
  Try(if (NumContrastParameterizations>0)
    Try(CompleteToptablesTcl          <- tclVar("1"))
  else
    Try(CompleteToptablesTcl          <- tclVar("0")))
  Try(if (NumContrastParameterizations>0)
    Try(Top50ToptablesTcl    <- tclVar("1"))
  else
    Try(Top50ToptablesTcl    <- tclVar("0")))
  Try(CompleteToptablesTcl <- tclVar("0"))

  Try(TargetsCheckbox                     <- tkcheckbutton(ttHTMLreportDialog,variable=TargetsTcl))
  Try(NormalizationMethodCheckbox         <- tkcheckbutton(ttHTMLreportDialog,variable=NormalizationMethodTcl))
  Try(RawIntensityBoxPlotCheckbox         <- tkcheckbutton(ttHTMLreportDialog,variable=RawIntensityBoxPlotTcl))
  Try(NormalizedIntensityBoxPlotCheckbox  <- tkcheckbutton(ttHTMLreportDialog,variable=NormalizedIntensityBoxPlotTcl))
  Try(DesignMatrixCheckbox                <- tkcheckbutton(ttHTMLreportDialog,variable=DesignMatrixTcl))
  Try(ContrastMatrixCheckbox              <- tkcheckbutton(ttHTMLreportDialog,variable=ContrastMatrixTcl))
  Try(MAPlotsContrastsCheckbox            <- tkcheckbutton(ttHTMLreportDialog,variable=MAPlotsContrastsTcl))
  Try(Top50ToptablesCheckbox              <- tkcheckbutton(ttHTMLreportDialog,variable=Top50ToptablesTcl))
  Try(CompleteToptablesCheckbox           <- tkcheckbutton(ttHTMLreportDialog,variable=CompleteToptablesTcl))

  Try(lbl2 <- tklabel(ttHTMLreportDialog,text="Components to be Included in the HTML Report",font=.affylmGUIglobals$affylmGUIfont2))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),lbl2)
  Try(tkgrid.configure(lbl2,columnspan=3,sticky="w"))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "))

  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="RNA Targets",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),TargetsCheckbox,currentLabel))
  Try(tkgrid.configure(TargetsCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Normalization Method",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),NormalizationMethodCheckbox,currentLabel))
  Try(tkgrid.configure(NormalizationMethodCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Raw Intensity Box Plot",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),RawIntensityBoxPlotCheckbox,currentLabel))
  Try(tkgrid.configure(RawIntensityBoxPlotCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Normalized Intensity Box Plot",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),NormalizedIntensityBoxPlotCheckbox,currentLabel))
  Try(tkgrid.configure(NormalizedIntensityBoxPlotCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Design Matrix (Parameterization)",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),DesignMatrixCheckbox,currentLabel))
  Try(tkgrid.configure(DesignMatrixCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))

  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Contrasts Matrix (Parameterization)",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),ContrastMatrixCheckbox,currentLabel))
  Try(tkgrid.configure(ContrastMatrixCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="M A Plots for Contrasts",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),MAPlotsContrastsCheckbox,currentLabel))
  Try(tkgrid.configure(MAPlotsContrastsCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Top 50 DE Genes",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),Top50ToptablesCheckbox,currentLabel))
  Try(tkgrid.configure(Top50ToptablesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Complete Lists of DE-Ranked Genes",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),CompleteToptablesCheckbox,currentLabel))
  Try(tkgrid.configure(CompleteToptablesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))

  if (NumContrastParameterizations==0)
  {
    Try(tkconfigure(ContrastMatrixCheckbox,state="disabled"))
    Try(tkconfigure(MAPlotsContrastsCheckbox,state="disabled"))
    Try(tkconfigure(Top50ToptablesCheckbox,state="disabled"))
    Try(tkconfigure(CompleteToptablesCheckbox,state="disabled"))
  }

  tkgrid(tklabel(ttHTMLreportDialog,text="    "))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "))
  ReturnVal <- list()
  onOK <- function()
  {
      if (tclvalue(TargetsTcl)=="1") ReturnVal[["Targets"]] <- TRUE else ReturnVal[["Targets"]] <- FALSE
      if (tclvalue(NormalizationMethodTcl)=="1") ReturnVal[["NormalizationMethod"]] <- TRUE else ReturnVal[["NormalizationMethod"]] <- FALSE
      if (tclvalue(RawIntensityBoxPlotTcl)=="1") ReturnVal[["RawIntensityBoxPlot"]] <- TRUE else ReturnVal[["RawIntensityBoxPlot"]] <- FALSE
      if (tclvalue(NormalizedIntensityBoxPlotTcl)=="1") ReturnVal[["NormalizedIntensityBoxPlot"]] <- TRUE else ReturnVal[["NormalizedIntensityBoxPlot"]] <- FALSE

      if (tclvalue(DesignMatrixTcl)=="1") ReturnVal[["DesignMatrix"]] <- TRUE else ReturnVal[["DesignMatrix"]] <- FALSE
      if (tclvalue(ContrastMatrixTcl)=="1") ReturnVal[["ContrastsMatrix"]] <- TRUE else ReturnVal[["ContrastsMatrix"]] <- FALSE
      if (tclvalue(MAPlotsContrastsTcl)=="1") ReturnVal[["MAPlotsContrasts"]] <- TRUE else ReturnVal[["MAPlotsContrasts"]] <- FALSE
      if (tclvalue(Top50ToptablesTcl)=="1") ReturnVal[["Top50ToptablesTcl"]] <- TRUE else ReturnVal[["Top50Toptables"]] <- FALSE
      if (tclvalue(CompleteToptablesTcl)=="1") ReturnVal[["CompleteToptablesTcl"]] <- TRUE else ReturnVal[["CompleteToptablesTcl"]] <- FALSE

      Try(tkgrab.release(ttHTMLreportDialog));Try(tkdestroy(ttHTMLreportDialog));Try(tkfocus(.affylmGUIglobals$ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttHTMLreportDialog));Try(tkdestroy(ttHTMLreportDialog));Try(tkfocus(.affylmGUIglobals$ttMain)); ReturnVal <<- list()}
  OK.but <-tkbutton(ttHTMLreportDialog,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttHTMLreportDialog,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),OK.but,Cancel.but,tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "))
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),
       tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "))
  Try(tkfocus(ttHTMLreportDialog))
  Try(tkbind(ttHTMLreportDialog, "<Destroy>", function() {Try(tkgrab.release(ttHTMLreportDialog));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkwait.window(ttHTMLreportDialog))

  return (ReturnVal)
}#end of GetComponentsToExportInHTMLreport <- function(contrastParameterizationIndex=NULL)

###########################################################################################################################

ExportHTMLreport <- function(){
# We will use the R2HTML package, but with my own HTMLplot function.
# Will we need xtable or does R2HTML have its own HTMLtable function?
	Require("xtable")
	Require("R2HTML")
	#
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	#
	if(ArraysLoaded==FALSE){
		Try(tkmessageBox(title="Export HTML Report",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	}
	if(NumContrastParameterizations>0){
		Try(contrastParameterizationIndex <- ChooseContrastParameterization())
		Try(if (contrastParameterizationIndex==0) return()) # Cancel
		#
		Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
		Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
		Try(contrastsMatrix <- as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts))
		Try(ContrastNamesVec  <- colnames(contrastsMatrix))
		Try(NumContrasts <- length(ContrastNamesVec))
		Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
		Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
		Try(fit <- eBayes(fit))
		#
		Try(ComponentsToExport <- GetComponentsToExportInHTMLreport(contrastParameterizationIndex))
	}else{
		Try(ComponentsToExport <- GetComponentsToExportInHTMLreport())
	}
  Try(if (length(ComponentsToExport)==0) return())
	#
  Try(fileNameWithPath<- tkgetSaveFile(initialfile=limmaDataSetNameText,filetypes="{{HTML Files} {.html .htm}} {{All files} *}"))
	#Try(tkmessageBox(title="229:affylmGUI-htmlreport",message=paste("fileNameWithPath =",fileNameWithPath),icon="info",default="ok"))
	Try(if (nchar(tclvalue(fileNameWithPath))==0)return())
  #Try(path     <- tclvalue(tkfile.dir (tclvalue(fileNameWithPath))))###tkfile.dir is deprecated but tclfile.dir fails
  Try(path     <- tclvalue(tcltk:::tclfile.dir (tclvalue(fileNameWithPath))))###tkfile.dir is deprecated but tclfile.dir fails
	#Try(tkmessageBox(title="235:affylmGUI-htmlreport",message=paste("path =",path),icon="info",default="ok"))
	#
  Try(fileName <- tclvalue(tcltk:::tclfile.tail(tclvalue(fileNameWithPath))))###tkfile.tail is deprecated but tclfile.tail fails
	#Try(tkmessageBox(title="242:affylmGUI-htmlreport",message=paste("fileName =",fileName),icon="info",default="ok"))

  Try(len <- nchar(fileName))
  if (len<4)
      Try(fileName <- paste(fileName,".html",sep=""))
  else if   ((tolower(substring(fileName,len-4,len))!=".html") &&
  (len<5 ||  (tolower(substring(fileName,len-4,len))!=".html")))
          Try(fileName <- paste(fileName,".html",sep=""))

  Try(fileNameWithoutExtension <- substring(fileName,1,nchar(fileName)-5))

  Try(HTMLfilePath <- paste(path,.Platform$file.sep,fileNameWithoutExtension,"_files",sep=""))
  Try(HTMLfileRelativePath <- paste(fileNameWithoutExtension,"_files",sep=""))
  Try(dir.create(HTMLfilePath))

  Try(fileNameWithPath <- paste(path,"/",fileName,sep=""))

  Try(R2HTMLpath <- system.file(package="R2HTML","output"))
  Try(cssFileSource <- paste(R2HTMLpath,"/","R2HTML.css",sep=""))
  Try(cssFileDestination <- paste(path,"/","R2HTML.css",sep=""))
  Try(R2HTMLlogoSource <- paste(R2HTMLpath,"/","R2HTMLlogo.gif",sep=""))
  Try(R2HTMLlogoDestination <- paste(path,"/","R2HTMLlogo.gif",sep=""))
  Try(file.copy(cssFileSource,cssFileDestination,overwrite=TRUE))
  Try(file.copy(R2HTMLlogoSource,R2HTMLlogoDestination,overwrite=TRUE))

  Try(HTMLtarget <- HTMLInitFile(path,filename=fileNameWithoutExtension,Title=paste(limmaDataSetNameText,"- Statistical Microarray Analysis using affylmGUI"), HTMLframe=FALSE,BackGroundColor="#FFFFFF"))

  Try(HTML.title(paste(limmaDataSetNameText,"- Statistical Microarray Analysis using affylmGUI"),HR=1))

  Try(ExportTargets                    <- ComponentsToExport$Targets)
  Try(ExportNormalizationMethod        <- ComponentsToExport$NormalizationMethod)
  Try(ExportRawIntensityBoxPlot        <- ComponentsToExport$RawIntensityBoxPlot)
  Try(ExportNormalizedIntensityBoxPlot <- ComponentsToExport$NormalizedIntensityBoxPlot)
  Try(ExportDesignMatrix               <- ComponentsToExport$DesignMatrix)
  Try(ExportContrastsMatrix            <- ComponentsToExport$ContrastsMatrix)
  Try(ExportMAPlotsContrasts           <- ComponentsToExport$MAPlotsContrasts)
  Try(ExportTop50Toptables             <- ComponentsToExport$Top50Toptables)
  Try(ExportCompleteToptables          <- ComponentsToExport$CompleteToptables)

  if (ExportRawIntensityBoxPlot || ExportNormalizedIntensityBoxPlot || ExportMAPlotsContrasts)
  {
    Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
    {
      Try(pngParams <- GetJpegOrPngX11Params(graphFileType="PNG"))
      Try(if (length(pngParams)==0) return())
      Try(plotBG        <- pngParams$bg)
      Try(plotRes       <- pngParams$res)
    }
    else
    {
      Try(pngParams <- GetJpegOrPngParams(graphFileType="PNG"))
      Try(if (length(pngParams)==0) return())
      Try(plotWidth     <- pngParams$width)
      Try(plotHeight    <- pngParams$height)
      Try(plotPointSize <- pngParams$pointsize)
      Try(plotBG        <- pngParams$bg)
    })
  }

  Try(HTML.title("Contents",HR=2))
  Try(if (ExportTargets) Try(HTMLli(txt="<a href=\"#Targets\"><b>Targets</b></a>")))
  Try(if (ExportNormalizationMethod) Try(HTMLli(txt="<a href=\"#NormalizationMethod\"><b>Normalization Method</b></a>")))
  Try(if (ExportRawIntensityBoxPlot) Try(HTMLli(txt="<a href=\"#RawIntensityBoxPlot\"><b>Raw Intensity Box Plot</b></a>")))
  Try(if (ExportNormalizedIntensityBoxPlot) Try(HTMLli(txt="<a href=\"#NormalizedIntensityBoxPlot\"><b>Normalized Intensity Box Plot</b></a>")))
  Try(if (ExportDesignMatrix) Try(HTMLli(txt="<a href=\"#DesignMatrix\"><b>Design Matrix</b></a>")))
  Try(if (ExportContrastsMatrix) Try(HTMLli(txt="<a href=\"#ContrastsMatrix\"><b>Contrasts Matrix</b></a>")))
  Try(if (ExportMAPlotsContrasts) Try(HTMLli(txt="<a href=\"#MAPlotsContrasts\"><b>M A Plots for Contrasts</b></a>")))
  Try(if (ExportTop50Toptables) Try(HTMLli(txt="<a href=\"#Top50Toptables\"><b>Tables of Top 50 Differentially Expressed Genes</b></a>")))
  Try(if (ExportCompleteToptables) Try(HTMLli(txt="<a href=\"#CompleteToptables\"><b>Complete Tables of Genes Ranked in order of Evidence for Differential Expression</b></a>")))

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.affylmGUIglobals$ttMain))

  if (ExportTargets)
  {
    Try(Targets <- get("Targets",envir=affylmGUIenvironment))
    Try(displayVector <- rep("s",ncol(Targets)+1))
    Try(for (i in (0:ncol(Targets)))
      if (i==0 || colnames(Targets)[i]=="SlideNumber")
        displayVector[i] <- "d")
    Try(TargetsXtable <- xtable(Targets,display=displayVector))
    Try(HTML.title("<a name=\"Targets\">RNA Targets</a>",HR=2))
    Try(print(TargetsXtable,type="html",file=fileNameWithPath,append=TRUE))
  }

  if (ExportNormalizationMethod)
  {
    Try(NormMethod <- get("NormMethod", envir=affylmGUIenvironment))
    Try(HTML.title("<a name=\"NormalizationMethod\">Normalization method</a>",HR=2))
    Try(if (NormMethod=="RMA")
      Try(HTMLli(txt="<b>RMA (Robust Multiarray Averaging)</b>")) else
        Try(HTMLli(txt="<b>PLM (Robust Probe-level Linear Model)</b>")))
  }

  if (ExportRawIntensityBoxPlot)
  {
    Try(HTML.title("<a name=\"RawIntensityBoxPlot\">Raw (Unnormalized) Intensity Box Plot</a>",HR=2))
    Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
    Try(SlideNamesVec  <- get("SlideNamesVec",envir=affylmGUIenvironment))
     Try(plotFunction <- function()
    {
      Try(opar<-par(bg="white",cex=0.7))
      Try(boxplot(RawAffyData,col="red",las=2,names=SlideNamesVec))
      Try(title(plotTitle))
      Try(tmp<-par(opar))
    })
    Try(plotTitle <- "Raw intensity distribution for each slide")
    Try(HTMLplotUsingFunction(Caption = plotTitle, File = fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = "RawIntensityBoxPlot",
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
  }

  if (ExportNormalizedIntensityBoxPlot)
  {
    Try(HTML.title("<a name=\"NormalizedIntensityBoxPlot\">Normalized Intensity Box Plot</a>",HR=2))
    Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
    Try(SlideNamesVec  <- get("SlideNamesVec",envir=affylmGUIenvironment))
		Try(plotFunction <- function()
		{
			Try(opar<-par(bg="white",cex=0.7))
			Try(boxplot(data.frame(exprs(NormalizedAffyData)),col="blue",las=2,names=SlideNamesVec))
			Try(title(plotTitle))
			Try(tmp<-par(opar))
		})
		Try(plotTitle<-"Normalized intensity distribution for each slide")
    Try(HTMLplotUsingFunction(Caption = plotTitle, File = fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = "NormalizedIntensityBoxPlot",
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
  }


  if (ExportDesignMatrix)
  {
    Try(design <- get("design",envir=affylmGUIenvironment))
    Try(displayVector <- rep("g",ncol(design)+1))
    Try(displayVector[0] <- "s")
    Try(DesignXtable <- xtable(design,display=displayVector))
    Try(HTML.title("<a name=\"DesignMatrix\">Design Matrix</a>",HR=2))
    Try(print(DesignXtable,type="html",file=fileNameWithPath,append=TRUE))
  }

  if (ExportContrastsMatrix)
  {
    Try(displayVector <- rep("g",ncol(contrastsMatrix)+1))
    Try(displayVector[0] <- "s")
    Try(ContrastsXtable <- xtable(contrastsMatrix,display=displayVector))
    Try(HTML.title("<a name=\"ContrastsMatrix\">Contrasts Matrix</a>",HR=2))
    Try(print(ContrastsXtable,type="html",file=fileNameWithPath,append=TRUE))
  }

  if (ExportMAPlotsContrasts)
  {
    Try(HTML.title("<a name=\"MAPlotsContrasts\">M A Plots for Contrasts</a>",HR=2))
		Try(A <- rowMeans(exprs(NormalizedAffyData)))
		Try(pch <- 16)
		Try(cex <- 0.2)

    Try(for (contrast in (1:NumContrasts))
    {
      Try(plotTitle<-paste("M A Plot (",ContrastNamesVec[contrast],")",sep=""))
      Try(HTML.title(plotTitle,HR=2))
      Try(M <- fit$coefficients[,contrast])
		  Try(plotFunction <- function()
		  {
		   Try(opar<-par(bg="white"))
		   Try(plot(A,M,pch=pch,cex=cex,xlab="A",ylab="M",main=plotTitle))
		   Try(tmp<-par(opar))
		  })
      Try(HTMLplotUsingFunction(Caption = plotTitle, File = fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("contrastMAplot.",contrast,sep=""),
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
		})
  }

	if (ExportTop50Toptables || ExportCompleteToptables){
		Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
		Try(cdfName <- strsplit(cleancdfname(cdfName(RawAffyData)),"cdf")[[1]])#Get the cdfname from the RawAffyData
		if (!(cdfName %in% .packages(all.available=TRUE))){#then check to see if the cdfname package is available on this computer
			Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))#if it is not available then install it on this computer over the internet from the Bioc repository
			Try(assign("cdfName",cdfName,affylmGUIenvironment))#and assign this cdfname to the  affylmGUI environment
		}#end of if (!(cdfName %in% .packages(all.available=TRUE)))
		Try(cdfenv<-getCdfInfo(RawAffyData))#Now get the cdfenv from the RawAffyData
		Try(genelist <- data.frame(ID=I(ls(cdfenv))))#get the geneID's from cdfenv and put them in the genelist. Note that the genelist = GeneID + GeneSymbol+GeneName
		Try(geneNames   <- get("geneNames"  ,envir=affylmGUIenvironment))#Try and get the geneNames and symbols from the affylmGUI environment
		Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))
		Try(
			if(length(geneNames)==0||length(geneSymbols)==0){
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
				Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))#Get the RawAffyData for this if condition
				Try(cdfName <- strsplit(cleancdfname(RawAffyData@cdfName),"cdf")[[1]])#get the cdfname form the affyData
				if(!(cdfName %in% .packages(all.available=TRUE))){
					Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))###inserted by keith
				}
			Try(
				if( (cdfName %in% .packages(all.available=TRUE)) ){
					Require(cdfName) #load the cdfname into memory
					Try(code2eval <- paste("Try(geneNames <- as.character(unlist(lapply(mget(ls(cdfenv),env=",cdfName,"GENENAME),function(nm) return(paste(nm,collapse=\"; \"))))))",sep=""))
					Try(eval(parse(text=code2eval)))
					Try(assign("geneNames",geneNames,affylmGUIenvironment))
					#get the geneSymbols from the cdfName
					Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(lapply(mget(ls(cdfenv),env=",cdfName,"SYMBOL),function(sym) return(paste(sym,collapse=\"; \"))))))",sep=""))
					Try(eval(parse(text=code2eval)))
					Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))
					Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
					#make up the complete genelist = Id+Symbol+Name
					Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
					Try(colnames(genelist) <- c("ID","Symbol","Name"))
				}else{
					Try(genelist <- data.frame(ID=I(ls(cdfenv))))
					Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
				}
			)
			}else{
				#if the geneNames and geneSymbols exist, then create the genelist
				Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
				Try(colnames(genelist) <- c("ID","Symbol","Name"))
			}#end of else/if(length(geneNames)==0||length(geneSymbols)==0)
		)
	}#end of if (ExportTop50Toptables || ExportCompleteToptables)
	#
	if (ExportTop50Toptables)
	{
		Try(HTML.title(paste("<a name=\"Top50Toptables\">Top 50 Differentially Expressed Genes for each Contrast in Contrasts Parameterization ",
			ContrastParameterizationNamesVec[contrastParameterizationIndex],"</a>",sep=""),HR=2))
		Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
		Try(if (!("Amean" %in% names(fit)))
			fit$Amean <- rowMeans(exprs(NormalizedAffyData)))
		Try(fit$genes <- genelist)

		for (coef in (1:NumContrasts))
		{
			Try(options(digits=3))
			Try(table1 <- topTable2(coef=coef,number=50,genelist=genelist,fit=fit))
			Try(toptableDisplay <- rep("s",ncol(table1)+1))
			Try(toptableDisplay[1] <- "d")
			Try(for (i in (2:(ncol(table1)+1)))
			{
				Try(if (colnames(table1)[i-1]=="M")       toptableDisplay[i] <- "f")
				Try(if (colnames(table1)[i-1]=="A")       toptableDisplay[i] <- "f")
				Try(if (colnames(table1)[i-1]=="t")       toptableDisplay[i] <- "f")
				Try(if (colnames(table1)[i-1]=="P.Value") toptableDisplay[i] <- "e")
				Try(if (colnames(table1)[i-1]=="B") toptableDisplay[i] <- "f")
			})
			Try(toptableXtable <- xtable(table1,display=toptableDisplay))
			Try(HTML.title(paste("Top 50 Differentially Expressed Genes for",ContrastNamesVec[coef]),HR=3))
			Try(print(toptableXtable,type="html",file=fileNameWithPath,append=TRUE))
		}

	}

	if (ExportCompleteToptables)
	{
		Try(HTML.title(paste("<a name=\"CompleteToptables\">Complete Tables of Genes Ranked in order of Evidence for Differential Expression for each contrast in Contrasts Parameterization ",ContrastParameterizationNamesVec[contrastParameterizationIndex],"</a>",sep=""),HR=2))

		Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
		Try(if (!("Amean" %in% names(fit)))
			fit$Amean <- rowMeans(exprs(NormalizedAffyData)))
		Try(fit$genes <- genelist)

		for (coef in (1:NumContrasts))
		{
			Try(options(digits=3))
			Try(table1 <- topTable2(coef=coef,number=nrow(genelist),genelist=genelist,fit=fit))
			Try(ToptableAbsoluteFilename <- paste(HTMLfilePath ,.Platform$file.sep,"CompleteToptable_Contrast",coef,".xls",sep=""))
			Try(ToptableRelativeFilename <- paste(HTMLfileRelativePath ,.Platform$file.sep,"CompleteToptable_Contrast",coef,".xls",sep=""))
			Try(write.table(table1,file=ToptableAbsoluteFilename,quote=FALSE,col.names=NA,sep="\t"))
			Try(HTML.title(paste("Complete Table of Genes Ranked in order of Evidence for Differential Expression for ",ContrastNamesVec[coef]),HR=3))
			Try(HTMLli(txt=paste("<a href=\"",ToptableRelativeFilename,"\"><b>",paste("CompleteToptable_Contrast",coef,".xls",sep=""),"</b></a>",sep="")))
		}
	}


	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	Try(HTMLhr())
	Try(HTMLli(txt="This report was generated by "))
	Try(HTMLli(txt=paste("affylmGUI Version",getPackageVersion("affylmGUI"),"(by James Wettenhall), using")))
	Try(HTMLli(txt=paste("affy Version",getPackageVersion("affy"),"(by Rafael A. Irizarry, Laurent Gautier and Benjamin Bolstad),")))
	Try(HTMLli(txt=paste("affyPLM Version",getPackageVersion("affyPLM"),"(by Benjamin Bolstad),")))
	Try(HTMLli(txt=paste("limma Version",getPackageVersion("limma"),"(by Gordon Smyth),")))
	Try(HTMLli(txt=paste("R2HTML Version",getPackageVersion("R2HTML"),"(by Eric Lecoutre) and ")))
	Try(HTMLli(txt=paste("xtable Version",getPackageVersion("xtable"),"(by David Dahl)")))
	Try(HTMLEndFile())

}#end of ExportHTMLreport <- function()

