
ImageArrayPlot <- function(){
	Try(Targets <- get("Targets", envir=affylmGUIenvironment))
	Try(FileNamesVec <- c())
	Try(
		if("FileName" %in% colnames(Targets)){
			FileNamesVec <- Targets$FileName
		}
	)
  Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
  Try(LocalHScale <- .affylmGUIglobals$Myhscale)
  Try(LocalVScale <- .affylmGUIglobals$Myvscale)
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
  Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Image Array Plot",message="Error: No arrays have been loaded.",
        icon="error",default="ok"))
    return()
  })
  Try(slide <- GetSlideNum())
  Try(if (slide==0) return())
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))

  Try(plotFunction <- function()
  {
    Try(opar<-par(bg="white"))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
    Try(image(1:ncol(RawAffyData),1:nrow(RawAffyData),log2(matrix(intensity(RawAffyData)[,slide],nrow=nrow(RawAffyData))),col=gray(c(0:64)/64),
      xlab=xLabel,ylab=yLabel))
    Try(title(plotTitle))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    Try(tmp<-par(opar))
  })
  Try(plotTitle<-SlideNamesVec[slide])
	Try(plotTitle<-paste("Image Array for ",SlideNamesVec[slide]," - ",FileNamesVec[slide]))
  Try(plotLabels <- GetPlotLabels(plotTitle,"",""))
  Try(if (length(plotLabels)==0) return())
  Try(plotTitle <- plotLabels$plotTitle)
  Try(xLabel    <- plotLabels$xLabel)
  Try(yLabel    <- plotLabels$yLabel)

Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  Try(WhetherToUseRplot <- tclvalue(tkmessageBox(title="Where To Plot Array Image",type="yesnocancel",
    message="Plot this image in R rather than a new (Tk) window? (Requires less memory.)",icon="question")))
  else
    Try(WhetherToUseRplot <- "yes"))
  Try(if (WhetherToUseRplot=="cancel")
  {
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    return()
  })
  Try(if (WhetherToUseRplot=="yes")
    plotFunction()
  else
  {
    Require("tkrplot")
    Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
    Try(tkwm.withdraw(ttGraph))
    Try(tkwm.title(ttGraph,plotTitle))
    Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    Try(tkwm.title(ttGraph,paste("Image Plot for",SlideNamesVec[slide])))
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

    CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
  })
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
}#end of ImageArrayPlot <- function()

GetWhichProbes <- function(includeBoth=FALSE)
{
	Try(ttGetWhichProbes <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetWhichProbes))
  Try(tkgrab.set(ttGetWhichProbes))
  Try(tkfocus(ttGetWhichProbes))
  Try(tkwm.title(ttGetWhichProbes,"Probe Set"))

	Try(tkgrid(tklabel(ttGetWhichProbes,text="    ")))
	Try(whichProbesTcl <- tclVar("pm"))
  Try(rb1 <- tkradiobutton(ttGetWhichProbes,text="PM probes",variable=whichProbesTcl,value="pm",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb2 <- tkradiobutton(ttGetWhichProbes,text="MM probes",variable=whichProbesTcl,value="mm",font=.affylmGUIglobals$affylmGUIfont2))
	Try(if (includeBoth==TRUE)
	  Try(rb3 <- tkradiobutton(ttGetWhichProbes,text="Both",variable=whichProbesTcl,value="both",font=.affylmGUIglobals$affylmGUIfont2)))
	Try(tkgrid(tklabel(ttGetWhichProbes,text="    "),rb1))
	Try(tkgrid(tklabel(ttGetWhichProbes,text="    "),rb2))
	Try(if (includeBoth==TRUE)
  	Try(tkgrid(tklabel(ttGetWhichProbes,text="    "),rb3)))
  Try(if (includeBoth==TRUE)
  	Try(tkgrid.configure(rb1,rb2,rb3,columnspan=2,sticky="w"))
  else
  	Try(tkgrid.configure(rb1,rb2,columnspan=2,sticky="w")))
	Try(tkgrid(tklabel(ttGetWhichProbes,text="    "),tklabel(ttGetWhichProbes,text="    ")))

	Try(ReturnVal <- "")
	Try(onCancel <- function() {Try(ReturnVal <<- "");Try(tkgrab.release(ttGetWhichProbes));Try(tkdestroy(ttGetWhichProbes));Try(tkfocus(.affylmGUIglobals$ttMain))})
	Try(onOK <- function() {Try(ReturnVal <<- tclvalue(whichProbesTcl));Try(tkgrab.release(ttGetWhichProbes));Try(tkdestroy(ttGetWhichProbes));Try(tkfocus(.affylmGUIglobals$ttMain))})

	Try(OK.but     <- tkbutton(ttGetWhichProbes,text="OK",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttGetWhichProbes,text="Cancel",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))

	Try(tkgrid(tklabel(ttGetWhichProbes,text="    "),OK.but,Cancel.but,tklabel(ttGetWhichProbes,text="    ")))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttGetWhichProbes,text="    ")))

	Try(tkbind(ttGetWhichProbes,"<Destroy>",function() {ReturnVal <- "";Try(tkgrab.release(ttGetWhichProbes));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(Cancel.but, "<Return>",onCancel))

	Try(tkwait.window(ttGetWhichProbes))

	return (ReturnVal)
}

GetLogPLMDataChoice <- function(){
	Try(ttLogPLMDataChoice <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttLogPLMDataChoice))
	Try(tkgrab.set    (ttLogPLMDataChoice))
	Try(tkfocus       (ttLogPLMDataChoice))
	Try(tkwm.title    (ttLogPLMDataChoice,"Log PLM Data Choice"))
	#
	Try(tkgrid(tklabel(ttLogPLMDataChoice,text="    ")))
	Try(LogPLMDataChoiceTcl <- tclVar("TRUE"))
  Try(rb1 <- tkradiobutton(ttLogPLMDataChoice,text="Log PLM Data",       variable=LogPLMDataChoiceTcl,value="TRUE", font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb2 <- tkradiobutton(ttLogPLMDataChoice,text="Do not Log PLM Data",variable=LogPLMDataChoiceTcl,value="FALSE",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttLogPLMDataChoice,text="    "),rb1))
	Try(tkgrid(tklabel(ttLogPLMDataChoice,text="    "),rb2))
  Try(tkgrid.configure(rb1,rb2,columnspan=2,sticky="w"))
	Try(tkgrid(tklabel      (ttLogPLMDataChoice,text="    "),tklabel(ttLogPLMDataChoice,text="    ")))
	#
	Try(ReturnVal <- "")
	Try(
		onCancel <- function(){
		Try(ReturnVal <<- "");
		Try(tkgrab.release(ttLogPLMDataChoice));
		Try(tkdestroy(ttLogPLMDataChoice));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	Try(
		onOK <- function(){
			Try(ReturnVal <<- tclvalue(LogPLMDataChoiceTcl));
			Try(tkgrab.release(ttLogPLMDataChoice));
			Try(tkdestroy(ttLogPLMDataChoice));
			Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	Try(OK.but     <- tkbutton(ttLogPLMDataChoice,text="OK",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttLogPLMDataChoice,text="Cancel",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel  (ttLogPLMDataChoice,text="    "),OK.but,Cancel.but,tklabel(ttLogPLMDataChoice,text="    ")))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel  (ttLogPLMDataChoice,text="    ")))
	#
	Try(
		tkbind(ttLogPLMDataChoice,
			"<Destroy>",
			function(){
				ReturnVal <- "";
				Try(tkgrab.release(ttLogPLMDataChoice));
				Try(tkfocus(.affylmGUIglobals$ttMain));
			}
		)
	)
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	#
	Try(tkwait.window(ttLogPLMDataChoice))
	#
	return (ReturnVal)
}# end of GetLogPLMDataChoice <- function()

IntensityHistogram <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Intensity Histogram(One Slide)",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}#end of if (ArraysLoaded==FALSE)
	)
	Try(slide <- GetSlideNum())
	Try(if (slide==0) return())
	#
	Try(whichProbes <- GetWhichProbes())
	Try(if (whichProbes=="") return())
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(
				if (whichProbes=="pm"){
					Try(hist(log2(pm(RawAffyData[,slide])),breaks=100,col="blue",xlab=xLabel,ylab=yLabel,main=plotTitle))
				}
			)
			Try(
				if (whichProbes=="mm"){
					Try(hist(log2(mm(RawAffyData[,slide])),breaks=100,col="blue",xlab=xLabel,ylab=yLabel,main=plotTitle))
				}
			)
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	Try(
		if (whichProbes=="pm"){
			Try(plotTitle<-paste("PM Intensity distribution for",SlideNamesVec[slide]))
			Try(xLabel <- "log2(PM Intensity)")
		}
	)
	Try(
		if (whichProbes=="mm"){
			Try(plotTitle<-paste("MM Intensity distribution for",SlideNamesVec[slide]))
			Try(xLabel <- "log2(MM Intensity)")
		}
	)
	Try(plotLabels <- GetPlotLabels(plotTitle,xLabel,"Frequency"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)#end of Try
	#
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of IntensityHistogram <- function()

IntensityHistogramAll <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Intensity Histogram(All Slides)",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}#end of if (ArraysLoaded==FALSE)
	)
	#Try(slide <- GetSlideNum())
	#Try(if (slide==0) return())
	#
	Try(whichProbes <- GetWhichProbes())
	Try(if (whichProbes=="") return())
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(
				if (whichProbes=="pm"){
					###Try(hist(log2(pm(RawAffyData[,slide])),breaks=100,col="blue",xlab=xLabel,ylab=yLabel,main=plotTitle))
					Try(hist(log2(pm(RawAffyData)),breaks=100,col="blue",xlab=xLabel,ylab=yLabel,main=plotTitle))
				}
			)
			Try(
				if (whichProbes=="mm"){
					Try(hist(log2(mm(RawAffyData[,slide])),breaks=100,col="blue",xlab=xLabel,ylab=yLabel,main=plotTitle))
				}
			)
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	Try(
		if (whichProbes=="pm"){
			###Try(plotTitle<-paste("PM Intensity distribution for",SlideNamesVec[slide]))
			Try(plotTitle<-paste("PM Intensity distribution for All Slides"))
			Try(xLabel <- "log2(PM Intensity)")
		}
	)
	Try(
		if (whichProbes=="mm"){
			###Try(plotTitle<-paste("MM Intensity distribution for",SlideNamesVec[slide]))
			Try(plotTitle<-paste("MM Intensity distribution for All Slides"))
			Try(xLabel <- "log2(MM Intensity)")
		}
	)
	Try(plotLabels <- GetPlotLabels(plotTitle,xLabel,"Frequency"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)#end of Try
	#
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of IntensityHistogramAll <- function()

DensityPlot <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Density Plot(One Slide)",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}
	)
	Try(slide <- GetSlideNum())
	Try(if (slide==0) return())
	#
	Try(whichProbes <- GetWhichProbes(includeBoth=TRUE))
	Try(if (whichProbes=="") return())
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(plotDensity.AffyBatch(RawAffyData[,slide],which=whichProbes,xlab=xLabel,ylab=yLabel))
			Try(title(plotTitle))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of Try(plotFunction <- function()
	)
	Try(yLabel <- "Density")
	Try(
		if(whichProbes=="both"){
			Try(plotTitle<-paste("Intensity distribution for",SlideNamesVec[slide]))
			Try(xLabel <- "log2(Intensity)")
		}
	)
	Try(
		if(whichProbes=="pm"){
			Try(plotTitle<-paste("PM Intensity distribution for",SlideNamesVec[slide]))
			Try(xLabel <- "log2(PM Intensity)")
		}
	)
	Try(
		if(whichProbes=="mm"){
			Try(plotTitle<-paste("MM Intensity distribution for",SlideNamesVec[slide]))
			Try(xLabel <- "log2(MM Intensity)")
		}
	)
	Try(plotLabels <- GetPlotLabels(plotTitle,xLabel,"Density"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}
	)
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of DensityPlot <- function()

DensityPlotAll <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Density Plot(All Slides)",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}
	)
	#Try(slide <- GetSlideNum())
	#Try(if (slide==0) return())
	#
	Try(whichProbes <- GetWhichProbes(includeBoth=TRUE))
	Try(if (whichProbes=="") return())
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			###Try(plotDensity.AffyBatch(RawAffyData[,slide],which=whichProbes,xlab=xLabel,ylab=yLabel))
			Try(plotDensity.AffyBatch(RawAffyData,which=whichProbes,xlab=xLabel,ylab=yLabel))
			Try(legend(x="topright", inset=0.025,legend=c(affylmGUIenvironment$Targets$Name),lty=1:NumSlides, col = 1:NumSlides))
			Try(title(plotTitle))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of Try(plotFunction <- function()
	)
	Try(yLabel <- "Density")
	Try(
		if(whichProbes=="both"){
			###Try(plotTitle<-paste("Intensity distribution for",SlideNamesVec[slide]))
			Try(plotTitle<-paste("Intensity distribution for All Slides"))
			Try(xLabel <- "log2(Intensity)")
		}
	)
	Try(
		if(whichProbes=="pm"){
			###Try(plotTitle<-paste("PM Intensity distribution for",SlideNamesVec[slide]))
			Try(plotTitle<-paste("PM Intensity distribution for All Slides"))
			Try(xLabel <- "log2(PM Intensity)")
		}
	)
	Try(
		if(whichProbes=="mm"){
			###Try(plotTitle<-paste("MM Intensity distribution for",SlideNamesVec[slide]))
			Try(plotTitle<-paste("MM Intensity distribution for All Slides"))
			Try(xLabel <- "log2(MM Intensity)")
		}
	)
	Try(plotLabels <- GetPlotLabels(plotTitle,xLabel,"Density"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}
	)
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of DensityPlotAll <- function()

RNADigestionPlotAll <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="RNA Digestion Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}#end of if (ArraysLoaded==FALSE)
	)
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(deg <- AffyRNAdeg(RawAffyData,log.it=log.it.choice))
			Try(plotAffyRNAdeg(deg,col=1:8))
			Try(legend(x="topright",inset=0.025,legend=1:NumSlides,col=1:NumSlides,lty=1))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	#Try(plotLabels <- GetPlotLabels(plotTitle,xLabel,"Frequency"))
	#Try(if (length(plotLabels)==0) return())
	#Try(plotTitle <- plotLabels$plotTitle)
	Try(log.it.choice <- GetLogPLMDataChoice()); #This should be TRUE, except set to FALSE if data has zeroes, like the Estrogen test data set.
	Try(
		if(log.it.choice == TRUE){
			Try(plotTitle<-paste("RNA Digestion Plot for All Slides(logged data)"))
		}
	)
	Try(
		if(log.it.choice == FALSE){
			Try(plotTitle<-paste("RNA Digestion Plot for All Slides(UNlogged data)"))
		}
	)
	Try(
		if(log.it.choice == ""){
			return()
		}
	)
	#Try(xLabel    <- plotLabels$xLabel)
	#Try(yLabel    <- plotLabels$yLabel)
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)#end of Try
	#
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of RNADigestionPlotAll <- function()

NUSEPlotAll <- function(){
	#Normalized Unscaled Standard Errors (NUSE) plot.
	#The standard error estimates obtained for each gene on each array from fitPLM
	#are standardized across arrays so that the median standard error for that
	#genes is 1 across all arrays.
	#An array with elevated SEs relative to other arrays is typically of
	#lower quality.	Try(NormMethod <- get("NormMethod", envir=affylmGUIenvironment))
	#This function does not store teh Normalized Data.
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="NUSE Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}else{
			#Try(tkmessageBox(title="527:DEBUG:NUSE Plot",message="Arrays ARE loaded!.",icon="error",default="ok"))
		}#end of if (ArraysLoaded==FALSE)
	)
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	#Try(tkmessageBox(title="532:DEBUG:NUSE Plot",message=paste("LocalHScale = ",LocalHScale),icon="error",default="ok"))###DEBUG
	Try(PsetData.Available <- get("PsetData.Available" , envir=affylmGUIenvironment))
	if(!PsetData.Available){
		Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
		Require("affyPLM")
		Try(Pset <- fitPLM(RawAffyData))
		Try(assign("Pset",Pset,affylmGUIenvironment))
		Try(assign("weightsPLM",Pset@weights,affylmGUIenvironment))
		Try(assign("PsetData.Available",TRUE,affylmGUIenvironment))
	}else{
		Try(Pset <- get("Pset", envir=affylmGUIenvironment))
	}
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(NUSE(Pset, main = plotTitle))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	Try(plotTitle<-paste("NUSE Plot of All Slides"))
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)#end of Try
	#
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of NUSEPlotAll <- function()

RLEPlotAll <- function(){
	#Relative Log Expression (RLE) values.
	#RLE values are computed for each probeset by comparing the expression value
	#on each array against the median expression value for that probeset across all arrays.
	#Assuming that most genes are not changing in expression across arrays means ideally
	#most of these RLE values will be near 0.
	#When examining this plot focus should be
	#on the shape and position of each of the boxes.
	#Typically arrays with poorer quality
	#show up with boxes that are not centered about 0 and/or are more spread out.
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="RLE Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}#end of if (ArraysLoaded==FALSE)
	)
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(PsetData.Available <- get("PsetData.Available" , envir=affylmGUIenvironment))
	if(!PsetData.Available){
		Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
		Require("affyPLM")
		Try(Pset <- fitPLM(RawAffyData))
		Try(assign("Pset",Pset,affylmGUIenvironment))
		Try(assign("weightsPLM",Pset@weights,affylmGUIenvironment))
		Try(assign("PsetData.Available",TRUE,affylmGUIenvironment))
	}else{
		Try(Pset <- get("Pset", envir=affylmGUIenvironment))
	}
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(RLE(Pset, main = plotTitle))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	Try(plotTitle<-paste("RLE Plot of All Slides"))
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10)  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)#end of Try
	#
	CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
}#end of RLEPlotAll <- function()

RawIntensityBoxPlot <- function()
{
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
  Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
  Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
  Try(LocalHScale <- .affylmGUIglobals$Myhscale)
  Try(LocalVScale <- .affylmGUIglobals$Myvscale)
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Raw Intensity Box Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
    return()
  })
  Try(plotFunction <- function()
  {
    Try(opar<-par(bg="white",cex=0.7))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
    Try(boxplot(RawAffyData,col="red",las=2,names=SlideNamesVec))
    Try(title(plotTitle))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    Try(tmp<-par(opar))
  })
  Try(plotTitle <- "Raw Intensity Box Plot for each array")
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  {
    Require("tkrplot")
    CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
    Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
    Try(tkwm.withdraw(ttGraph))
    Try(tkwm.title(ttGraph,plotTitle))
    Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    Try(tkwm.title(ttGraph,plotTitle))
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
  }
  else
  {
    Try(plot.new())
    Try(plotFunction())
  })

}

NormalizedIntensityBoxPlot <- function()
{
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
  Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Normalized Intensity Box Plot",message="Error: No arrays have been loaded.",
        icon="error",default="ok"))
    return()
  })
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
  Try(if (NormalizedAffyData.Available==FALSE)
    NormalizeNow())
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
  Try(if (NormalizedAffyData.Available==FALSE)
  {
    tkmessageBox(title="Normalized Intensity Box Plot",message="An error occured while trying to normalize the data.")
    return()

  })
  Try(NormalizedAffyData <- get("NormalizedAffyData", envir=affylmGUIenvironment))
  Try(LocalHScale <- .affylmGUIglobals$Myhscale)
  Try(LocalVScale <- .affylmGUIglobals$Myvscale)
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Normalized Intensity Box Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
    return()
  })
  Try(plotFunction <- function()
  {
    Try(opar<-par(bg="white",cex=0.7))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
    Try(boxplot(data.frame(exprs(NormalizedAffyData)),col="blue",las=2,names=SlideNamesVec))
    Try(title(plotTitle))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    Try(tmp<-par(opar))
  })
  Try(plotTitle<-"Normalized Intensity Box Plot for each array")
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  {
    Require("tkrplot")
    CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
    Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
    Try(tkwm.withdraw(ttGraph))
    Try(tkwm.title(ttGraph,plotTitle))
    Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    Try(tkwm.title(ttGraph,plotTitle))
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
  }
  else
  {
    Try(plot.new())
    Try(plotFunction())
  })

}


# The idea of having this function below came about from allowing customized menu items.
# Perhaps one reason it is not widely used is that the code specific to one particular
# plotting function may include asking the user a question via dialog box, but we may
# not want this question to be asked again if they select "Copy" to clipboard or "Save"
# i.e. replot
generalPlotFunction <- function(code="",WindowTitle="")
{

  Try(plotTitle <- WindowTitle)
  Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.withdraw(ttGraph))
  Try(tkwm.title(ttGraph,plotTitle))

  Try(e1 <- try(parse(text=code)))
  if (inherits(e1, "try-error"))
  {
    Try(tkmessageBox(message="Syntax error",icon="error"))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    return()
  }
  e2 <- try(print(eval(e1,envir=affylmGUIenvironment)))
  if (inherits(e2, "try-error"))
  {
    Try(tkmessageBox(message="An error occured while trying to plot the graph(s) for your R code",icon="error"))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
    return()
  }

  Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  {
    Require("tkrplot")
    CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
    Try(plotFunction <- get("plotFunction",envir=affylmGUIenvironment))
    Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=1,vscale=1))
    Try(tkwm.title(ttGraph,plotTitle))
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
  }
  else
  {
    Try(plot.new())
    Try(plotFunction())
  })

}


GetPlotLabels <- function(plottitle="",xlabel="",ylabel="")
{
  Try(ttGetPlotLabels<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetPlotLabels))
  Try(tkgrab.set(ttGetPlotLabels))
  Try(tkfocus(ttGetPlotLabels)  )
  Try(tkwm.title(ttGetPlotLabels,"Plot title and axis labels"))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    ")))
  Try(TitleTcl <- tclVar(init=plottitle))
  Try(entry.Title<-tkentry(ttGetPlotLabels,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=TitleTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="Plot Title : ",font=.affylmGUIglobals$affylmGUIfont2),entry.Title))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    ")))
  Try(xLabelTcl <- tclVar(init=xlabel))
  Try(entry.xLabel<-tkentry(ttGetPlotLabels,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=xLabelTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="X Axis Label : ",font=.affylmGUIglobals$affylmGUIfont2),entry.xLabel))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    ")))
  Try(yLabelTcl <- tclVar(init=ylabel))
  Try(entry.yLabel<-tkentry(ttGetPlotLabels,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=yLabelTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="Y Axis Label :   ",font=.affylmGUIglobals$affylmGUIfont2),entry.yLabel))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    ")))

  Try(tkgrid.configure(entry.Title,entry.xLabel,entry.yLabel,columnspan=2))
  Try(ReturnVal <- list())
  Try(onOK <- function()
  {
      Try(plotTitle <- tclvalue(TitleTcl))
      Try(xLabel <- tclvalue(xLabelTcl))
      Try(yLabel <- tclvalue(yLabelTcl))
      Try(Try(tkgrab.release(ttGetPlotLabels)))
      Try(tkdestroy(ttGetPlotLabels))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      Try(ReturnVal <<- list(plotTitle=plotTitle,xLabel=xLabel,yLabel=yLabel))
  })
  onCancel <- function() {Try(tkgrab.release(ttGetPlotLabels));Try(tkdestroy(ttGetPlotLabels));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotLabels,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotLabels,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    "),OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttGetPlotLabels,text="    ")))
  Try(tkbind(ttGetPlotLabels, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotLabels));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkfocus(ttGetPlotLabels))
  Try(tkwait.window(ttGetPlotLabels))

  return (ReturnVal)
}

GetPlotTitle <- function(plottitle="")
{
  ttGetPlotTitle<-tktoplevel(.affylmGUIglobals$ttMain)
  tkwm.deiconify(ttGetPlotTitle)
  tkgrab.set(ttGetPlotTitle)
  tkfocus(ttGetPlotTitle)
  tkwm.title(ttGetPlotTitle,"Plot title")
  tkgrid(tklabel(ttGetPlotTitle,text="    "))
  TitleTcl <- tclVar(init=plottitle)
  entry.Title<-tkentry(ttGetPlotTitle,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=TitleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotTitle,text="Plot Title : ",font=.affylmGUIglobals$affylmGUIfont2),entry.Title)
  tkgrid(tklabel(ttGetPlotTitle,text="    "))

  tkgrid.configure(entry.Title,columnspan=2)
  ReturnVal <- list()
  onOK <- function()
  {
      plotTitle <- tclvalue(TitleTcl)
      Try(tkgrab.release(ttGetPlotTitle))
      Try(tkdestroy(ttGetPlotTitle))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      ReturnVal <<- list(plotTitle=plotTitle)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetPlotTitle));Try(tkdestroy(ttGetPlotTitle));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotTitle,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotTitle,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  tkgrid(tklabel(ttGetPlotTitle,text="    "),OK.but,Cancel.but)
  tkgrid(tklabel(ttGetPlotTitle,text="    "))
  Try(tkbind(entry.Title, "<Return>",onOK))
  Try(tkbind(ttGetPlotTitle, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotTitle));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkfocus(ttGetPlotTitle))
  Try(tkwait.window(ttGetPlotTitle))

  return (ReturnVal)
}


GetPlotSize <- function()
{
  Try(Myhscale <- .affylmGUIglobals$Myhscale)
  Try(Myvscale <- .affylmGUIglobals$Myvscale)
  ttGetPlotSize<-tktoplevel(.affylmGUIglobals$ttMain)
  tkwm.deiconify(ttGetPlotSize)
  tkgrab.set(ttGetPlotSize)
  tkfocus(ttGetPlotSize)
  tkwm.title(ttGetPlotSize,"Plot size")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  tkgrid(tklabel(ttGetPlotSize,text="If desired, you may adjust the horizontal and vertical size of the plot.",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  HScaleTcl <- tclVar(paste(Myhscale))
  entry.HScale<-tkentry(ttGetPlotSize,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=HScaleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotSize,text="Horizontal Scaling Factor : ",font=.affylmGUIglobals$affylmGUIfont2),entry.HScale,sticky="w")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  VScaleTcl <- tclVar(paste(Myvscale))
  entry.VScale<-tkentry(ttGetPlotSize,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=VScaleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotSize,text="Vertical Scaling Factor :   ",font=.affylmGUIglobals$affylmGUIfont2),entry.VScale,sticky="w")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  ReturnVal <- 0
  HScale <- 0
  VScale <- 0
  onOK <- function()
  {
      HScale <<- as.numeric(tclvalue(HScaleTcl))
      VScale <<- as.numeric(tclvalue(VScaleTcl))
      Try(tkgrab.release(ttGetPlotSize))
      Try(tkdestroy(ttGetPlotSize))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      ReturnVal <<- list(HScale=HScale,VScale=VScale)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetPlotSize));Try(tkdestroy(ttGetPlotSize));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotSize,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotSize,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  Try(tkfocus(ttGetPlotSize))
  Try(tkbind(ttGetPlotSize, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotSize));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkbind(entry.HScale, "<Return>",function() tkfocus(entry.VScale)))
  Try(tkbind(entry.VScale, "<Return>", onOK))
  Try(tkwait.window(ttGetPlotSize))

  return (ReturnVal)
}

SaveGraphAsJpeg <- function(initialfile,plotFunction)
{
  Try(jpegFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{JPEG Files} {.jpg .jpeg}} {{All files} *}"))  )
  if (!nchar(jpegFileName))
    return()
  Try(len <- nchar(jpegFileName))
  if (len<4)
      Try(jpegFileName <- paste(jpegFileName,".jpg",sep=""))
  else if   ((tolower(substring(jpegFileName,len-3,len))!=".jpg") &&
  (len<5 || (tolower(substring(jpegFileName,len-4,len))!=".jpeg")))
        Try(jpegFileName <- paste(jpegFileName,".jpg",sep=""))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
  {
    Try(jpegParams <- GetJpegOrPngX11Params(graphFileType="JPEG"))
    Try(bitmap(file=jpegFileName,bg=jpegParams$bg,res=jpegParams$res,type="jpeg"))
  }
  else
  {
    Try(jpegParams <- GetJpegOrPngParams(graphFileType="JPEG"))
    if (length(jpegParams)==0) return()
    Try(jpeg(file=jpegFileName,width=jpegParams$width,height=jpegParams$height,pointsize=jpegParams$pointsize,bg=jpegParams$bg))
  })
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPNG <- function(initialfile,plotFunction)
{
  Try(pngFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{PNG Files} {.png}} {{All files} *}"))  )
  if (!nchar(pngFileName))
    return()
  Try(len <- nchar(pngFileName))
  if (len<4)
      Try(pngFileName <- paste(pngFileName,".png",sep=""))
  else if   ((tolower(substring(pngFileName,len-3,len))!=".png"))
        Try(pngFileName <- paste(pngFileName,".png",sep=""))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
  {
    Try(pngParams <- GetJpegOrPngX11Params(graphFileType="PNG"))
    Try(bitmap(file=pngFileName,bg=pngParams$bg,res=pngParams$res))
  }
  else
  {
    Try(pngParams <- GetJpegOrPngParams(graphFileType="PNG"))
    if (length(pngParams)==0) return()
    Try(png(file=pngFileName,width=pngParams$width,height=pngParams$height,pointsize=pngParams$pointsize,bg=pngParams$bg))
  })
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPostscript <- function(initialfile,plotFunction)
{
  Try(psFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{Postscript Files} {.ps .eps}} {{All files} *}"))  )
  if (!nchar(psFileName))
    return()
  Try(len <- nchar(psFileName))
  if (len<2)
      Try(psFileName <- paste(psFileName,".ps",sep=""))
  else if   ((tolower(substring(psFileName,len-2,len))!=".ps"))
        Try(psFileName <- paste(psFileName,".ps",sep=""))

  Try(postscript(file=psFileName,title=substring(psFileName,1,nchar(psFileName)-3)))
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPDF <- function(initialfile,plotFunction)
{
  Try(pdfFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{PDF Files} {.pdf}} {{All files} *}"))  )
  if (!nchar(pdfFileName))
    return()
  Try(len <- nchar(pdfFileName))
  if (len<2)
      Try(pdfFileName <- paste(pdfFileName,".pdf",sep=""))
  else if   ((tolower(substring(pdfFileName,len-3,len))!=".pdf"))
        Try(pdfFileName <- paste(pdfFileName,".pdf",sep=""))

  Try(pdf(file=pdfFileName,title=substring(pdfFileName,1,nchar(pdfFileName)-4)))
  Try(plotFunction())
  Try(dev.off())
}


Resize <- function(img,plotFunction)
{
  Try(PlotSize <- GetPlotSize())
  Try(if (length(PlotSize)==0)      return())
  Try(LocalHScale <<- PlotSize$HScale)
  Try(LocalVScale <<- PlotSize$VScale)
  Try(tkconfigure(img,cursor="watch"))
  Try(tkfocus(img))
  Try(tkrreplot(img,fun=plotFunction,hscale=LocalHScale,vscale=LocalVScale))
  Try(tkconfigure(img,cursor="arrow"))
}

CopyGraph <- function(img) Try(tkrreplot(img))

SetupPlotKeyBindings <- function(tt,img)
{
  Try(tkbind(tt, "<Control-C>", function() CopyGraph(img)))
  Try(tkbind(tt, "<Control-c>", function() CopyGraph(img)))
}

SetupPlotMenus <- function(tt,initialfile,plotFunction,img)
{
  Try(topMenu <- tkmenu(tt))
  Try(tkconfigure(tt, menu=topMenu))
  Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(resizeMenu <- tkmenu(topMenu, tearoff=FALSE))

  Try(tkadd(fileMenu, "command", label="Save As PNG",command=function() SaveGraphAsPNG(initialfile=initialfile,plotFunction=plotFunction)))
  Try(tkadd(fileMenu, "command", label="Save As JPEG",command=function() SaveGraphAsJpeg(initialfile=initialfile,plotFunction=plotFunction)))
  Try(tkadd(fileMenu, "command", label="Save As Postscript",command=function() SaveGraphAsPostscript(initialfile=initialfile,plotFunction=plotFunction)))
  Try(tkadd(fileMenu, "command", label="Save As PDF",command=function() SaveGraphAsPDF(initialfile=initialfile,plotFunction=plotFunction)))
  Try(tkadd(fileMenu, "separator"))
  Try(tkadd(fileMenu, "command", label="Close",command=function() tkdestroy(tt)))
  Try(tkadd(topMenu, "cascade", label="File",menu=fileMenu))

  Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",command=function() CopyGraph(img=img)))
  Try(tkadd(topMenu, "cascade", label="Edit", menu=editMenu))

  Try(tkadd(resizeMenu, "command", label="Resize Window",command=function() Resize(img=img,plotFunction=plotFunction)))
  Try(tkadd(topMenu, "cascade", label="Resize", menu=resizeMenu))
  return (list(topMenu=topMenu,fileMenu=fileMenu,editMenu=editMenu,resizeMenu=resizeMenu))
}

GetJpegOrPngParams <- function(graphFileType)
{
  ttGetJpegOrPngParams<-tktoplevel(.affylmGUIglobals$ttMain)
  tkwm.deiconify(ttGetJpegOrPngParams)
  tkgrab.set(ttGetJpegOrPngParams)
  tkfocus(ttGetJpegOrPngParams)
  tkwm.title(ttGetJpegOrPngParams,paste(graphFileType,"Image Parameters"))
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  tkgrid(tklabel(ttGetJpegOrPngParams,text=paste(graphFileType,"Image Parameters"),font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  WidthTcl <- tclVar(paste(600))
  entry.Width<-tkentry(ttGetJpegOrPngParams,width="10",font=.affylmGUIglobals$affylmGUIfont2,textvariable=WidthTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Width   ",font=.affylmGUIglobals$affylmGUIfont2),entry.Width,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  HeightTcl <- tclVar(paste(600))
  entry.Height<-tkentry(ttGetJpegOrPngParams,width="10",font=.affylmGUIglobals$affylmGUIfont2,textvariable=HeightTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Height    ",font=.affylmGUIglobals$affylmGUIfont2),entry.Height,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  BackgroundTcl <- tclVar("white")
  entry.Background<-tkentry(ttGetJpegOrPngParams,width="10",font=.affylmGUIglobals$affylmGUIfont2,textvariable=BackgroundTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Background    ",font=.affylmGUIglobals$affylmGUIfont2),entry.Background,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  PointSizeTcl <- tclVar(paste(12))
  entry.PointSize<-tkentry(ttGetJpegOrPngParams,width="10",font=.affylmGUIglobals$affylmGUIfont2,textvariable=PointSizeTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Font Size    ",font=.affylmGUIglobals$affylmGUIfont2),entry.PointSize,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))

  ReturnVal <- list()
  Width <- 600
  Height <- 600
  Background <- "white"
  PointSize <- 12

  onOK <- function()
  {
      Try(Width  <<- as.numeric(tclvalue(WidthTcl)))
      Try(Height <<- as.numeric(tclvalue(HeightTcl)))
      Try(Background <<- tclvalue(BackgroundTcl))
      Try(PointSize <<- as.numeric(tclvalue(PointSizeTcl)))
      Try(tkgrab.release(ttGetJpegOrPngParams))
      Try(tkdestroy(ttGetJpegOrPngParams))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      Try(ReturnVal <<- list(width=Width,height=Height,pointsize=PointSize,bg=Background))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetJpegOrPngParams));Try(tkdestroy(ttGetJpegOrPngParams));Try(tkfocus(.affylmGUIglobals$ttMain));Try(ReturnVal <<- list())}
  OK.but <-tkbutton(ttGetJpegOrPngParams,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetJpegOrPngParams,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  Try(tkfocus(ttGetJpegOrPngParams))
  Try(tkbind(ttGetJpegOrPngParams, "<Destroy>", function() {Try(tkgrab.release(ttGetJpegOrPngParams));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetJpegOrPngParams))

  return (ReturnVal)
}

GetJpegOrPngX11Params <- function(graphFileType)
{
  ttGetJpegOrPngX11Params<-tktoplevel(.affylmGUIglobals$ttMain)
  tkwm.deiconify(ttGetJpegOrPngX11Params)
  tkgrab.set(ttGetJpegOrPngX11Params)
  tkfocus(ttGetJpegOrPngX11Params)
  tkwm.title(ttGetJpegOrPngX11Params,paste(graphFileType,"Image Parameters"))
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text=paste(graphFileType,"Image Parameters"),font=.affylmGUIglobals$affylmGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  BackgroundTcl <- tclVar("white")
  entry.Background<-tkentry(ttGetJpegOrPngX11Params,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=BackgroundTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="Background    ",font=.affylmGUIglobals$affylmGUIfont2),entry.Background,sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  ResolutionTcl <- tclVar("72")
  entry.Resolution<-tkentry(ttGetJpegOrPngX11Params,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=ResolutionTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="Resolution    ",font=.affylmGUIglobals$affylmGUIfont2),entry.Resolution,sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))

  ReturnVal <- list()
  Background <- "white"
  Resolution <- 72

  onOK <- function()
  {
      Try(Background <<- tclvalue(BackgroundTcl))
      Try(Resolution <<- as.numeric(tclvalue(ResolutionTcl)))
      Try(tkgrab.release(ttGetJpegOrPngX11Params))
      Try(tkdestroy(ttGetJpegOrPngX11Params))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      Try(ReturnVal <<- list(bg=Background,res=Resolution))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetJpegOrPngX11Params));Try(tkdestroy(ttGetJpegOrPngX11Params));Try(tkfocus(.affylmGUIglobals$ttMain));Try(ReturnVal <<- list())}
  OK.but <-tkbutton(ttGetJpegOrPngX11Params,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetJpegOrPngX11Params,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  Try(tkfocus(ttGetJpegOrPngX11Params))
  Try(tkbind(ttGetJpegOrPngX11Params, "<Destroy>", function() {Try(tkgrab.release(ttGetJpegOrPngX11Params));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetJpegOrPngX11Params))

  return (ReturnVal)
}

VennDiagramPlot <- function()
{
  Try(limmaDataSetNameText <-  get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
  Try(design <- get("design", envir=affylmGUIenvironment))

  Try(if (ArraysLoaded==FALSE)
  {
      tkmessageBox(title="Venn Diagram",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.affylmGUIglobals$ttMain))
      return()
  })

  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Venn Diagram",message="There are no contrast parameterizations available.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.affylmGUIglobals$ttMain))
    return()
  })

  Try(contrastParameterizationIndex <- ChooseContrastParameterization())
  Try(if (contrastParameterizationIndex==0)    return())
  Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
  Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
  Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))

  Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)

	Try(if (("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
										length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0)
		Try(ebayesAvailable <- TRUE)
	else
		Try(ebayesAvailable <- FALSE))

  Try(if (ebayesAvailable==FALSE)
  {
    Try(tkmessageBox(title="Venn diagram",message="For now, Venn diagrams are only available when empirical bayes statistics are available (requires replicate arrays).",icon="error"))
    return()
  })

#  Try(eb  <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$eb)
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
  Try(fit <- eBayes(fit))
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))

  Try(Contrasts <- GetMultipleContrasts(contrastParameterizationIndex))
  Try(NumContrastsSelected <- length(Contrasts$contrastIndices))
  Try(if (NumContrastsSelected==0)
    return())

  Try(include <- UpDownOrBoth())
  Try(if (include=="")
    return())

  Try(contrastsMatrix <- c())
  Try(tstats <- c())

  Try(NumParameters <- get("NumParameters" , envir=affylmGUIenvironment))

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))

  Try(for (i in (1:NumContrastsSelected))
  {
    Try(currentIndex <- Contrasts$contrastIndices[[i]])
    Try(tstat <- as.matrix((ContrastParameterizationList[[ContrastParameterizationNameNode]])$eb$t))
    Try(if (ncol(tstat)>1)
      tstat <- tstat[,currentIndex])

    Try(contrastsMatrix <- cbind(contrastsMatrix,as.matrix(rep(0,NumParameters))))
    Try(contrastsMatrix[currentIndex,ncol(contrastsMatrix)] <- 1)
    Try(ContrastName <- ContrastNamesVec[currentIndex])

    Try(if (length(tstats)==0)
      Try(tstats <- as.matrix(tstat))
    else
    {
      Try(tstats <- cbind(tstats,  as.matrix(tstat)))
    })
    Try(colnames(tstats)[ncol(tstats)] <- ContrastName)
  })

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))

  plotFunction <- function()
  {
    Try(opar<-par(bg="white"))
    Try(vennDiagramaffylmGUI(vc,include=include,names=as.vector(setNames),cex=0.85,mar=rep(1,4)))
    Try(TempGraphPar<-par(opar))
  }

  Try(LocalHScale <- .affylmGUIglobals$Myhscale*1.25)
  Try(LocalVScale <- .affylmGUIglobals$Myvscale*1.25)

  # FIXME: It'd be nice to list the one, two or three parameters.
  Try(plotTitle <- paste("Venn diagram for contrast parameterization",ContrastParameterizationNamesVec[contrastParameterizationIndex]))

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
  Try(p.value <- 0.01)
  Try(pvalueText <- GetPValueCutoff(p.value))
  Try(if (pvalueText=="ID_CancelFromGetPValueCutoff") return())
  Try(while (pvalueText=="" || inherits(try(p.value <- eval(parse(text=pvalueText)),TRUE),"try-error"))
  {
    Try(tkmessageBox(title="Invalid P-Value",message="Please enter a valid decimal number for the p-value cutoff.",icon="error",type="ok",default="ok"))
    Try(pvalueText <- GetPValueCutoff())
    Try(if (pvalueText=="ID_CancelFromGetPValueCutoff") return())
  })
  Try(clas <- classifyTestsF(tstats,p.value=p.value))
  Try(vc   <- vennCounts(clas,include=include))
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))

  Try(if (NumContrastsSelected==1)
    Try(setNames <- GetSetNames(numSets=1,set1=colnames(vc)[1])))
  Try(if (NumContrastsSelected==2)
    Try(setNames <- GetSetNames(numSets=2,set1=colnames(vc)[1],set2=colnames(vc)[2])))
  Try(if (NumContrastsSelected==3)
    Try(setNames <- GetSetNames(numSets=3,set1=colnames(vc)[1],set2=colnames(vc)[2],set3=colnames(vc)[3])))

  Try(if (length(setNames)==0) return())

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.affylmGUIglobals$ttMain))

  Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttVennDiagramPlot <- tktoplevel(.affylmGUIglobals$ttMain))
    Try(tkwm.title(ttVennDiagramPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <- tkrplot(ttVennDiagramPlot,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttVennDiagramPlot,img=img))
    Try(SetupPlotMenus(tt=ttVennDiagramPlot,initialfile=paste(limmaDataSetNameText,"VennDiagram",sep=""),
                 plotFunction=plotFunction,img=img))

    Try(tkgrid(img))
    Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))

    Try(if (as.numeric(tclvalue(tkwinfo("reqheight",img)))<10)  # Nothing plotted.
      Try(tkdestroy(ttVennDiagramPlot))
    else
      Try(tkfocus(ttVennDiagramPlot)))
  }
  else
  {
    Try(plot.new())
    Try(plotFunction())
  })
}


UpDownOrBoth <- function()
{
  Try(ttUpDownOrBoth <- tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.title(ttUpDownOrBoth,"D.E. Genes to Include in Venn Diagram"))
  Try(tkwm.deiconify(ttUpDownOrBoth))
  Try(tkgrab.set(ttUpDownOrBoth))
  Try(tkfocus(ttUpDownOrBoth))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tklabel(ttUpDownOrBoth,text="Which differentially expressed genes should be",font=.affylmGUIglobals$affylmGUIfont2),tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tklabel(ttUpDownOrBoth,text="included in the Venn diagram?",font=.affylmGUIglobals$affylmGUIfont2),tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(UpDownOrBothTcl <- tclVar("both"))
  Try(frame1 <- tkframe(ttUpDownOrBoth,relief="groove",borderwidth="2"))
  Try(tkgrid(tkradiobutton(frame1,text="Up-regulated genes",variable=UpDownOrBothTcl,value="up",font=.affylmGUIglobals$affylmGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Down-regulated genes",variable=UpDownOrBothTcl,value="down",font=.affylmGUIglobals$affylmGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Both",variable=UpDownOrBothTcl,value="both",font=.affylmGUIglobals$affylmGUIfont2),sticky="w"))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),frame1))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkframeOKCancel <- tkframe(ttUpDownOrBoth))
  Try(ReturnVal <- "")
  Try(onOK <- function() { Try(ReturnVal <<- tclvalue(UpDownOrBothTcl)); Try(tkdestroy(ttUpDownOrBoth));Try(tkfocus(.affylmGUIglobals$ttMain))})
  Try(onCancel <- function() { Try(tkdestroy(ttUpDownOrBoth));Try(ReturnVal <- "")})
  Try(OK.but     <- tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,    font=.affylmGUIglobals$affylmGUIfont2))
  Try(Cancel.but <- tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tkframeOKCancel))

  Try(tkbind(ttUpDownOrBoth, "<Destroy>", function() {Try(tkgrab.release(ttUpDownOrBoth));Try(tkfocus(.affylmGUIglobals$ttMain))}))
  Try(tkwait.window(ttUpDownOrBoth))

  return (ReturnVal)
}


vennDiagramaffylmGUI <- function(object,include="both",names,cex=1.5,mar=rep(1,4),...) {
# Plot Venn diagram
# Gordon Smyth and James Wettenhall
# 4 July 2003.  Last modified 23 September 2003.

  if(class(object) != "VennCounts") object <- vennCounts(object,include=include)
  nsets <- ncol(object)-1
  if(nsets > 3) stop("Can't plot Venn diagram for more than 3 sets")
  if(missing(names)) names <- colnames(object)[1:nsets]
  counts <- object[,"Counts"]
  theta <- 2*pi*(1:360)/360
  xcentres <- list(0,c(-1,1),c(-1,1,0))[[nsets]]
  ycentres <- list(0,c(0,0),c(1/sqrt(3),1/sqrt(3),-2/sqrt(3)))[[nsets]]
  r <- c(1.5,1.5,1.5)[nsets]
  xtext <- list(-1.2,c(-1.2,1.2),c(-1.2,1.2,0))[[nsets]]
  ytext <- list(1.8,c(1.8,1.8),c(2.4,2.4,-3))[[nsets]]
  opar <- par(mar=mar)
  plot(x=0,y=0,type="n",xlim=c(-4,4),ylim=c(-4,4),xlab="",ylab="",axes=FALSE,...)
  for(circle in 1:nsets) {
    lines(xcentres[circle]+r*cos(theta),ycentres[circle]+r*sin(theta))
    text(xtext[circle],ytext[circle],names[circle],cex=cex)
  }
  switch(nsets,
    {
      rect(-3,-2.5,3,2.5)
      text(2.3,-2.1,counts[1],cex=cex)
      text(0,0,counts[2],cex=cex)
    }, {
      rect(-3,-2.5,3,2.5)
        text(2.3,-2.1,counts[1],cex=cex)
      text(1.5,0.1,counts[2],cex=cex)
      text(-1.5,0.1,counts[3],cex=cex)
      text(0,0.1,counts[4],cex=cex)
    }, {
      rect(-3,-3.5,3,3.3)
      text(2.5,-3,counts[1],cex=cex)
      text(0,-1.7,counts[2],cex=cex)
      text(1.5,1,counts[3],cex=cex)
      text(.75,-.35,counts[4],cex=cex)
      text(-1.5,1,counts[5],cex=cex)
      text(-.75,-.35,counts[6],cex=cex)
      text(0,.9,counts[7],cex=cex)
      text(0,0,counts[8],cex=cex)
    }
  )
  par(opar)
  invisible()
}


HeatDiagramDialog <- function(parameterName){
	Try(ttHeatDiagramDialog <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.title(ttHeatDiagramDialog,"Heat Diagram Options"))
	Try(tkwm.deiconify(ttHeatDiagramDialog))
	Try(tkgrab.set(ttHeatDiagramDialog))
	Try(tkfocus(ttHeatDiagramDialog))
	Try(tkframe1 <- tkframe(ttHeatDiagramDialog))
	Try(tkgrid(tklabel(tkframe1,text="    ")))
	Try(
		tkgrid(
			tklabel(tkframe1,text="    "),
			tklabel(tkframe1,text="The absolute value of the (moderated) t statistic will be used to plot",font=.affylmGUIglobals$affylmGUIfont2)
		)#end of tkgrid
	)#end of Try
	Try(
		tkgrid(
			tklabel(tkframe1,text="    "),
			tklabel(tkframe1,text=paste("the heat diagram, relative to parameter ",parameterName,".",sep=""),font=.affylmGUIglobals$affylmGUIfont2),
			tklabel(tkframe1,text="    ")
		)#end of tkgrid
	)#end of Try
	Try(tkgrid(tklabel(tkframe1,text="    ")))
	Try(primaryCutoffTcl <- tclVar("4"))
	Try(otherCutoffTcl   <- tclVar("3"))
	Try(entry.primaryCutoff <- tkentry(tkframe1,textvariable=primaryCutoffTcl,bg="white",width=10,font=.affylmGUIglobals$affylmGUIfont2))
	Try(entry.otherCutoff   <- tkentry(tkframe1,textvariable=otherCutoffTcl,  bg="white",width=10,font=.affylmGUIglobals$affylmGUIfont2))
	Try(
		tkgrid(
			tklabel(tkframe1,text="    "),
			tklabel(tkframe1,text=paste("D.E. cutoff for parameter ",parameterName,":   ",sep=""),font=.affylmGUIglobals$affylmGUIfont2),
			entry.primaryCutoff,
			tklabel(tkframe1,text="    ")
		)#end of tkgrid
	)#end of Try
	Try(
		tkgrid(
			tklabel(tkframe1,text="    "),
			tklabel(tkframe1,text="D.E. cutoff for other parameters:   ",font=.affylmGUIglobals$affylmGUIfont2),
			entry.otherCutoff,
			tklabel(tkframe1,text="    ")
		)#end of tkgrid
	)#end of Try
	Try(tkgrid.configure(entry.primaryCutoff,sticky="w"))
	Try(tkgrid.configure(entry.otherCutoff,sticky="w"))
	Try(tkgrid(tklabel(tkframe1,text="    ")))
	Try(tkgrid(tkframe1))
	Try(tkframeOKCancel <- tkframe(ttHeatDiagramDialog))
	ReturnVal <- list()
	onOK <- function(){
		Try(primaryCutoffVal <- as.numeric(tclvalue(primaryCutoffTcl)))
		Try(otherCutoffVal   <- as.numeric(tclvalue(otherCutoffTcl)))
		Try(tkgrab.release(ttHeatDiagramDialog))
		Try(tkdestroy(ttHeatDiagramDialog))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		Try(ReturnVal <<- list(primaryCutoff=primaryCutoffVal,otherCutoff=otherCutoffVal))
	}#end of onOK <- function()
	Try(
		onCancel <- function(){
			Try(tkgrab.release(ttHeatDiagramDialog));
			Try(tkdestroy(ttHeatDiagramDialog));
			Try(tkfocus(.affylmGUIglobals$ttMain));
			Try(ReturnVal <<- list())
		}#end of onCancel <- function()
	)
	Try(OK.but <-tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <-tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(OK.but,Cancel.but))
	Try(tkgrid(tklabel(tkframeOKCancel,text="    ")))
	Try(tkgrid(tkframeOKCancel))
	Try(tkfocus(entry.primaryCutoff))
	Try(tkbind(ttHeatDiagramDialog, "<Destroy>", function() {Try(tkgrab.release(ttHeatDiagramDialog));Try(tkfocus(.affylmGUIglobals$ttMain))}))
	Try(tkwait.window(ttHeatDiagramDialog))
	return (ReturnVal)
}#end of HeatDiagramDialog <- function(parameterName)
#
#
HeatDiagramPlot <- function(){
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Heat Diagram",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}
	)
	Try(
		if (NumContrastParameterizations==0){
			Try(tkmessageBox(title="Heat Diagram",message="There are no contrast parameterizations available.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}
	)
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	Try(if (contrastParameterizationIndex==0) return()) # Cancel
	#
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
	Try(NumContrasts <- length(ContrastNamesVec))
	#
	Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
	Try(if (GetContrastReturnVal$contrastIndex==0) return()) # Cancel
	Try(contrast <- GetContrastReturnVal$contrastIndex)
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
	#
	Try(
		if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
		       length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0){
			Try(ebayesAvailable <- TRUE)
		}else{
			Try(ebayesAvailable <- FALSE)
		}
	)
	Try(
		if (ebayesAvailable==FALSE){
			Try(tkmessageBox(title="Heat diagram",message="For now, heat diagrams are only available when empirical bayes statistics are available (requires replicate arrays).",icon="error"))
			return()
		}
	)
	#
	Try(eb <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$eb)
	#  Try(fit <- eBayes(fit))
	#
	Try(
		if (NumContrasts<=1){
			Try(tkmessageBox(title="Heat Diagram",message="To plot a heat diagram, you need to have more than one contrast, i.e. more than two RNA types.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}
	)
	#
	Try(HeatDiagramOptions <- HeatDiagramDialog(colnames(fit$coefficients)[1]))
	Try(if(length(HeatDiagramOptions)==0)return())
	Try(primaryCutoff <- HeatDiagramOptions$primaryCutoff)
	Try(otherCutoff   <- HeatDiagramOptions$otherCutoff)
	#
	Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
	Try(cdfenv<-getCdfInfo(RawAffyData))
	#
	Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))
	Try(
		if(length(geneSymbols)==0){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch") )
			Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
			Try(dataName <- strsplit(cleancdfname(RawAffyData@cdfName),"cdf")[[1]] )
			Try(availablePackages <- available.packages(contriburl=contrib.url(Biobase::biocReposList())))
			Try(matchIndex <- match(dataName,availablePackages[,"Package"]))
			if (!is.na(matchIndex)){ #ie. if there is a match to this package name
				Try(install.packages(pkgs=dataName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))
				Require(dataName)
				Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(mget(ls(envir=",dataName,"SYMBOL),env=",dataName,"SYMBOL))))",sep=""))
				Try(eval(parse(text=code2eval)))
				Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			}
		}#end of if (length(geneSymbols)==0)
	)
	Try(
		if (length(geneSymbols)>0){
			geneLabels <- geneSymbols
		}else{
			geneLabels <- ls(cdfenv)
		}
	)
	Try(contrastNames <- colnames(eb$t))
	Try(contrastNamesVec <- GetContrastNamesForHeatDiagram(numContrasts=length(contrastNames),ContrastNames=contrastNames))
	Try(
		if (length(contrastNamesVec)==0){
			return()
		}else{
			colnames(eb$t) <- contrastNamesVec # Local assignment only.
		}
	)
	plotFunction <- function(){
		Try(opar<-par(bg="white"))
		Try(
			heatdiagram(abs(eb$t),fit$coefficients,primary=1,
			critical.primary=primaryCutoff,critical.other=otherCutoff,
			names=substr(geneLabels,1,20))
		)
		Try(title(plotTitle))
		Try(TempGraphPar<-par(opar))
	}#end of plotFunction <- function()
	#
	Try(LocalHScale <- .affylmGUIglobals$Myhscale*1.5)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale*0.5)
	Try(plotTitle <- paste("Heat diagram relative to parameter",ContrastNamesVec[contrast]))
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	Try(plotTitleList <- GetPlotTitle(plotTitle))
	Try(if (length(plotTitleList)==0) return())
	Try(plotTitle <- plotTitleList$plotTitle)
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(
		if (.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Try(ttHeatDiagramPlot <- tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.title(ttHeatDiagramPlot,plotTitle))
			Try(Require("tkrplot"))
			Try(img <-tkrplot(ttHeatDiagramPlot,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(SetupPlotKeyBindings(tt=ttHeatDiagramPlot,img=img))
			Try(SetupPlotMenus(tt=ttHeatDiagramPlot,initialfile=paste(limmaDataSetNameText,"HeatDiagram",sep=""),plotFunction=plotFunction,img=img))
			Try(tkgrid(img))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tkfocus(ttHeatDiagramPlot))
		}else{
			Try(plot.new())
			Try(plotFunction())
		}
	)
}#end of HeatDiagramPlot <- function()
#
#
affyPlotMA <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="M A Plot",message="Error: No arrays have been loaded.", icon="error",default="ok"))
			return()
		}
	)
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	#
	Try(SlideNums <- GetSlideNums())
	Try(if (length(SlideNums)==0) return())
	Try(slide1 <- SlideNums$slide1)
	Try(slide2 <- SlideNums$slide2)
	#
	Try(WhetherToNormalize <- tclvalue(tkmessageBox(title="M A Plot",message="Use normalized data?",type="yesnocancel",icon="question")))
	Try(if (WhetherToNormalize=="cancel") return())
	#
	Try(
		if (WhetherToNormalize=="yes"){
			Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
			Try(
				if (NormalizedAffyData.Available==FALSE){
					NormalizeNow()
				}
			)
			Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
			Try(
				if (NormalizedAffyData.Available==FALSE){
					tkmessageBox(title="M A Plot",message="An error occured while trying to normalize the data.")
					return()
				}
			)
			Try(NormalizedAffyData <- get("NormalizedAffyData", envir=affylmGUIenvironment))

			Try(R <- exprs(NormalizedAffyData)[,slide1])  # Using cDNA notation (R for one channel/array, G for the other)
			Try(G <- exprs(NormalizedAffyData)[,slide2])  # Using cDNA notation (R for one channel/array, G for the other)
			pch <- 16
			cex <- 0.2
		}else{
			Try(R <- log2(RawAffyData@exprs[,slide1]))  # Using cDNA notation (R for one channel/array, G for the other)
			Try(G <- log2(RawAffyData@exprs[,slide2]))  # Using cDNA notation (R for one channel/array, G for the other)
			pch <- "."
			cex <- 1
		}
	)
	# R and G are already log2ed.
	M <- R - G
	A <- 0.5*(R+G)
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(plot(A,M,pch=pch,cex=cex,xlab=xLabel,ylab=yLabel))
			Try(title(plotTitle))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	Try(
		if (WhetherToNormalize=="yes"){
			Try(plotTitle<-paste("Normalized M A Plot (",SlideNamesVec[slide1]," vs ",SlideNamesVec[slide2],")",sep=""))
		}else{
			Try(plotTitle<-paste("Raw M A Plot (",SlideNamesVec[slide1]," vs ",SlideNamesVec[slide2],")",sep=""))
		}
	)
	Try(plotLabels <- GetPlotLabels(plotTitle,"A","M"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if(as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}
	)
}#end of affyPlotMA <- function()
#
#
GetSlideNums <- function(){
	Try(SlideNamesVec <- get("SlideNamesVec",envir=affylmGUIenvironment))
	Try(
		if (min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE){
			SlideNamesVec <- paste("Slide",SlideNamesVec)
		}
	)
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	ttGetSlideNum<-tktoplevel(.affylmGUIglobals$ttMain)
	tkwm.deiconify(ttGetSlideNum)
	tkgrab.set(ttGetSlideNum)
	tkfocus(ttGetSlideNum)
	tkwm.title(ttGetSlideNum,"Please Specify Slides To Compare")
	TclRequire("BWidget")
	combo1<-tkwidget(ttGetSlideNum,"ComboBox",background="white",editable=FALSE,font=.affylmGUIglobals$affylmGUIfont2)
	combo2<-tkwidget(ttGetSlideNum,"ComboBox",background="white",editable=FALSE,font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttGetSlideNum,text="    "))
	lbl2<-tklabel(ttGetSlideNum,text="Choose a pair of slides to compare",font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttGetSlideNum,text="    "),lbl2,sticky="w")
	tkgrid(tklabel(ttGetSlideNum,text="    "))
	tkgrid(tklabel(ttGetSlideNum,text="    "),combo1,tklabel(ttGetSlideNum,text="    "),combo2,tklabel(ttGetSlideNum,text="    "))
	#
	tkgrid(tklabel(ttGetSlideNum,text="    "))
	tkconfigure(combo1,values=SlideNamesVec)
	tkconfigure(combo2,values=SlideNamesVec)
	#
	tkgrid(tklabel(ttGetSlideNum,text="    "))
	ReturnVal <- list()
	onOK <- function(){
		slidenum1 <- as.numeric(tclvalue(tcl(combo1,"getvalue")))+1
		slidenum2 <- as.numeric(tclvalue(tcl(combo2,"getvalue")))+1
		Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal <<- list(slide1=slidenum1,slide2=slidenum2)
	}#end of onOK <- function()
	onCancel <- function(){
		Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(.affylmGUIglobals$ttMain)); ReturnVal <<- list()
	}
	OK.but <-tkbutton(ttGetSlideNum,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttGetSlideNum,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	tkgrid(tklabel(ttGetSlideNum,text="    "),OK.but,Cancel.but,tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
	tkgrid.configure(OK.but,sticky="e")
	tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
		tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	Try(tkfocus(ttGetSlideNum))
	Try(tkbind(ttGetSlideNum, "<Destroy>", function() {Try(tkgrab.release(ttGetSlideNum));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttGetSlideNum))
	#
	return (ReturnVal)
}#end of GetSlideNums <- function()
#
#
affyPlotMAcontrast <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	#
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="M A Plot",message="Error: No arrays have been loaded.", icon="error",default="ok"))
			return()
		}
	)
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	#
	Try(
		if (NumContrastParameterizations==0){
			Try(tkmessageBox(title="M A Plot",message="There are no contrast parameterizations available.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
			Try(tkfocus(.affylmGUIglobals$ttMain))
			return()
		}
	)
	#
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	Try(if (contrastParameterizationIndex==0) return()) # Cancel
	#
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
	Try(NumContrasts <- length(ContrastNamesVec))
	#
	Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
	Try(if (GetContrastReturnVal$contrastIndex==0) return()) # Cancel
	Try(contrast <- GetContrastReturnVal$contrastIndex)
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	Try(GeneLabelsOptions <- GetGeneLabelsOptions())
	Try(if(length(GeneLabelsOptions)==0) return())
	Try(numDEgenesLabeled   <- GeneLabelsOptions$HowManyDEGeneLabels)
	Try(GeneLabelsMaxLength <- GeneLabelsOptions$GeneLabelsMaxLength)
	Try(IDorSymbol <- GeneLabelsOptions$IDorSymbol)
	#
	Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
	#
	Try(
		if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
				length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0){
			Try(ebayesAvailable <- TRUE)
		}else{
			Try(ebayesAvailable <- FALSE)
		}
	)
	Try(
		if(ebayesAvailable==TRUE){
			Try(fit <- eBayes(fit))
		}
	)
	Try(M <- fit$coefficients[,contrast])
	Try(A <- rowMeans(exprs(NormalizedAffyData)))
	Try(pch <- 16)
	Try(cex <- 0.2)
	#
	Try(
		if (numDEgenesLabeled>0){
			Try(
				if (NumContrasts>1){
					Try(
						if(ebayesAvailable==TRUE){
							Try(ord <- order(fit$lods[,contrast],decreasing=TRUE))
						}else{
							Try(ord <- order(abs(fit$coef[,contrast]),decreasing=TRUE))
						}
					)
				}
			)
		}else{
			Try(
				if(ebayesAvailable==TRUE){
					Try(ord <- order(fit$lods,decreasing=TRUE))
				}else{
					Try(ord <- order(abs(fit$coef),decreasing=TRUE))
				}
			)
		}
	)
	Try(topGenes <- ord[1:numDEgenesLabeled])
	#
	Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
	Try(cdfenv<-getCdfInfo(RawAffyData))
	#
	Try(genelist <- data.frame(ID=I(ls(cdfenv))))
	#
	Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))
	Try(
		if (length(geneSymbols)==0){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch") )
			Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
			Try(dataName <- strsplit(cleancdfname(RawAffyData@cdfName),"cdf")[[1]] )
			Try(availablePackages <- available.packages(contriburl=contrib.url(Biobase::biocReposList())))
			Try(matchIndex <- match(dataName,availablePackages[,"Package"]))
			if (!is.na(matchIndex)){ #ie. if there is a match to this package name
				Try(install.packages(pkgs=dataName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))
				Require(dataName)
				Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(mget(ls(envir=",dataName,"SYMBOL),env=",dataName,"SYMBOL))))",sep=""))
				Try(eval(parse(text=code2eval)))
				Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			}else{
				Try(genelist <- data.frame(ID=I(ls(cdfenv))))
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			}
		}else{
			Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols)))
			Try(colnames(genelist) <- c("ID","Symbol"))
		}
	)
	Try(
		if (IDorSymbol=="Symbol" && !("Symbol" %in% colnames(genelist))){
			Try(tkmessageBox(title="Symbols Not Available",message="Gene symbols are not available.  Probe set IDs will be used instead.",icon="warning"))
			Try(IDorSymbol <- "ID")
		}
	)
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			Try(plot(A,M,pch=pch,cex=cex,xlab=xLabel,ylab=yLabel))
			Try(title(plotTitle))
			Try(
				if (numDEgenesLabeled>0){
					Try(text(A[topGenes],M[topGenes],labels=substr(genelist[topGenes,IDorSymbol],1,GeneLabelsMaxLength),cex=0.8,col="blue"))
				}
			)
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of Try(plotFunction <- function()
	)
	Try(plotTitle<-paste("M A Plot (",ContrastNamesVec[contrast],")",sep=""))
	Try(plotLabels <- GetPlotLabels(plotTitle,"A","M"))
	Try(if (length(plotLabels)==0) return())
	Try(plotTitle <- plotLabels$plotTitle)
	Try(xLabel    <- plotLabels$xLabel)
	Try(yLabel    <- plotLabels$yLabel)
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Require("tkrplot")
			CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,plotTitle))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
		}else{
			Try(plot.new())
			Try(plotFunction())
		}
	)
}#end of affyPlotMAcontrast <- function()
#
#
GetGeneLabelsOptions <- function()
{
  Try(ttGeneLabelsOptions <- tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGeneLabelsOptions))
  Try(tkgrab.set(ttGeneLabelsOptions))
  Try(tkfocus(ttGeneLabelsOptions))
  Try(tkwm.title(ttGeneLabelsOptions,"D.E. Gene Labels"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
  Try(HowManyDEGenesTcl <- tclVar(paste(10)))
  Try(entry.HowManyDEGenes<-tkentry(ttGeneLabelsOptions,width="12",font=.affylmGUIglobals$affylmGUIfont2,
    textvariable=HowManyDEGenesTcl,bg="white"))
  Try(GeneLabelsMaxLengthTcl <- tclVar(paste(10)))
  Try(entry.GeneLabelsMaxLength<-tkentry(ttGeneLabelsOptions,width="12",font=.affylmGUIglobals$affylmGUIfont2,
    textvariable=GeneLabelsMaxLengthTcl,bg="white"))

  Try(ReturnVal <- list())
  onOK <- function()
  {
    Try(tkgrab.release(ttGeneLabelsOptions))
    Try(tkdestroy(ttGeneLabelsOptions))
    Try(tkfocus(.affylmGUIglobals$ttMain))
    Try(ReturnVal <<- list(HowManyDEGeneLabels=as.integer(tclvalue(HowManyDEGenesTcl)),
                           GeneLabelsMaxLength=as.integer(tclvalue(GeneLabelsMaxLengthTcl)),
                           IDorSymbol=tclvalue(IDorSymbolTcl)))
  }
  onCancel <- function() {Try(tkgrab.release(ttGeneLabelsOptions));Try(tkdestroy(ttGeneLabelsOptions));Try(tkfocus(.affylmGUIglobals$ttMain)); ReturnVal <<- list()}

  Try(OK.but <-tkbutton(ttGeneLabelsOptions,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGeneLabelsOptions,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),tklabel(ttGeneLabelsOptions,text="Please select D.E. gene labeling options.",font=.affylmGUIglobals$affylmGUIfont2),tklabel(ttGeneLabelsOptions,text="    "),sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),tklabel(ttGeneLabelsOptions,text="Number of labeled differentially expressed genes",font=.affylmGUIglobals$affylmGUIfont2),entry.HowManyDEGenes,tklabel(ttGeneLabelsOptions,text="    "),sticky="w"))
  Try(tkgrid.configure(entry.HowManyDEGenes,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),tklabel(ttGeneLabelsOptions,text="Maximum length of gene labels",font=.affylmGUIglobals$affylmGUIfont2),entry.GeneLabelsMaxLength,tklabel(ttGeneLabelsOptions,text="       "),sticky="w"))
  Try(tkgrid.configure(entry.GeneLabelsMaxLength,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
	Try(IDorSymbolTcl <- tclVar("ID"))
  Try(rb1 <- tkradiobutton(ttGeneLabelsOptions,text="Use Probe Set ID",variable=IDorSymbolTcl,value="ID",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb2 <- tkradiobutton(ttGeneLabelsOptions,text="Use Gene Symbol",variable=IDorSymbolTcl,value="Symbol",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),rb1))
	Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),rb2))
	Try(tkgrid.configure(rb1,sticky="w"))
	Try(tkgrid.configure(rb2,sticky="w"))

  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))

  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    ")))

  Try(tkfocus(ttGeneLabelsOptions))

  Try(tkbind(ttGeneLabelsOptions, "<Destroy>", function() {Try(tkgrab.release(ttGeneLabelsOptions));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkwait.window(ttGeneLabelsOptions))

  return(ReturnVal)

}
#
#
QQTplot <- function()
{
  Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
  Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
  Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))

  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Quantile-Quantile t-Statistic Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      return()
  }
  if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Quantile-Quantile t-Statistic Plot",message="There are no contrast parameterizations loaded.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.affylmGUIglobals$ttMain))
    return()
  }

  Try(contrastParameterizationIndex <- ChooseContrastParameterization())
  Try(if (contrastParameterizationIndex==0) return()) # Cancel

  Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
  Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
  Try(NumContrasts <- length(ContrastNamesVec))

  Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
  Try(if (GetContrastReturnVal$contrastIndex==0) return()) # Cancel
  Try(contrast <- GetContrastReturnVal$contrastIndex)
  Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))

  Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)

	Try(if (("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
										length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0)
		Try(ebayesAvailable <- TRUE)
	else
		Try(ebayesAvailable <- FALSE))

  Try(if (ebayesAvailable==FALSE)
  {
    Try(tkmessageBox(title="QQT plot",message="t statistics are not available because of a lack of replicate arrays.",icon="error"))
    return()
  })


  Try(fit <- eBayes(fit))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
    Try(cex <- 0.3)
  else
    Try(cex <- 0.2))
  plotFunction <- function()
  {
    Try(opar<-par(bg="white"))
    Try(if (NumContrasts>1)
      qqt(fit$t[,contrast],df=fit$df.residual+fit$df.prior,pch=16,cex=cex,main=plotTitle)
    else
      qqt(fit$t,df=fit$df.residual+fit$df.prior,pch=16,cex=cex,main=plotTitle))
    abline(0,1)
    Try(tempGraphPar <- par(opar))
  }
  Try(LocalHScale <- .affylmGUIglobals$Myhscale)
  Try(LocalVScale <- .affylmGUIglobals$Myvscale)
  Try(plotTitle <- ContrastNamesVec[contrast])
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(tkfocus(.affylmGUIglobals$ttMain))
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.affylmGUIglobals$ttMain))

  Try(if (.affylmGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(Require("tkrplot"))
    Try(ttQQTplot <- tktoplevel(.affylmGUIglobals$ttMain))
    Try(tkwm.title(ttQQTplot,plotTitle))
    Try(img <-tkrplot(ttQQTplot,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttQQTplot,img=img))
    Try(SetupPlotMenus(tt=ttQQTplot,initialfile=paste(limmaDataSetNameText,"QQTPlot",ContrastNamesVec[contrast],sep=""),
                 plotFunction=plotFunction,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttQQTplot))
  }
  else
  {
    Try(plot.new())
    Try(plotFunction())
  })
  Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
}
#
#
LogOddsPlot <- function(){
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment))
	Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment))
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	if (ArraysLoaded==FALSE){
		Try(tkmessageBox(title="Log Odds Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	}
	if (NumContrastParameterizations==0){
		Try(tkmessageBox(title="Log Odds Plot",message="There are no contrast parameterizations loaded.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	}
	#
	Try(contrastParameterizationIndex <- ChooseContrastParameterization())
	Try(if (contrastParameterizationIndex==0) return()) # Cancel
	#
	Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex])
	Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))
	Try(NumContrasts <- length(ContrastNamesVec))
	#
	Try(GetContrastReturnVal <- GetContrast(contrastParameterizationIndex))
	Try(if (GetContrastReturnVal$contrastIndex==0) return()) # Cancel
	Try(contrast <- GetContrastReturnVal$contrastIndex)
	Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
	#
	Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
	#
	Try(
		if(("eb" %in% names(ContrastParameterizationList[[contrastParameterizationIndex]]))&&
			   length(ContrastParameterizationList[[contrastParameterizationIndex]]$eb)>0){
			Try(ebayesAvailable <- TRUE)
		}else{
			Try(ebayesAvailable <- FALSE)
		}
	)
	Try(
		if(ebayesAvailable==FALSE){
			Try(tkmessageBox(title="Log Odds Plot",message="Log Odds (B statistic) values are not available because of a lack of replicate arrays.",icon="error"))
			return()
		}
	)
	Try(fit <- eBayes(fit))
	#
	Try(GeneLabelsOptions <- GetGeneLabelsOptions())
	Try(if(length(GeneLabelsOptions)==0) return())
	Try(numDEgenesLabeled   <- GeneLabelsOptions$HowManyDEGeneLabels)
	Try(GeneLabelsMaxLength <- GeneLabelsOptions$GeneLabelsMaxLength)
	Try(IDorSymbol <- GeneLabelsOptions$IDorSymbol)
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(
		if (numDEgenesLabeled>0){
			Try(
				if(NumContrasts>1){
					Try(ord <- order(fit$lods[,contrast],decreasing=TRUE))
				}else{
					Try(ord <- order(fit$lods,decreasing=TRUE))
				}
			)
			Try(topGenes <- ord[1:numDEgenesLabeled])
		}
	)
	Try(
		if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows"){
			Try(cex <- 0.3)
		}else{
			Try(cex <- 0.2)
		}
	)
	Try(
		if (numDEgenesLabeled>0){
			Try(
				if (NumContrasts>1){
					Try(ord <- order(fit$lods[,contrast],decreasing=TRUE))
				}else{
					Try(ord <- order(fit$lods,decreasing=TRUE))
				}
			)
			Try(topGenes <- ord[1:numDEgenesLabeled])
			#
			Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
			Try(cdfenv      <-getCdfInfo(RawAffyData))
			Try(genelist    <- data.frame(ID=I(ls(cdfenv))))
			Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment))
			Try(
				if (length(geneSymbols)==0){
					Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
					Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
					Try(cdfName <- strsplit(cleancdfname(RawAffyData@cdfName),"cdf")[[1]])
					if(!(cdfName %in% .packages(all.available=TRUE))){
						Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))###inserted by keith
					}
					Try(
						if( (cdfName %in% .packages(all.available=TRUE)) ){
							Require(cdfName)
							Try(code2eval <- paste("Try(geneSymbols <- as.character(unlist(mget(ls(envir=",cdfName,"SYMBOL),env=",cdfName,"SYMBOL))))",sep=""))
							Try(eval(parse(text=code2eval)))
							Try(assign("geneSymbols",geneSymbols,affylmGUIenvironment))
							Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
							Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols)))
							Try(colnames(genelist) <- c("ID","Symbol"))
						}else{
							Try(genelist <- data.frame(ID=I(ls(cdfenv))))
							Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
						}
					)
				}else{
					Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols)))
					Try(colnames(genelist) <- c("ID","Symbol"))
				}#end of else/if (length(geneSymbols)==0)
			)
		}#end of if (numDEgenesLabeled>0)
	)
	plotFunction <- function(){
		Try(opar<-par(bg="white"))
		Try(
			if (NumContrasts>1){
				Try(plot(fit$coef[,contrast],fit$lods[,contrast],pch=16,cex=cex,xlab="Log Fold Change",ylab="Log Odds"))
				Try(title(plotTitle))
				Try(
					if (numDEgenesLabeled>0){
						text(fit$coef[topGenes,contrast],fit$lods[topGenes,contrast],labels=substr(genelist[topGenes,IDorSymbol],1,GeneLabelsMaxLength),cex=0.8,col="blue")
					}
				)
			}else{
				Try(plot(fit$coef,fit$lods,pch=16,cex=cex,xlab="Log Fold Change",ylab="Log Odds"))
				Try(title(plotTitle))
				Try(
					if(numDEgenesLabeled>0){
						text(fit$coef[topGenes],fit$lods[topGenes],labels=substring(genelist[topGenes,IDorSymbol],1,10),cex=0.8,col="blue")
					}
				)
			}#end of else/if (NumContrasts>1)
		)
		Try(tempGraphPar <- par(opar))
	}#end of plotFunction <- function()
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(plotTitle <- (ContrastNamesVec[contrast]))
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	Try(plotTitleList <- GetPlotTitle(plotTitle))
	Try(if (length(plotTitleList)==0) return())
	Try(plotTitle <- plotTitleList$plotTitle)
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Try(ttLogOddsPlot <- tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.title(ttLogOddsPlot,plotTitle))
			Try(Require("tkrplot"))
			Try(img <-tkrplot(ttLogOddsPlot,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(SetupPlotKeyBindings(tt=ttLogOddsPlot,img=img))
			Try(
				SetupPlotMenus(
					tt=ttLogOddsPlot,
					initialfile=paste(limmaDataSetNameText,"LogOddsPlot",ContrastNamesVec[contrast],sep=""),
					plotFunction=plotFunction,img=img
				)
			)
			Try(tkgrid(img))
			Try(tkfocus(ttLogOddsPlot))
		}else{
			Try(plot.new())
			Try(plotFunction())
		}#end of else/if(.affylmGUIglobals$graphicsDevice=="tkrplot")
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
}#end of LogOddsPlot <- function()
#
#
ImageQualityWeightPlot <- function(){
	Try(Targets <- get("Targets", envir=affylmGUIenvironment))
	Try(FileNamesVec <- c())
	Try(
		if("FileName" %in% colnames(Targets)){
			FileNamesVec <- Targets$FileName
		}
	)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Image Quality Weights Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}
	)
	Try(PsetData.Available <- get("PsetData.Available" , envir=affylmGUIenvironment))
	if(!PsetData.Available){
		Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
		Require("affyPLM")
		Try(Pset <- fitPLM(RawAffyData))
		Try(assign("Pset",Pset,affylmGUIenvironment))
		Try(assign("weightsPLM",Pset@weights,affylmGUIenvironment))
		Try(assign("PsetData.Available",TRUE,affylmGUIenvironment))
	}else{
		#Try(tkmessageBox(title="2405:DEBUG:",message=paste("PLM model already fitted"),icon="warning",default="ok"))###DEBUG
	}
	Try(Pset <- get("Pset", envir=affylmGUIenvironment))
	Try(weightsPLM <- get("weightsPLM", envir=affylmGUIenvironment))
	#
	Try(slide <- GetSlideNum(all=TRUE))
	Try(if (slide==0) return())
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			if (slide==1000000){
				op <- par(mfrow = c((sqrt(NumSlides) + 1), (sqrt(NumSlides) + 1)),pty = "s",mar=c(0,0,2,0)+0.1)
				image(Pset)#ie do all slides
				par(op)
			}else{
				image(Pset, which = slide, main = plotTitle)
			}
			#pm.index <- unique(unlist(indexProbes(RawAffyData, "pm")))#returns a list with locations of the probes in all probe sets
			#rows <- nrow(RawAffyData)
			#Try(tkmessageBox(title="2412:DEBUG:",message=paste("rows = ",rows),icon="error",default="ok"))###DEBUG
			#cols <- ncol(RawAffyData)
			#Try(tkmessageBox(title="2414:DEBUG:",message=paste("cols = ",cols),icon="error",default="ok"))###DEBUG
			#pm.x.locs <- pm.index%%rows
			#pm.x.locs[pm.x.locs == 0] <- rows
			#pm.y.locs <- pm.index%/%rows + 1
			#xycoor <- matrix(cbind(pm.x.locs,pm.y.locs),ncol=2)
			#Try(tkmessageBox(title="2419:DEBUG:",message=paste("xycoor = ",xycoor),icon="error",default="ok"))###DEBUG
			#xycoor2 <- matrix(cbind(pm.x.locs,pm.y.locs+1),ncol=2)
			#Try(tkmessageBox(title="2421:DEBUG:",message=paste("xycoor2 = ",xycoor2),icon="error",default="ok"))###DEBUG
			#weightmatrix <- matrix(nrow=rows,ncol=cols)
			#weightmatrix[xycoor] <- weightsPLM[,slide]
			#weightmatrix[xycoor2] <- weightsPLM[,slide]
			# this line flips the matrix around so it is correct
			#weightmatrix <- as.matrix(rev(as.data.frame(weightmatrix)))
			#image(weightmatrix,col=terrain.colors(12),xaxt='n',yaxt='n')
			#Try(title(SlideNamesVec[slide]))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	if (slide==1000000){ #ie for all slides
		Try(plotTitle <- "")
		Try(xLabel    <- "")
		Try(yLabel    <- "")
	}else{
		Try(plotTitle<-paste("Weights for ",SlideNamesVec[slide]," - ",FileNamesVec[slide]))
		Try(plotLabels <- GetPlotLabels(plotTitle,"",""))
		Try(if (length(plotLabels)==0) return())
		Try(plotTitle <- plotLabels$plotTitle)
		Try(xLabel    <- plotLabels$xLabel)
		Try(yLabel    <- plotLabels$yLabel)
	}
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Try(WhetherToUseRplot <- tclvalue(tkmessageBox(title="Where To Plot Array Image",type="yesnocancel",
			message="Plot this image in R rather than a new (Tk) window? (Requires less memory.)",icon="question")))
		}else{
			Try(WhetherToUseRplot <- "yes")
		}
	)
	Try(
		if (WhetherToUseRplot=="cancel"){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		}
	)
	Try(
		if (WhetherToUseRplot=="yes"){
			plotFunction()
		}else{
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,paste("Image Plot for",SlideNamesVec[slide])))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
			CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
		}#end of else/if (WhetherToUseRplot=="yes")
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
}#end of ImageQualityWeightPlot <- function()

ImageQualityResidualPlot <- function(){
	Try(Targets <- get("Targets", envir=affylmGUIenvironment))
	Try(FileNamesVec <- c())
	Try(
		if("FileName" %in% colnames(Targets)){
			FileNamesVec <- Targets$FileName
		}
	)
	Try(SlideNamesVec  <- get("SlideNamesVec", envir=affylmGUIenvironment))
	Try(NumSlides <- get("NumSlides",envir=affylmGUIenvironment))
	Try(LocalHScale <- .affylmGUIglobals$Myhscale)
	Try(LocalVScale <- .affylmGUIglobals$Myvscale)
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Image Quality Residuals Plot",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}
	)
	Try(PsetData.Available <- get("PsetData.Available" , envir=affylmGUIenvironment))
	if(!PsetData.Available){
		Try(RawAffyData <- get("RawAffyData", envir=affylmGUIenvironment))
		Require("affyPLM")
		Try(Pset <- fitPLM(RawAffyData))
		Try(assign("Pset",Pset,affylmGUIenvironment))
		Try(assign("weightsPLM",Pset@weights,affylmGUIenvironment))
		Try(assign("PsetData.Available",TRUE,affylmGUIenvironment))
	}else{
		#Try(tkmessageBox(title="2530:DEBUG:",message=paste("No Wait - PLM model previously calculated"),icon="warning",default="ok"))###DEBUG
	}
	Try(Pset <- get("Pset", envir=affylmGUIenvironment))
	Try(weightsPLM <- get("weightsPLM", envir=affylmGUIenvironment))
	#
	Try(slide <- GetSlideNum(all=TRUE))
	Try(if (slide==0) return())
	Try(residType <- GetResidualTypeChoice())
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	#
	Try(
		plotFunction <- function(){
			Try(opar<-par(bg="white"))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
			if (slide==1000000){
				op <- par(mfrow = c((sqrt(NumSlides) + 1), (sqrt(NumSlides) + 1)),pty = "s",mar=c(0,0,2,0)+0.1)
				image(Pset, type = residType)#ie do all slides
				par(op)
			}else{
				image(Pset, which = slide, type = residType, main = plotTitle)
			}
			#pm.index <- unique(unlist(indexProbes(RawAffyData, "pm")))#returns a list with locations of the probes in all probe sets
			#rows <- nrow(RawAffyData)
			#Try(tkmessageBox(title="2412:DEBUG:",message=paste("rows = ",rows),icon="error",default="ok"))###DEBUG
			#cols <- ncol(RawAffyData)
			#Try(tkmessageBox(title="2414:DEBUG:",message=paste("cols = ",cols),icon="error",default="ok"))###DEBUG
			#pm.x.locs <- pm.index%%rows
			#pm.x.locs[pm.x.locs == 0] <- rows
			#pm.y.locs <- pm.index%/%rows + 1
			#xycoor <- matrix(cbind(pm.x.locs,pm.y.locs),ncol=2)
			#Try(tkmessageBox(title="2419:DEBUG:",message=paste("xycoor = ",xycoor),icon="error",default="ok"))###DEBUG
			#xycoor2 <- matrix(cbind(pm.x.locs,pm.y.locs+1),ncol=2)
			#Try(tkmessageBox(title="2421:DEBUG:",message=paste("xycoor2 = ",xycoor2),icon="error",default="ok"))###DEBUG
			#weightmatrix <- matrix(nrow=rows,ncol=cols)
			#weightmatrix[xycoor] <- weightsPLM[,slide]
			#weightmatrix[xycoor2] <- weightsPLM[,slide]
			# this line flips the matrix around so it is correct
			#weightmatrix <- as.matrix(rev(as.data.frame(weightmatrix)))
			#image(weightmatrix,col=terrain.colors(12),xaxt='n',yaxt='n')
			#Try(title(SlideNamesVec[slide]))
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			Try(tmp<-par(opar))
		}#end of plotFunction <- function()
	)
	if (slide==1000000){ #ie for all slides
		Try(plotTitle <- "")
		Try(xLabel    <- "")
		Try(yLabel    <- "")
	}else{
		Try(plotTitle<-paste("Residuals(",residType,") for ",SlideNamesVec[slide]," - ",FileNamesVec[slide]))
		Try(plotLabels <- GetPlotLabels(plotTitle,"",""))
		Try(if (length(plotLabels)==0) return())
		Try(plotTitle <- plotLabels$plotTitle)
		Try(xLabel    <- plotLabels$xLabel)
		Try(yLabel    <- plotLabels$yLabel)
	}
	#
	Try(
		if(.affylmGUIglobals$graphicsDevice=="tkrplot"){
			Try(WhetherToUseRplot <- tclvalue(tkmessageBox(title="Where To Plot Array Image",type="yesnocancel",
			message="Plot this image in R rather than a new (Tk) window? (Requires less memory.)",icon="question")))
		}else{
			Try(WhetherToUseRplot <- "yes")
		}
	)
	Try(
		if (WhetherToUseRplot=="cancel"){
			Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
			return()
		}
	)
	Try(
		if (WhetherToUseRplot=="yes"){
			plotFunction()
		}else{
			Require("tkrplot")
			Try(ttGraph<-tktoplevel(.affylmGUIglobals$ttMain))
			Try(tkwm.withdraw(ttGraph))
			Try(tkwm.title(ttGraph,plotTitle))
			Try(imgaffylmGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
			Try(tkwm.title(ttGraph,paste("Image Plot for",SlideNamesVec[slide])))
			SetupPlotKeyBindings(tt=ttGraph,img=imgaffylmGUI)
			SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgaffylmGUI)
			Try(tkgrid(imgaffylmGUI))
			Try(
				if (as.numeric(tclvalue(tkwinfo("reqheight",imgaffylmGUI)))<10){  # Nothing plotted.
					Try(tkdestroy(ttGraph))
				}else{
					Try(tkwm.deiconify(ttGraph))
					Try(tkfocus(imgaffylmGUI))
				}
			)
			CopyToClip <- function() Try(tkrreplot(imgaffylmGUI))
		}#end of else/if (WhetherToUseRplot=="yes")
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
}#end of ImageQualityResidualPlot <- function()

GetResidualTypeChoice <- function(){
	Try(ttResidualTypeChoice <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttResidualTypeChoice))
	Try(tkgrab.set    (ttResidualTypeChoice))
	Try(tkfocus       (ttResidualTypeChoice))
	Try(tkwm.title    (ttResidualTypeChoice,"Residual Image Type Choice"))
	#
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    ")))
	Try(ttResidualTypeChoiceTcl <- tclVar("resids"))
  Try(rb1 <- tkradiobutton(ttResidualTypeChoice,text="Residuals",       variable=ttResidualTypeChoiceTcl,value="resids", font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb2 <- tkradiobutton(ttResidualTypeChoice,text="Positive Residuals",variable=ttResidualTypeChoiceTcl,value="pos.resids",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb3 <- tkradiobutton(ttResidualTypeChoice,text="Negative Residuals",variable=ttResidualTypeChoiceTcl,value="neg.resids",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rb4 <- tkradiobutton(ttResidualTypeChoice,text="Signed Residuals",variable=ttResidualTypeChoiceTcl,value="sign.resids",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    "),rb1))
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    "),rb2))
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    "),rb3))
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    "),rb4))
  Try(tkgrid.configure(rb1,rb2,rb3,rb4,columnspan=2,sticky="w"))
	Try(tkgrid(tklabel(ttResidualTypeChoice,text="    "),tklabel(ttResidualTypeChoice,text="    ")))
	#
	Try(ReturnVal <- "")
	Try(
		onCancel <- function(){
		Try(ReturnVal <<- "");
		Try(tkgrab.release(ttResidualTypeChoice));
		Try(tkdestroy(ttResidualTypeChoice));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	Try(
		onOK <- function(){
			Try(ReturnVal <<- tclvalue(ttResidualTypeChoiceTcl));
			Try(tkgrab.release(ttResidualTypeChoice));
			Try(tkdestroy(ttResidualTypeChoice));
			Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	Try(OK.but     <- tkbutton(ttResidualTypeChoice,text="OK",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttResidualTypeChoice,text="Cancel",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel  (ttResidualTypeChoice,text="    "),OK.but,Cancel.but,tklabel(ttResidualTypeChoice,text="    ")))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel  (ttResidualTypeChoice,text="    ")))
	#
	Try(
		tkbind(ttResidualTypeChoice,
			"<Destroy>",
			function(){
				ReturnVal <- "";
				Try(tkgrab.release(ttResidualTypeChoice));
				Try(tkfocus(.affylmGUIglobals$ttMain));
			}
		)
	)
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	#
	Try(tkwait.window(ttResidualTypeChoice))
	#
	return (ReturnVal)
}# end of GetResidualTypeChoice <- function()


GetMultipleContrasts <- function(contrastParameterizationIndex)
{
  Try(ttGetMultipleContrasts<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetMultipleContrasts))
  Try(tkgrab.set(ttGetMultipleContrasts)  )
  Try(tkfocus(ttGetMultipleContrasts))
  Try(tkwm.title(ttGetMultipleContrasts,"Choose one or more contrast"))
  Try(scr <- tkscrollbar(ttGetMultipleContrasts, repeatinterval=5,command=function(...)tkyview(tl,...)))
  Try(xscr <- tkscrollbar(ttGetMultipleContrasts, repeatinterval=5,command=function(...)tkxview(tl,...) ,orient="horizontal"))
  ## Safest to make sure scr exists before setting yscrollcommand
  Try(tl<-tklistbox(ttGetMultipleContrasts,height=4,width=30,selectmode="multiple",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),background="white",font=.affylmGUIglobals$affylmGUIfont2)   )
  Try(lbl2<-tklabel(ttGetMultipleContrasts,text="Which contrast(s) is this for?",font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="       "),row=0,column=1,columnspan=1))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="       "),row=0,column=4,columnspan=1))
  Try(tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1))
  Try(tkgrid.configure(lbl2,sticky="w"))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="         "),row=2,column=1))
  Try(tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew"))
  Try(tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns"))
  Try(tkgrid(xscr,row=6,column=2,columnspan=2,sticky="wne"))

  Try(ContrastParameterizationList <- get("ContrastParameterizationList",envir=affylmGUIenvironment))

  Try(ContrastNamesVec  <- colnames(as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts)))

  Try(NumContrasts <- length(ContrastNamesVec))

  coefIndexList <- list()

  Try(if (NumContrasts>0)
    Try(for (i in (1:NumContrasts))
      Try(tkinsert(tl,"end",ContrastNamesVec[i]))))

  Try(tkselection.set(tl,0))

  Try(ReturnVal <- list(coefIndices=list())) # Other attributes can be added later if necessary.
  onOK <- function()
  {
			Try(ReturnVal <<- list(contrastIndices=as.list(as.numeric(strsplit(tclvalue(tkcurselection(tl))," ")[[1]])+1)))
      Try(tkgrab.release(ttGetMultipleContrasts));Try(tkdestroy(ttGetMultipleContrasts));Try(tkfocus(.affylmGUIglobals$ttMain))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetMultipleContrasts));Try(tkdestroy(ttGetMultipleContrasts));Try(tkfocus(.affylmGUIglobals$ttMain));Try(ReturnVal <<- list(contrastIndices=list()))}
  Try(OK.but <-tkbutton(ttGetMultipleContrasts,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetMultipleContrasts,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="    ")))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="    "),tklabel(ttGetMultipleContrasts,text="    "),OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(tl, "<Return>",onOK))
  Try(tkbind(Cancel.but, "<Return>",onCancel))
  Try(tkgrid(tklabel(ttGetMultipleContrasts,text="    ")))
  Try(tkfocus(ttGetMultipleContrasts))
  Try(tkbind(ttGetMultipleContrasts, "<Destroy>", function() {Try(tkgrab.release(ttGetMultipleContrasts));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetMultipleContrasts))

  return (ReturnVal)
}


GetSetNames <- function(numSets=2,set1="",set2="",set3="")
{
  Try(ttGetSetNames<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetSetNames))
  Try(tkgrab.set(ttGetSetNames))
  Try(tkfocus(ttGetSetNames)  )
  Try(tkwm.title(ttGetSetNames,"Set names for Venn diagram"))
  Try(tkgrid(tklabel(ttGetSetNames,text="    ")))
  Try(set1Tcl <- tclVar(init=set1))
  Try(entry.set1<-tkentry(ttGetSetNames,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=set1Tcl,bg="white"))
  Try(tkgrid(tklabel(ttGetSetNames,text="Set 1 : ",font=.affylmGUIglobals$affylmGUIfont2),entry.set1))
  Try(tkgrid(tklabel(ttGetSetNames,text="    ")))
  Try(tkgrid.configure(entry.set1,columnspan=2))
  Try(set2Tcl <- tclVar(init=set2))
  Try(if (numSets>1)
  {
    Try(entry.set2<-tkentry(ttGetSetNames,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=set2Tcl,bg="white"))
    Try(tkgrid(tklabel(ttGetSetNames,text="Set 2 : ",font=.affylmGUIglobals$affylmGUIfont2),entry.set2))
    Try(tkgrid(tklabel(ttGetSetNames,text="    ")))
    Try(tkgrid.configure(entry.set2,columnspan=2))
  })
  Try(set3Tcl <- tclVar(init=set3))
  Try(if (numSets>2)
  {
    Try(entry.set3<-tkentry(ttGetSetNames,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=set3Tcl,bg="white"))
    Try(tkgrid(tklabel(ttGetSetNames,text="Set 3 :   ",font=.affylmGUIglobals$affylmGUIfont2),entry.set3))
    Try(tkgrid(tklabel(ttGetSetNames,text="    ")))
    Try(tkgrid.configure(entry.set3,columnspan=2))
  })

  Try(ReturnVal <- list())
  Try(onOK <- function()
  {
      Try(set1 <- tclvalue(set1Tcl))
      Try(set2 <- "")
      Try(if (numSets>1)
        Try(set2 <- tclvalue(set2Tcl)))
      Try(set3 <- "")
      Try(if (numSets>2)
        Try(set3 <- tclvalue(set3Tcl)))
      Try(tkgrab.release(ttGetSetNames))
      Try(tkdestroy(ttGetSetNames))
      Try(tkfocus(.affylmGUIglobals$ttMain))
      Try(ReturnVal <<- list(set1=set1,set2=set2,set3=set3))
  })
  onCancel <- function() {Try(tkgrab.release(ttGetSetNames));Try(tkdestroy(ttGetSetNames));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetSetNames,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetSetNames,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  Try(tkgrid(tklabel(ttGetSetNames,text="    "),OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttGetSetNames,text="    ")))
  Try(tkbind(ttGetSetNames, "<Destroy>", function() {Try(tkgrab.release(ttGetSetNames));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkfocus(ttGetSetNames))
  Try(tkwait.window(ttGetSetNames))

  return (ReturnVal)
}

GetContrastNamesForHeatDiagram <- function(numContrasts=2,ContrastNames=c("Contrast 1","Contrast 2"))
{
  Try(ttGetContrastNamesForHeatDiagram<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetContrastNamesForHeatDiagram))
  Try(tkgrab.set(ttGetContrastNamesForHeatDiagram))
  Try(tkfocus(ttGetContrastNamesForHeatDiagram)  )
  Try(tkwm.title(ttGetContrastNamesForHeatDiagram,"Contrast names for heat diagram"))
  Try(tkgrid(tklabel(ttGetContrastNamesForHeatDiagram,text="    ")))

  Try(namesTcl <- list())
  Try(entryBoxes <- list())

  Try(for (i in (1:numContrasts))
  {
    Try(namesTcl[[i]] <- tclVar(init=ContrastNames[i]))
    Try(entryBoxes[[i]]<-tkentry(ttGetContrastNamesForHeatDiagram,width="40",font=.affylmGUIglobals$affylmGUIfont2,textvariable=namesTcl[[i]],bg="white"))
    Try(tkgrid(tklabel(ttGetContrastNamesForHeatDiagram,text=paste("Contrast",i),font=.affylmGUIglobals$affylmGUIfont2),entryBoxes[[i]]))
    Try(tkgrid(tklabel(ttGetContrastNamesForHeatDiagram,text="    ")))
    Try(tkgrid.configure(entryBoxes[[i]],columnspan=2))
  })

  Try(ReturnVal <- c())
  Try(onOK <- function()
  {
      Try(ReturnVal <<- c())
      Try(for (i in (1:numContrasts))
      {
        Try(ReturnVal[i] <<- tclvalue(namesTcl[[i]]))
        Try(names(ReturnVal)[i] <<- paste("Contrast",i,sep=""))
      })
      Try(tkgrab.release(ttGetContrastNamesForHeatDiagram))
      Try(tkdestroy(ttGetContrastNamesForHeatDiagram))
      Try(tkfocus(.affylmGUIglobals$ttMain))
  })
  onCancel <- function() {Try(tkgrab.release(ttGetContrastNamesForHeatDiagram));Try(tkdestroy(ttGetContrastNamesForHeatDiagram));Try(tkfocus(.affylmGUIglobals$ttMain));ReturnVal <<- c()}
  OK.but <-tkbutton(ttGetContrastNamesForHeatDiagram,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2)
  Cancel.but <-tkbutton(ttGetContrastNamesForHeatDiagram,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
  Try(tkgrid(tklabel(ttGetContrastNamesForHeatDiagram,text="    "),OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttGetContrastNamesForHeatDiagram,text="    ")))
  Try(tkbind(ttGetContrastNamesForHeatDiagram, "<Destroy>", function() {Try(tkgrab.release(ttGetContrastNamesForHeatDiagram));Try(tkfocus(.affylmGUIglobals$ttMain));}))
  Try(tkfocus(ttGetContrastNamesForHeatDiagram))
  Try(tkwait.window(ttGetContrastNamesForHeatDiagram))

  return (ReturnVal)
}

GetPValueCutoff <- function(p.value=0.01)
{
  Try(ttGetPValueCutoff<-tktoplevel(.affylmGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetPValueCutoff))
  Try(tkgrab.set(ttGetPValueCutoff))
  Try(tkfocus(ttGetPValueCutoff))
  Try(tkwm.title(ttGetPValueCutoff,"P-Value Cutoff"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="    ")))
  Try(PValueCutoffTcl <- tclVar(paste(p.value)))
  Try(entry.PValueCutoff <-tkentry(ttGetPValueCutoff,width="20",font=.affylmGUIglobals$affylmGUIfont2,textvariable=PValueCutoffTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="Enter a p-value cutoff",font=.affylmGUIglobals$affylmGUIfont2),columnspan=2))
  Try(tkgrid(entry.PValueCutoff,columnspan=2))
  Try(ReturnVal <- "ID_CancelFromGetPValueCutoff")
  onOK <- function()
  {
    Try(PValueCutoffTxt <- tclvalue(PValueCutoffTcl))
    Try(ReturnVal <<- PValueCutoffTxt)
    Try(tkgrab.release(ttGetPValueCutoff));Try(tkdestroy(ttGetPValueCutoff));Try(tkfocus(.affylmGUIglobals$ttMain))
  }
  onCancel <- function()
  {
    Try(ReturnVal <<- "ID_CancelFromGetPValueCutoff")
    Try(tkgrab.release(ttGetPValueCutoff));Try(tkdestroy(ttGetPValueCutoff));Try(tkfocus(.affylmGUIglobals$ttMain))
  }
  Try(OK.but <-tkbutton(ttGetPValueCutoff,text="   OK   ",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetPValueCutoff,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="       ")))
  Try(tkfocus(entry.PValueCutoff))
  Try(tkbind(entry.PValueCutoff, "<Return>",onOK))
  Try(tkbind(ttGetPValueCutoff, "<Destroy>", function(){Try(tkgrab.release(ttGetPValueCutoff));Try(tkfocus(.affylmGUIglobals$ttMain));return("ID_CancelFromGetPValueCutoff")}))
  Try(tkwait.window(ttGetPValueCutoff))
  Try(tkfocus(.affylmGUIglobals$ttMain))
  return (ReturnVal)
}

