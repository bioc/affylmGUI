GetNormalizationMethod <- function()
{
  Try(tkmessageBox(title="Normalization Method",message="Currently the only method available is rma."))
}

NormalizeNow <- function()
{
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment)) 
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Image Array Plot",message="Error: No arrays have been loaded.",
        icon="error",default="ok"))
    return()
  })
  Require("affy")
  Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
  
  Try(NormalizationMethod <- GetNormalizationMethod())
  Try(if (NormalizationMethod=="") return())
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))
  Try(if (NormalizationMethod=="RMA")
  {
    Try(NormalizedAffyData <- rma(RawAffyData))
    Try(assign("NormMethod","RMA",affylmGUIenvironment))
  }
  else
  {
    Require("affyPLM")
    Try(Pset <- fitPLM(RawAffyData))
    Try(NormalizedAffyData <- new("exprSet"))
    Try(NormalizedAffyData@exprs <- coefs(Pset))
    Try(NormalizedAffyData@se.exprs <- se(Pset))    
    Try(NormalizedAffyData@phenoData <- phenoData(Pset))
    Try(NormalizedAffyData@description <- description(Pset))
    Try(NormalizedAffyData@annotation <- annotation(Pset))
    Try(NormalizedAffyData@notes <- notes(Pset))
    Try(assign("NormMethod","PLM",affylmGUIenvironment))    
    Try(assign("weightsPLM",Pset@weights,affylmGUIenvironment))
  })
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(assign("NormalizedAffyData.Available",TRUE,affylmGUIenvironment))
  Try(assign("NormalizedAffyData",NormalizedAffyData,affylmGUIenvironment))
  Try(tkdelete(mainTree,"NormalizedAffyData.Status"))
  Try(if (NormalizationMethod=="RMA")
    Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Available (RMA)",font=affylmGUIfontTree))    
  else
    Try(tkinsert(mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Available (PLM)",font=affylmGUIfontTree)))  
}

AboutNormalization <- function()
{
  Try(tkmessageBox(title="About Normalization",message="Currently the only method available is rma."))
}

GetNormalizationMethod <- function()
{
	Try(ttGetNormalizationMethod <- tktoplevel(ttMain))
	Try(tkwm.deiconify(ttGetNormalizationMethod))
  Try(tkgrab.set(ttGetNormalizationMethod))
  Try(tkfocus(ttGetNormalizationMethod))
  Try(tkwm.title(ttGetNormalizationMethod,"Normalization Method"))
	
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    ")))
	Try(NormalizationMethodTcl <- tclVar("RMA"))
  Try(rb1 <- tkradiobutton(ttGetNormalizationMethod,text="RMA (Robust Multiarray Averaging)",variable=NormalizationMethodTcl,value="RMA",font=affylmGUIfont2))
	Try(rb2 <- tkradiobutton(ttGetNormalizationMethod,text="Robust Probe-level Linear Model",variable=NormalizationMethodTcl,value="RPLM",font=affylmGUIfont2))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),rb1))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),rb2))
	Try(tkgrid.configure(rb1,rb2,columnspan=2,sticky="w"))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),tklabel(ttGetNormalizationMethod,text="    ")))

	Try(ReturnVal <- "")
	Try(onCancel <- function() {Try(ReturnVal <<- "");Try(tkgrab.release(ttGetNormalizationMethod));Try(tkdestroy(ttGetNormalizationMethod));Try(tkfocus(ttMain))})
	Try(onOK <- function() {Try(ReturnVal <<- tclvalue(NormalizationMethodTcl));Try(tkgrab.release(ttGetNormalizationMethod));Try(tkdestroy(ttGetNormalizationMethod));Try(tkfocus(ttMain))})

	Try(OK.but     <- tkbutton(ttGetNormalizationMethod,text="OK",command=onOK,font=affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttGetNormalizationMethod,text="Cancel",command=onCancel,font=affylmGUIfont2))

	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),OK.but,Cancel.but,tklabel(ttGetNormalizationMethod,text="    ")))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    ")))

	Try(tkbind(ttGetNormalizationMethod,"<Destroy>",function() {ReturnVal <- "";Try(tkgrab.release(ttGetNormalizationMethod));Try(tkfocus(ttMain));}))
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(Cancel.but, "<Return>",onCancel))      

	Try(tkwait.window(ttGetNormalizationMethod))

	return (ReturnVal)
}

ExportNormalizedExpressionValues <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
  Try(ArraysLoaded <- get("ArraysLoaded",envir=affylmGUIenvironment))
  Try(if (ArraysLoaded==FALSE)
  {
    Try(tkmessageBox(title="Export Normalized Expression Values",message="Error: No arrays have been loaded.",
        icon="error",default="ok"))
    return()
  })
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
  Try(if (NormalizedAffyData.Available==FALSE)
    NormalizeNow())
  Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))    
  Try(if (NormalizedAffyData.Available==FALSE)
  {
    tkmessageBox(title="Export Normalized Expression Values",message="An error occured while trying to normalize the data.")
    return()
  
  })
  Try(NormalizedAffyData <- get("NormalizedAffyData",envir=affylmGUIenvironment))
	Try(FileName <- tclvalue(tkgetSaveFile(initialfile=paste(limmaDataSetNameText,"_exprs.txt",sep=""),filetypes="{{Tab-Delimited Text Files} {.txt}} {{All files} *}")))
	Try(if (!nchar(FileName)) return())
	Try(len <- nchar(FileName))
	if (len<=4)
		Try(FileName <- paste(FileName,".txt",sep=""))
	else if (substring(FileName,len-3,len)!=".txt")
				Try(FileName <- paste(FileName,".txt",sep=""))
  Try(write.table(NormalizedAffyData@exprs,file=FileName,sep="\t",quote=FALSE,col.names=NA))

}
