NormalizeNow <- function(){
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Normalization",message="Error: No arrays have been loaded.",icon="error",default="ok"))
		return()
		}
	)
	###Require("affy")
	Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
	#
	Try(NormalizationMethod <- GetNormalizationMethod())
	Try(if (NormalizationMethod=="") return())
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	Try(
		if (NormalizationMethod=="RMA"){
			Try(NormalizedAffyDataExpressionSet <- rma(RawAffyData))
			#rma returns an ExpressionSet as shown below:
			#  new("ExpressionSet",
			#      phenoData = phenoData(object),
			#      annotation = annotation(object),
			#      experimentData = experimentData(object),
			#      exprs = exprs)
			# NormalizedAffyData is an ExpressionSet as from R2.5.0
			#Just store the exprs values and set se.exprs values to  NULL
			Try(NormalizedAffyData.exprs    <- exprs(NormalizedAffyDataExpressionSet))
			Try(NormalizedAffyData.se.exprs <- NULL) #rma does not set se.exprs values
			Try(assign("NormalizedAffyData.exprs",NormalizedAffyData.exprs,affylmGUIenvironment))
			Try(assign("NormalizedAffyData.se.exprs",NormalizedAffyData.se.exprs,affylmGUIenvironment))
			Try(assign("NormMethod","RMA",affylmGUIenvironment))

		}else if (NormalizationMethod=="GCRMA"){
			Require("gcrma")
			Try(NormalizedAffyDataExpressionSet <- gcrma(RawAffyData))
			# gcrma returns an ExpressionSet, using the rma function as shown:
			# return(rma(object,subset=subset,background=FALSE,normalize=normalize,verbose=verbose))
			# NormalizedAffyData is an ExpressionSet as from R2.5.0
			#Just store the exprs values and set se.exprs values to  NULL
			Try(NormalizedAffyData.exprs    <- exprs(NormalizedAffyDataExpressionSet))
			Try(NormalizedAffyData.se.exprs <- NULL) #gcrma does not set se.exprs values
			Try(assign("NormalizedAffyData.exprs",NormalizedAffyData.exprs,affylmGUIenvironment))
			Try(assign("NormalizedAffyData.se.exprs",NormalizedAffyData.se.exprs,affylmGUIenvironment))
			Try(assign("NormMethod","GCRMA",affylmGUIenvironment))
		}else{
			Require("affyPLM")
			Try(NormalizedAffyDataPset <- fitPLM(RawAffyData))
			#affyPLM returns an object of class PLMset.
			Try(NormalizedAffyData.exprs    <- coefs(NormalizedAffyDataPset))
			Try(NormalizedAffyData.se.exprs <- se(NormalizedAffyDataPset)) #affyPLM does  set se.exprs values
			Try(assign("NormalizedAffyData.exprs",NormalizedAffyData.exprs,affylmGUIenvironment))
			Try(assign("NormalizedAffyData.se.exprs",NormalizedAffyData.se.exprs,affylmGUIenvironment))
			Try(assign("weightsPLM",weights(NormalizedAffyDataPset),affylmGUIenvironment))
			Try(assign("NormMethod","PLM",affylmGUIenvironment))
			###Try(NormalizedAffyData              <- new("ExpressionSet"))
			###Try(phenoData(NormalizedAffyData)   <- phenoData(Pset))
			###Try(description(NormalizedAffyData) <- description(Pset))
			###Try(annotation(NormalizedAffyData)  <- annotation(Pset))
			###Try(notes(NormalizedAffyData)       <- notes(Pset))
		}
	)
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="arrow"))
	Try(assign("NormalizedAffyData.Available",TRUE,affylmGUIenvironment))
	Try(assign("NormalizedAffyData.exprs",NormalizedAffyData.exprs,affylmGUIenvironment))
	Try(assign("NormalizedAffyData.se.exprs",NormalizedAffyData.se.exprs,affylmGUIenvironment))
	Try(tkdelete(.affylmGUIglobals$mainTree,"NormalizedAffyData.Status"))
	Try(
		if(NormalizationMethod=="RMA"){
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Available (RMA)",font=.affylmGUIglobals$affylmGUIfontTree))
		}else if(NormalizationMethod=="GCRMA"){
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Available (GCRMA)",font=.affylmGUIglobals$affylmGUIfontTree))
		}else{
			Try(tkinsert(.affylmGUIglobals$mainTree,"end","NormalizedAffyData","NormalizedAffyData.Status" ,text="Available (PLM)",font=.affylmGUIglobals$affylmGUIfontTree))
		}
	)
}#end of NormalizeNow <- function(){

GetNormalizationMethod <- function(){
	Try(ttGetNormalizationMethod <- tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttGetNormalizationMethod))
	Sys.sleep(0.1)
	Try(tkgrab.set(ttGetNormalizationMethod))
	Try(tkfocus(ttGetNormalizationMethod))
	Try(tkwm.title(ttGetNormalizationMethod,"Normalization Method"))
	#
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    ")))
	Try(NormalizationMethodTcl <- tclVar("RMA"))
	Try(rbRMA <- tkradiobutton(ttGetNormalizationMethod,text="RMA (Robust Multiarray Averaging)",variable=NormalizationMethodTcl,value="RMA",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rbGCRMA<-tkradiobutton(ttGetNormalizationMethod,text="GCRMA (Background Adjustment Using Sequence Information)",variable=NormalizationMethodTcl,value="GCRMA",font=.affylmGUIglobals$affylmGUIfont2))
	Try(rbPLM <- tkradiobutton(ttGetNormalizationMethod,text="Robust Probe-level Linear Model",variable=NormalizationMethodTcl,value="RPLM",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),rbRMA))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),rbGCRMA))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),rbPLM))
	Try(tkgrid.configure(rbRMA,rbGCRMA,rbPLM,columnspan=2,sticky="w"))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),tklabel(ttGetNormalizationMethod,text="    ")))
	#
	Try(ReturnVal <- "")
	Try(
		onCancel <- function() {
			Try(ReturnVal <<- "");
			Try(tkgrab.release(ttGetNormalizationMethod));
			Try(tkdestroy(ttGetNormalizationMethod));
			Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	Try(
		onOK <- function() {
			Try(ReturnVal <<- tclvalue(NormalizationMethodTcl));
			Try(tkgrab.release(ttGetNormalizationMethod));
			Try(tkdestroy(ttGetNormalizationMethod));
			Try(tkfocus(.affylmGUIglobals$ttMain))
		}
	)
	#
	Try(OK.but     <- tkbutton(ttGetNormalizationMethod,text="OK",command=onOK,font=.affylmGUIglobals$affylmGUIfont2))
	Try(Cancel.but <- tkbutton(ttGetNormalizationMethod,text="Cancel",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2))
	#
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    "),OK.but,Cancel.but,tklabel(ttGetNormalizationMethod,text="    ")))
	Try(tkgrid.configure(OK.but,sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttGetNormalizationMethod,text="    ")))
	#
	Try(tkbind(ttGetNormalizationMethod,"<Destroy>",function() {ReturnVal <- "";Try(tkgrab.release(ttGetNormalizationMethod));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkbind(OK.but, "<Return>",onOK))
	Try(tkbind(Cancel.but, "<Return>",onCancel))
	#
	Try(tkwait.window(ttGetNormalizationMethod))
	#
	return (ReturnVal)
}#end of GetNormalizationMethod <- function()

ExportNormalizedExpressionValues <- function(){
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=affylmGUIenvironment))
	Try(ArraysLoaded <- get("ArraysLoaded",envir=affylmGUIenvironment))
	Try(
		if (ArraysLoaded==FALSE){
			Try(tkmessageBox(title="Export Normalized Expression Values",message="Error: No arrays have been loaded.",icon="error",default="ok"))
			return()
		}
	)
	Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
	Try(
		if (NormalizedAffyData.Available==FALSE){
			NormalizeNow()
		}
	)
	Try(NormalizedAffyData.Available <- get("NormalizedAffyData.Available",envir=affylmGUIenvironment))
	Try(
		if (NormalizedAffyData.Available==FALSE){
			tkmessageBox(title="Export Normalized Expression Values",message="An error or cancellation occured while trying to normalize the data.")
			return()
		}
	)#end of Try
	Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
	Try(
		FileName <- tclvalue(
			tkgetSaveFile(
				initialfile=paste(
					limmaDataSetNameText,
					"_exprs.xls",
					sep=""),
				filetypes="{{Tab-Delimited Text Files} {.txt .xls}} {{All files} *}"
			)#end of tkgetSaveFile
		)#end of FileName <- tclvalue
	)#end of Try
	Try(if (!nchar(FileName)) return())
	Try(len <- nchar(FileName))
	if (len <= 4){
		Try(FileName <- paste(FileName,".xls",sep=""))
	}else if ((substring(FileName,len-3,len)!=".txt") &&(substring(FileName,len-3,len)!=".xls")){
		Try(FileName <- paste(FileName,".xls",sep=""))
	}
	Try(write.table(NormalizedAffyData.exprs,file=FileName,sep="\t",quote=FALSE,col.names=NA))
}#end of ExportNormalizedExpressionValues <- function()
