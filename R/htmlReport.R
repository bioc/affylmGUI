###########################################################################################################################
# R2HTML Plot Function (Modified to accept a plotFunction argument, rather than using the main R Graphics Device)
#
"HTMLplotUsingFunction" <- function (
	Caption = "",
	File = .HTML.file,
	GraphRelativeDirectory = ".",
	GraphAbsoluteDirectory = NULL,
	GraphFileName = "",
	GraphSaveAs = "png",
	GraphBorder = 1,
	Align = "center",
	plotFunction = NULL,
	Width=600,
	Height=600,
	PointSize=12,
	BG="white",
	res=72,...){
	#
	if(is.null(GraphAbsoluteDirectory)){
		GraphAbsoluteDirectory <- getwd()
		#For eg. "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files"
	}
	if(GraphFileName == ""){
		nowd <- date() #For eg. = "Thu Sep 20 11:27:14 2007"
		#                          000000000111111111122222
		#                          123456789012345678901234
		GraphFileName <- paste(
			"GRAPH_",
			substring(nowd,  5, 7),
			substring(nowd,  9, 10), "_",
			substring(nowd, 12, 13),
			substring(nowd, 15, 16),
			substring(nowd, 18, 19),
			sep = ""
		) #end of GraphFileName <- paste
		# Example name = GRAPH_Sep20_112714
	} #end of if(GraphFileName == "")
	GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")
	# for eg. = GRAPH_Sep20_112714.png
	#
	#AbsGraphFileName <- paste(GraphRelativeDirectory,.Platform$file.sep,GraphFileName,sep="")
	AbsGraphFileName <- file.path(GraphAbsoluteDirectory, GraphFileName)
	#For eg. = "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/GRAPH_Sep20_112714.png"
	if(GraphSaveAs=="png"){
		if(is.null(plotFunction)){
			dev.print(png, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
		}else{
			Try( #if it is linux like
				if(exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin"){
					Try(bitmap(file = AbsGraphFileName,bg=BG,res=res))
				}else{ #it is MS Windows or Mac
					Try(png(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG))
				} #end of else/if(exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
			)
			plotFunction()
			dev.off()
		} #end of else/if(is.null(plotFunction))
	}else if (GraphSaveAs=="jpg"){
		if (is.null(plotFunction)){
			dev.print(jpeg, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
		}else{
			Try( #if it is linux like
				if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin"){
					Try(bitmap(filename = AbsGraphFileName,bg=BG,res=res,type="jpeg"))
				}else{ #it is MS Windows or Mac
					Try(jpeg(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG))
				} #end of if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
			)
			plotFunction()
			dev.off()
		} #end of else/if (is.null(plotFunction))
	}else{
		stop("GraphSaveAs must be either jpg, or png")
	} #end of else/if (GraphSaveAs=="png")
	cat(
		paste(
			"<p align=",
			Align,
			"><img src='",
			paste(GraphRelativeDirectory,"/",GraphFileName,sep=""),
			"' border=",
			GraphBorder,
			">",
			sep = "",
			collapse = ""
		),
		file = File,
		append = TRUE,
		sep = ""
	) #end of cat
	if (Caption != "") {
		cat(paste("<br><i>", Caption, "</i>"), file = File, append = TRUE, sep = "")
	}
	cat("</P>", file = File, append = TRUE, sep = "\n")
	#
	if (exists("HTMLenv",where=".GlobalEnv",mode="environment")){
		try(assign(".HTML.graph", TRUE, env = get("HTMLenv", envir = .GlobalEnv)))
	}#end of if (exists("HTMLenv",where=".GlobalEnv",mode="environment"))
	#
	###try(assign(".HTML.graph", TRUE, env = get("HTMLenv", envir = .GlobalEnv)))
	invisible(return())
} #end of "HTMLplotUsingFunction" <- function

###########################################################################################################################

GetComponentsToExportInHTMLreport <- function(contrastParameterizationIndex=NULL){
	#get numberOfGenes,adjustMethod and sortBy from environment if they are available, or set them to default values
	Try(
		if(exists("numberOfGenes",envir=affylmGUIenvironment)){
			Try(numberOfGenes <- get("numberOfGenes",envir=affylmGUIenvironment))
			if(numberOfGenes > 100)numberOfGenes <- 100 #dontplot more than 100 here
		}else{
			Try(numberOfGenes <- 50)
			Try(assign("numberOfGenes",numberOfGenes,affylmGUIenvironment))
		} #end of else/if(exists("numberOfGenes",envir=affylmGUIenvironment))
	) #end of Try
	Try(
		if(exists("sortBy",envir=affylmGUIenvironment)){
			Try(sortBy <- get("sortBy",envir=affylmGUIenvironment))
		}else{
			Try(sortBy <- "B")
			Try(assign("sortBy",sortBy,affylmGUIenvironment))
		} #end of else/if(exists("sortBy",envir=affylmGUIenvironment))
	) #end of Try
	Try(
		if(exists("adjustMethod",envir=affylmGUIenvironment)){
			Try(adjustMethod <- get("adjustMethod",envir=affylmGUIenvironment))
		}else{
			Try(adjustMethod <- "BH")
			Try(assign("adjustMethod",adjustMethod,affylmGUIenvironment))
		} #end of else/if(exists("adjustMethod",envir=affylmGUIenvironment))
	) #end of Try
	#
	Try(NumContrastParameterizations <- get("NumContrastParameterizations",envir=affylmGUIenvironment))
	#
	Try(ttHTMLreportDialog<-tktoplevel(.affylmGUIglobals$ttMain))
	Try(tkwm.deiconify(ttHTMLreportDialog))
	Try(tkgrab.set(ttHTMLreportDialog))
	Try(tkfocus(ttHTMLreportDialog))
	Try(tkwm.title(ttHTMLreportDialog,"HTML Report"))
	Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    ")))
	#
	Try(TargetsTcl                            <- tclVar("1"))
	Try(NormalizationMethodTcl                <- tclVar("1"))
	Try(RawIntensityBoxPlotTcl                <- tclVar("1"))
	Try(NormalizedIntensityBoxPlotTcl         <- tclVar("1"))
	Try(DesignMatrixTcl                       <- tclVar("1"))
	#set ContrastMatrixTcl
	Try(
		if (NumContrastParameterizations>0)
			Try(ContrastMatrixTcl                 <- tclVar("1"))
		else
			Try(ContrastMatrixTcl                 <- tclVar("0"))
	)
	#set MAPlotsContrastsTcl
	Try(
		if (NumContrastParameterizations>0)
			Try(MAPlotsContrastsTcl               <- tclVar("1"))
		else
			Try(MAPlotsContrastsTcl               <- tclVar("0"))
	)
	#set TopNNToptablesTcl
	Try(
		if (NumContrastParameterizations>0)
			Try(TopNNToptablesTcl                 <- tclVar("1"))
		else
			Try(TopNNToptablesTcl                 <- tclVar("0"))
	)
	#set CompleteToptablesTcl
	Try(
		if (NumContrastParameterizations>0)
			Try(CompleteToptablesTcl              <- tclVar("1"))
		else
			Try(CompleteToptablesTcl              <- tclVar("0"))
	)
	#set TopNNToptablesTcl
	Try(
		if (NumContrastParameterizations>0)
			Try(TopNNToptablesTcl                 <- tclVar("1"))
		else
			Try(TopNNToptablesTcl                 <- tclVar("0"))
	)
	#set CompleteToptablesTcl
	Try(CompleteToptablesTcl                  <- tclVar("0"))
	#
	Try(TargetsCheckbox                       <- tkcheckbutton(ttHTMLreportDialog,variable=TargetsTcl))
	Try(NormalizationMethodCheckbox           <- tkcheckbutton(ttHTMLreportDialog,variable=NormalizationMethodTcl))
	Try(RawIntensityBoxPlotCheckbox           <- tkcheckbutton(ttHTMLreportDialog,variable=RawIntensityBoxPlotTcl))
	Try(NormalizedIntensityBoxPlotCheckbox    <- tkcheckbutton(ttHTMLreportDialog,variable=NormalizedIntensityBoxPlotTcl))
	Try(DesignMatrixCheckbox                  <- tkcheckbutton(ttHTMLreportDialog,variable=DesignMatrixTcl))
	Try(ContrastMatrixCheckbox                <- tkcheckbutton(ttHTMLreportDialog,variable=ContrastMatrixTcl))
	Try(MAPlotsContrastsCheckbox              <- tkcheckbutton(ttHTMLreportDialog,variable=MAPlotsContrastsTcl))
	Try(TopNNToptablesCheckbox                <- tkcheckbutton(ttHTMLreportDialog,variable=TopNNToptablesTcl))
	Try(CompleteToptablesCheckbox             <- tkcheckbutton(ttHTMLreportDialog,variable=CompleteToptablesTcl))
	#
	Try(lbl2 <- tklabel(ttHTMLreportDialog,text="Components to be Included in the HTML Report",font=.affylmGUIglobals$affylmGUIfont2))
	tkgrid(tklabel(ttHTMLreportDialog,text="    "),lbl2)
	Try(tkgrid.configure(lbl2,columnspan=3,sticky="w"))
	tkgrid(tklabel(ttHTMLreportDialog,text="    "))
	#
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
	#
	Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Contrasts Matrix (Parameterization)",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),ContrastMatrixCheckbox,currentLabel))
	Try(tkgrid.configure(ContrastMatrixCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
	Try(currentLabel <- tklabel(ttHTMLreportDialog,text="M A Plots for Contrasts",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),MAPlotsContrastsCheckbox,currentLabel))
	Try(tkgrid.configure(MAPlotsContrastsCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
	Try(currentLabel <- tklabel(ttHTMLreportDialog,text=paste("Top ",numberOfGenes," DE Genes. TopTable sets Adjust Method = ",adjustMethod," sort by = ",sortBy,".",sep=""),font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),TopNNToptablesCheckbox,currentLabel))
	Try(tkgrid.configure(TopNNToptablesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
	Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Complete Lists of DE-Ranked Genes",font=.affylmGUIglobals$affylmGUIfont2))
	Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),CompleteToptablesCheckbox,currentLabel))
	Try(tkgrid.configure(CompleteToptablesCheckbox,sticky="e"));
	Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
	#
	if (NumContrastParameterizations==0){
		Try(tkconfigure(ContrastMatrixCheckbox,   state="disabled"))
		Try(tkconfigure(MAPlotsContrastsCheckbox, state="disabled"))
		Try(tkconfigure(TopNNToptablesCheckbox,   state="disabled"))
		Try(tkconfigure(CompleteToptablesCheckbox,state="disabled"))
	} #end of if (NumContrastParameterizations==0)
	#
	tkgrid(tklabel(ttHTMLreportDialog,text="    "))
	tkgrid(tklabel(ttHTMLreportDialog,text="    "))
	ReturnVal <- list()
	onOK <- function(){
		if (tclvalue(TargetsTcl)                   =="1") ReturnVal[["Targets"]]                    <- TRUE else ReturnVal[["Targets"]]                    <- FALSE
		if (tclvalue(NormalizationMethodTcl)       =="1") ReturnVal[["NormalizationMethod"]]        <- TRUE else ReturnVal[["NormalizationMethod"]]        <- FALSE
		if (tclvalue(RawIntensityBoxPlotTcl)       =="1") ReturnVal[["RawIntensityBoxPlot"]]        <- TRUE else ReturnVal[["RawIntensityBoxPlot"]]        <- FALSE
		if (tclvalue(NormalizedIntensityBoxPlotTcl)=="1") ReturnVal[["NormalizedIntensityBoxPlot"]] <- TRUE else ReturnVal[["NormalizedIntensityBoxPlot"]] <- FALSE
		if (tclvalue(DesignMatrixTcl)              =="1") ReturnVal[["DesignMatrix"]]               <- TRUE else ReturnVal[["DesignMatrix"]]               <- FALSE
		if (tclvalue(ContrastMatrixTcl)            =="1") ReturnVal[["ContrastsMatrix"]]            <- TRUE else ReturnVal[["ContrastsMatrix"]]            <- FALSE
		if (tclvalue(MAPlotsContrastsTcl)          =="1") ReturnVal[["MAPlotsContrasts"]]           <- TRUE else ReturnVal[["MAPlotsContrasts"]]           <- FALSE
		if (tclvalue(TopNNToptablesTcl)            =="1") ReturnVal[["TopNNToptablesTcl"]]          <- TRUE else ReturnVal[["TopNNToptables"]]             <- FALSE
		if (tclvalue(CompleteToptablesTcl)         =="1") ReturnVal[["CompleteToptablesTcl"]]       <- TRUE else ReturnVal[["CompleteToptablesTcl"]]       <- FALSE
		Try(tkgrab.release(ttHTMLreportDialog));
		Try(tkdestroy(ttHTMLreportDialog));
		Try(tkfocus(.affylmGUIglobals$ttMain))
		ReturnVal  <<-  ReturnVal
	} #end of onOK <- function()
	#
	onCancel <- function(){
		Try(tkgrab.release (ttHTMLreportDialog));
		Try(tkdestroy      (ttHTMLreportDialog));
		Try(tkfocus        (.affylmGUIglobals$ttMain));
		ReturnVal <<- list()
	} #end of onCancel <- function()
	#
	OK.but     <-tkbutton(ttHTMLreportDialog,text="   OK   ",command=onOK,    font=.affylmGUIglobals$affylmGUIfont2)
	Cancel.but <-tkbutton(ttHTMLreportDialog,text=" Cancel ",command=onCancel,font=.affylmGUIglobals$affylmGUIfont2)
	#
	tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),OK.but,Cancel.but,tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "))
	tkgrid.configure(OK.but,    sticky="e")
	tkgrid.configure(Cancel.but,sticky="w")
	tkgrid(
		tklabel(ttHTMLreportDialog,text="    "),
		tklabel(ttHTMLreportDialog,text="    "),
		tklabel(ttHTMLreportDialog,text="    "),
		tklabel(ttHTMLreportDialog,text="    "),
		tklabel(ttHTMLreportDialog,text="    ")
	)
	Try(tkfocus(ttHTMLreportDialog))
	Try(tkbind(ttHTMLreportDialog, "<Destroy>", function() {Try(tkgrab.release(ttHTMLreportDialog));Try(tkfocus(.affylmGUIglobals$ttMain));}))
	Try(tkwait.window(ttHTMLreportDialog))
	#
	return (ReturnVal)
}#end of GetComponentsToExportInHTMLreport <- function(contrastParameterizationIndex=NULL)
#
###########################################################################################################################
#
ExportHTMLreport <- function(){
	# We will use the R2HTML package, but with my own HTMLplot function.
	# Will we need xtable or does R2HTML have its own HTMLtable function?
	Require("xtable")
	Require("R2HTML")
	#
	#get numberOfGenes,adjustMethod and sortBy from environment if they are available, if not,  set them to default values
	Try(
		if(exists("numberOfGenes",envir=affylmGUIenvironment)){
			Try(numberOfGenes <- get("numberOfGenes",envir=affylmGUIenvironment))
			if(numberOfGenes > 100)numberOfGenes <- 100 #dontplot more than 100 here
		}else{
			Try(numberOfGenes <- 50)
			Try(assign("numberOfGenes",numberOfGenes,affylmGUIenvironment))
		} #end of else/if(exists("numberOfGenes",envir=affylmGUIenvironment))
	) #end of Try
	Try(
		if(exists("sortBy",envir=affylmGUIenvironment)){
			Try(sortBy <- get("sortBy",envir=affylmGUIenvironment))
		}else{
			Try(sortBy <- "B")
			Try(assign("sortBy",sortBy,affylmGUIenvironment))
		} #end of else/if(exists("sortBy",envir=affylmGUIenvironment))
	) #end of Try
	Try(
		if(exists("adjustMethod",envir=affylmGUIenvironment)){
			Try(adjustMethod <- get("adjustMethod",envir=affylmGUIenvironment))
		}else{
			Try(adjustMethod <- "BH")
			Try(assign("adjustMethod",adjustMethod,affylmGUIenvironment))
		} #end of else/if(exists("adjustMethod",envir=affylmGUIenvironment))
	) #end of Try
	#
	Try(limmaDataSetNameText                 <- get("limmaDataSetNameText"         ,envir=affylmGUIenvironment)) #for eg: [1] "alg_Est_gcrma_3C"
	Try(ArraysLoaded                         <- get("ArraysLoaded"                 , envir=affylmGUIenvironment))
	Try(NumContrastParameterizations         <- get("NumContrastParameterizations" ,envir=affylmGUIenvironment)) #for eg: [1] 1
	Try(ContrastParameterizationList         <- get("ContrastParameterizationList" ,envir=affylmGUIenvironment))
	# for eg:
	#> summary(ContrastParameterizationList)
	#                               Length Class  Mode
	#ContrastParameterizationName.1 5      -none- list
	#
	#ContrastParameterizationList is a list with entrys for each gene(?) for:
	#some values for each gene off the top of the screen, followed by all of this data:
	#
	#$ContrastParameterizationName.1$eb$rank
	#[1] 4
	#
	#$ContrastParameterizationName.1$eb$assign
	#NULL
	#
	#$ContrastParameterizationName.1$eb$qr
	#$qr
	#             EstAbsent10 EstAbsent48 EstPresent10 EstPresent48
	#low10-1.cel   -1.4142136   0.0000000    0.0000000    0.0000000
	#low10-2.cel    0.7071068  -1.4142136    0.0000000    0.0000000
	#high10-1.cel   0.0000000   0.0000000   -1.4142136    0.0000000
	#high10-2.cel   0.0000000   0.0000000    0.7071068   -1.4142136
	#low48-1.cel    0.0000000   0.7071068    0.0000000    0.0000000
	#low48-2.cel    0.0000000   0.7071068    0.0000000    0.0000000
	#high48-1.cel   0.0000000   0.0000000    0.0000000    0.7071068
	#high48-2.cel   0.0000000   0.0000000    0.0000000    0.7071068
	#
	#$qraux
	#[1] 1.707107 1.000000 1.707107 1.000000
	#
	#$pivot
	#[1] 1 2 3 4
	#
	#$tol
	#[1] 1e-07
	#
	#$rank
	#[1] 4
	#
	#attr(,"class")
	#[1] "qr"
	#
	#$ContrastParameterizationName.1$eb$df.residual
	#    [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
	#   [85] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
	#   ...  ...................................
	#[12601] 4 4 4 ...
	#
	#$ContrastParameterizationName.1$eb$sigma
	#                   100_g_at                     1000_at                     1001_at                   1002_f_at                   1003_s_at                     1004_at
	#                0.210371658                 0.229581752                 0.056224969                 0.079154826                 0.035168585                 0.062620730
	#                    1005_at                     1006_at                   1007_s_at                   1008_f_at                     1009_at                      101_at
	#                        ...                         ...                         ...                         ...                         ...                         ...
	#$ContrastParameterizationName.1$eb$cov.coefficients
	#                              (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#(EstAbsent10)-(EstAbsent48)                           1.0                           0.0                          0.5
	#(EstPresent10)-(EstPresent48)                         0.0                           1.0                          0.5
	#(EstAbsent10)-(EstPresent48)                          0.5                           0.5                          1.0
	#
	#$ContrastParameterizationName.1$eb$stdev.unscaled
	#                            (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#100_g_at                                              1                             1                            1
	#1000_at                                               1                             1                            1
	#   ...                                              ...                           ...                          ...
	#
	#$ContrastParameterizationName.1$eb$contrasts
	#             (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#EstAbsent10                            1                             0                            1
	#EstAbsent48                           -1                             0                            0
	#EstPresent10                           0                             1                            0
	#EstPresent48                           0                            -1                           -1
	#
	#$ContrastParameterizationName.1$eb$df.prior
	#[1] 1.916164
	#
	#$ContrastParameterizationName.1$eb$s2.prior
	#[1] 0.01619520
	#
	#$ContrastParameterizationName.1$eb$var.prior
	#[1] 156.9730 123.4235 256.8846
	#
	#$ContrastParameterizationName.1$eb$proportion
	#[1] 0.01
	#
	#$ContrastParameterizationName.1$eb$s2.post
	#                   100_g_at                     1000_at                     1001_at                   1002_f_at                   1003_s_at                     1004_at
	#                0.035167654                 0.040881861                 0.007382765                 0.009481584                 0.006081640                 0.007896685
	#                    1005_at                     1006_at                   1007_s_at                   1008_f_at                     1009_at                      101_at
	#                0.031434553                 0.009843242                 0.042246541                 0.501898805                 0.061243025                 0.008799333
	#                        ...                         ...                         ...                         ...                         ...                         ...
	#AND
	#$ContrastParameterizationName.1$eb$t
	#                            (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#100_g_at                                   1.030421e+00                 -8.576221e-01                 1.325653e+00
	#1000_at                                   -7.997174e-01                 -2.053407e+00                 6.645345e-02
	#                                                    ...                           ...                          ...
	#	AND
	#$ContrastParameterizationName.1$eb$p.value
	#                            (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#100_g_at                                   3.430840e-01                  4.244753e-01                 2.338416e-01
	#1000_at                                    4.547765e-01                  8.650180e-02                 9.492055e-01
	#                                                    ...                           ...                          ...
	#AND
	#$ContrastParameterizationName.1$eb$lods
	#                            (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#                                                    ...                           ...                          ...
	#AND
	#$ContrastParameterizationName.1$contrastsMatrixInList
	#$ContrastParameterizationName.1$contrastsMatrixInList$contrasts
	#             (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
	#EstAbsent10                            1                             0                            1
	#EstAbsent48                           -1                             0                            0
	#EstPresent10                           0                             1                            0
	#EstPresent48                           0                            -1                           -1
	#
	#$ContrastParameterizationName.1$contrastsMatrixInList$contrastsCreatedFromDropDowns
	#[1] TRUE
	#
	#$ContrastParameterizationName.1$contrastsMatrixInList$Param1
	#[1] 1 3 1
	#
	#$ContrastParameterizationName.1$contrastsMatrixInList$Param2
	#[1] 2 4 4
	#
	#
	#$ContrastParameterizationName.1$ContrastParameterizationNameText
	#[1] "ContSet1"
	#
	#################3
	Try(ContrastParameterizationTREEIndexVec <- get("ContrastParameterizationTREEIndexVec",envir=affylmGUIenvironment)) #for eg: [1] 1
	#
	if(ArraysLoaded==FALSE){
		Try(tkmessageBox(title="Export HTML Report",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.affylmGUIglobals$ttMain))
		return()
	} #end of if(ArraysLoaded==FALSE)
	#
	#
	#
	if(NumContrastParameterizations>0){
		#Select which parameterization
		Try(contrastParameterizationIndex <- ChooseContrastParameterization()) #for eg: [1] 1
		#If click cancel button, then exit this routine
		Try(if (contrastParameterizationIndex==0) return()) # Cancel
		#
		Try(ContrastParameterizationNamesVec <- get("ContrastParameterizationNamesVec",envir=affylmGUIenvironment)) #for eg: [1] "ContSet1"
		#store locally TREEindex for chosen parameterization
		Try(.affylmGUIglobals$ContrastParameterizationTREEIndex <- ContrastParameterizationTREEIndexVec[contrastParameterizationIndex]) #for eg: [1] 1
		#setup contrast matrix
		Try(contrastsMatrix <- as.matrix(ContrastParameterizationList[[contrastParameterizationIndex]]$contrastsMatrixInList$contrasts))
		#             (EstAbsent10)-(EstAbsent48) (EstPresent10)-(EstPresent48) (EstAbsent10)-(EstPresent48)
		#EstAbsent10                            1                             0                            1
		#EstAbsent48                           -1                             0                            0
		#EstPresent10                           0                             1                            0
		#EstPresent48                           0                            -1                           -1
		#>
		#get vector of names for contrasts
		Try(ContrastNamesVec  <- colnames(contrastsMatrix))
		#for eg: [1] "(EstAbsent10)-(EstAbsent48)"   "(EstPresent10)-(EstPresent48)" "(EstAbsent10)-(EstPresent48)"
		Try(NumContrasts <- length(ContrastNamesVec)) #for eg: 3
		Try(ContrastParameterizationNameNode <- paste("ContrastParameterizationName.",.affylmGUIglobals$ContrastParameterizationTREEIndex,sep=""))
		#for eg: "ContrastParameterizationName.1"
		Try(fit <- (ContrastParameterizationList[[ContrastParameterizationNameNode]])$fit)
		#> summary(fit)
		#                 Length Class  Mode
		#coefficients     37875  -none- numeric
		#rank                 1  -none- numeric
		#assign               0  -none- NULL
		#qr                   5  qr     list
		#df.residual      12625  -none- numeric
		#sigma            12625  -none- numeric
		#cov.coefficients     9  -none- numeric
		#stdev.unscaled   37875  -none- numeric
		#contrasts           12  -none- numeric
		#Amean            12625  -none- numeric
		#
		Try(fit <- eBayes(fit))
		#> summary(fit)
		#                 Length Class  Mode
		#coefficients     37875  -none- numeric
		#rank                 1  -none- numeric
		#assign               0  -none- NULL
		#qr                   5  qr     list
		#df.residual      12625  -none- numeric
		#sigma            12625  -none- numeric
		#cov.coefficients     9  -none- numeric
		#stdev.unscaled   37875  -none- numeric
		#contrasts           12  -none- numeric
		#Amean            12625  -none- numeric
		#df.prior             1  -none- numeric
		#s2.prior             1  -none- numeric
		#var.prior            3  -none- numeric
		#proportion           1  -none- numeric
		#s2.post          12625  -none- numeric
		#t                37875  -none- numeric
		#p.value          37875  -none- numeric
		#lods             37875  -none- numeric
		#
		Try(ComponentsToExport <- GetComponentsToExportInHTMLreport(contrastParameterizationIndex))
	}else{
		Try(ComponentsToExport <- GetComponentsToExportInHTMLreport())
	} #end of else/if(NumContrastParameterizations>0)
	#if nothing selected, exit this routine
	Try(if (length(ComponentsToExport)==0) return())
	#
	#
	#Setup files to use for HTML report
	Try(fileNameWithPath<- tkgetSaveFile(initialfile=limmaDataSetNameText,filetypes="{{HTML Files} {.html .htm}} {{All files} *}"))
	#for eg: W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/alg_Est_gcrma_3C
	#Try(tkmessageBox(title="229:affylmGUI-htmlreport",message=paste("fileNameWithPath =",fileNameWithPath),icon="info",default="ok"))
	#if null filename selected, exit this routine
	Try(if (nchar(tclvalue(fileNameWithPath))==0)return())
	#
	#
	#
	Try(path <- tclvalue(tclfile.dir (tclvalue(fileNameWithPath)))) #for eg: "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files"
	#Try(tkmessageBox(title="235:affylmGUI-htmlreport",message=paste("path =",path),icon="info",default="ok"))
	#
	Try(fileName <- tclvalue(tclfile.tail(tclvalue(fileNameWithPath)))) #for eg: "alg_Est_gcrma_3C"
	#Try(tkmessageBox(title="242:affylmGUI-htmlreport",message=paste("fileName =",fileName),icon="info",default="ok"))
	#
	Try(len <- nchar(fileName))
	if (len<4){
		Try(fileName <- paste(fileName,".html",sep=""))
	}else if((tolower(substring(fileName,len-4,len))!=".html") &&(len<5 || (tolower(substring(fileName,len-4,len))!=".html"))){
		Try(fileName <- paste(fileName,".html",sep="")) #for eg: "alg_Est_gcrma_3C.html"
	} #end of else/if (len<4)
	#
	Try(fileNameWithoutExtension <- substring(fileName,1,nchar(fileName)-5)) #for eg: "alg_Est_gcrma_3C"
	#
	Try(HTMLfilePath <- paste(path,.Platform$file.sep,fileNameWithoutExtension,"_files",sep="")) #for eg:  "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/alg_Est_gcrma_3C_files"
	Try(HTMLfileRelativePath <- paste(fileNameWithoutExtension,"_files",sep="")) #for eg: "alg_Est_gcrma_3C_files"
	Try(dir.create(HTMLfilePath))
	#
	Try(fileNameWithPath <- paste(path,"/",fileName,sep="")) #for eg: "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/alg_Est_gcrma_3C.html"
	#
	Try(R2HTMLpath <- system.file(package="R2HTML","output")) #for eg: [1] "C:/R/R-2.6.0alpha/library/R2HTML/output"
	Try(cssFileSource <- paste(R2HTMLpath,"/","R2HTML.css",sep="")) #for eg: [1] "C:/R/R-2.6.0alpha/library/R2HTML/output/R2HTML.css"
	Try(cssFileDestination <- paste(path,"/","R2HTML.css",sep="")) #for eg: "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/R2HTML.css"
	Try(R2HTMLlogoSource <- paste(R2HTMLpath,"/","R2HTMLlogo.png",sep="")) #for eg: [1] "C:/R/R-2.6.0alpha/library/R2HTML/output/R2HTMLlogo.png"
	Try(R2HTMLlogoDestination <- paste(path,"/","R2HTMLlogo.png",sep="")) #for eg: [1] "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/R2HTMLlogo.png"
	Try(file.copy(cssFileSource,cssFileDestination,overwrite=TRUE)) #for eg: TRUE
	Try(file.copy(R2HTMLlogoSource,R2HTMLlogoDestination,overwrite=TRUE)) #for eg: TRUE
	#
	Try(HTMLtarget <- HTMLInitFile(path,filename=fileNameWithoutExtension,Title=paste(limmaDataSetNameText,"- Statistical Microarray Analysis using affylmGUI"), HTMLframe=FALSE,BackGroundColor="#FFFFFF"))
	# for eg: "W:/aaa-R/eg_datasets/Affy-Estrogen_Data/estrogen_CEL_files/alg_Est_gcrma_3C.html"
	#
	# The following is written to this file:
	# <html xmlns:mml="http://www.w3.org/1998/Math/MathML">
	# <head>
	# <title> alg_Est_gcrma_3C - Statistical Microarray Analysis using affylmGUI </title>
	# <link rel=stylesheet href="R2HTML.css" type=text/css>
	# <object id="mathplayer" classid="clsid:32F66A20-7614-11D4-BD11-00104BD3F987"></object>
	# <?import namespace="mml" implementation="#mathplayer"?>
	# <script type="text/javascript" src="ASCIIMathML.js"></script>
	# <link href="./runtime/styles/xp/grid.css" rel="stylesheet" type="text/css" ></link>
	# <link href="gridR2HTML.css" rel="stylesheet" type="text/css" ></link>
	#
	# <script src="./runtime/lib/grid.js"></script>
	#
	# <script src="./gridR2HTML.js"></script>
	# <script>
	#    nequations=0;
	# </script>
	# </head>
	# <body onload="translate()" bgcolor= #FFFFFF background="" >
	###########################
	#
	Try(HTML.title(paste(limmaDataSetNameText,"- Statistical Microarray Analysis using affylmGUI"),HR=1)) #for eg: NULL
	# for eg: written to html file is:
	# <h1 > alg_Est_gcrma_3C - Statistical Microarray Analysis using affylmGUI</h1>
	###########################
	Try(ExportTargets                    <- ComponentsToExport$Targets)
	Try(ExportNormalizationMethod        <- ComponentsToExport$NormalizationMethod)
	Try(ExportRawIntensityBoxPlot        <- ComponentsToExport$RawIntensityBoxPlot)
	Try(ExportNormalizedIntensityBoxPlot <- ComponentsToExport$NormalizedIntensityBoxPlot)
	Try(ExportDesignMatrix               <- ComponentsToExport$DesignMatrix)
	Try(ExportContrastsMatrix            <- ComponentsToExport$ContrastsMatrix)
	Try(ExportMAPlotsContrasts           <- ComponentsToExport$MAPlotsContrasts)
	Try(ExportTopNNToptables             <- ComponentsToExport$TopNNToptables)
	Try(ExportCompleteToptables          <- ComponentsToExport$CompleteToptables)
	#All the above are set to TRUE or FALSE, depending on what was chosen in the dialogue box
	#
	#This sets up parameters for the plots
	if(ExportRawIntensityBoxPlot || ExportNormalizedIntensityBoxPlot || ExportMAPlotsContrasts){
		Try(
			#if linux like
			if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin"){
				Try(pngParams     <- GetJpegOrPngX11Params(graphFileType="PNG")) #sets background colour(bg) and resolution(res)
				Try(if (length(pngParams)==0) return())
				Try(plotBG        <- pngParams$bg)
				Try(plotRes       <- pngParams$res)
			}else{
				#if Windows or Mac
				Try(pngParams     <- GetJpegOrPngParams(graphFileType="PNG")) #sets width, height, background colour(bg) and Font Size(pointsize)
				Try(if (length(pngParams)==0) return())
				Try(plotWidth     <- pngParams$width)
				Try(plotHeight    <- pngParams$height)
				Try(plotPointSize <- pngParams$pointsize)
				Try(plotBG        <- pngParams$bg)
			} #end of else/if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
		) #end of Try
	} #end of if (ExportRawIntensityBoxPlot || ExportNormalizedIntensityBoxPlot || ExportMAPlotsContrasts)
	#
	Try(HTML.title("Contents",HR=2)) # this writes " <h2 > Contents</h2>" to html file
	#This writes links to html file for each feature selected
	Try(if (ExportTargets) Try(HTMLli(txt="<a href=\"#Targets\"><b>Targets</b></a>")))
	Try(if (ExportNormalizationMethod) Try(HTMLli(txt="<a href=\"#NormalizationMethod\"><b>Normalization Method</b></a>")))
	Try(if (ExportRawIntensityBoxPlot) Try(HTMLli(txt="<a href=\"#RawIntensityBoxPlot\"><b>Raw Intensity Box Plot</b></a>")))
	Try(if (ExportNormalizedIntensityBoxPlot) Try(HTMLli(txt="<a href=\"#NormalizedIntensityBoxPlot\"><b>Normalized Intensity Box Plot</b></a>")))
	Try(if (ExportDesignMatrix) Try(HTMLli(txt="<a href=\"#DesignMatrix\"><b>Design Matrix</b></a>")))
	Try(if (ExportContrastsMatrix) Try(HTMLli(txt="<a href=\"#ContrastsMatrix\"><b>Contrasts Matrix</b></a>")))
	Try(if (ExportMAPlotsContrasts) Try(HTMLli(txt="<a href=\"#MAPlotsContrasts\"><b>M A Plots for Contrasts</b></a>")))
	Try(if (ExportTopNNToptables) Try(HTMLli(txt=paste("<a href=\"#TopNNToptables\"><b>Tables of Top ",numberOfGenes," Differentially Expressed Genes</b></a>",sep=""))))
	Try(if (ExportCompleteToptables) Try(HTMLli(txt="<a href=\"#CompleteToptables\"><b>Complete Tables of Genes Ranked in order of Evidence for Differential Expression</b></a>")))
	#
	Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.affylmGUIglobals$ttMain))
	#
	#
	#
	if (ExportTargets){
		Try(Targets <- get("Targets",envir=affylmGUIenvironment))
		#for eg: Targets is:
		#      Name     FileName       Target
		#1  Abs10.1  low10-1.cel  EstAbsent10
		#2  Abs10.2  low10-2.cel  EstAbsent10
		#3 Pres10.1 high10-1.cel EstPresent10
		#4 Pres10.2 high10-2.cel EstPresent10
		#5  Abs48.1  low48-1.cel  EstAbsent48
		#6  Abs48.2  low48-2.cel  EstAbsent48
		#7 Pres48.1 high48-1.cel EstPresent48
		#8 Pres48.2 high48-2.cel EstPresent48
		#######################
		Try(displayVector <- rep("s",ncol(Targets)+1)) #for eg: [1] "s" "s" "s" "s"
		Try(
			for (i in (0:ncol(Targets))){
				if (i==0 || colnames(Targets)[i]=="SlideNumber"){
					displayVector[i] <- "d"
				} #end of if (i==0 || colnames(Targets)[i]=="SlideNumber")
			} #end of for (i in (0:ncol(Targets)))
		) #end of Try
		#for eg: This has set the displayVector to [1] "s" "s" "s" "s"
		Try(TargetsXtable <- xtable(Targets,display=displayVector))
		# for eg: This outputs:
		# % latex table generated in R 2.6.0 by xtable 1.5-1 package
		# % Thu Sep 20 16:44:04 2007
		# \begin{table}[ht]
		# \begin{center}
		# \begin{tabular}{rlll}
		#   \hline
		#  & Name & FileName & Target \\
		#   \hline
		#  1 & Abs10.1 & low10-1.cel & EstAbsent10 \\
		#   2 & Abs10.2 & low10-2.cel & EstAbsent10 \\
		#   3 & Pres10.1 & high10-1.cel & EstPresent10 \\
		#   4 & Pres10.2 & high10-2.cel & EstPresent10 \\
		#   5 & Abs48.1 & low48-1.cel & EstAbsent48 \\
		#   6 & Abs48.2 & low48-2.cel & EstAbsent48 \\
		#   7 & Pres48.1 & high48-1.cel & EstPresent48 \\
		#   8 & Pres48.2 & high48-2.cel & EstPresent48 \\
		#    \hline
		# \end{tabular}
		# \end{center}
		# \end{table}
		#########################
		Try(HTML.title("<a name=\"Targets\">RNA Targets</a>",HR=2)) #for eg: this writes " <h2 > <a name="Targets">RNA Targets</a></h2>" to html file
		Try(print(TargetsXtable,type="html",file=fileNameWithPath,append=TRUE))
		#For eg: This prints the following to the html file:
		# <!-- Thu Sep 20 16:46:24 2007 -->
		# <TABLE border=1>
		# <TR> <TH>  </TH> <TH> Name </TH> <TH> FileName </TH> <TH> Target </TH>  </TR>
		#   <TR> <TD align="right"> 1 </TD> <TD> Abs10.1 </TD> <TD> low10-1.cel </TD> <TD> EstAbsent10 </TD> </TR>
		#   <TR> <TD align="right"> 2 </TD> <TD> Abs10.2 </TD> <TD> low10-2.cel </TD> <TD> EstAbsent10 </TD> </TR>
		#   <TR> <TD align="right"> 3 </TD> <TD> Pres10.1 </TD> <TD> high10-1.cel </TD> <TD> EstPresent10 </TD> </TR>
		#   <TR> <TD align="right"> 4 </TD> <TD> Pres10.2 </TD> <TD> high10-2.cel </TD> <TD> EstPresent10 </TD> </TR>
		#   <TR> <TD align="right"> 5 </TD> <TD> Abs48.1 </TD> <TD> low48-1.cel </TD> <TD> EstAbsent48 </TD> </TR>
		#   <TR> <TD align="right"> 6 </TD> <TD> Abs48.2 </TD> <TD> low48-2.cel </TD> <TD> EstAbsent48 </TD> </TR>
		#   <TR> <TD align="right"> 7 </TD> <TD> Pres48.1 </TD> <TD> high48-1.cel </TD> <TD> EstPresent48 </TD> </TR>
		#   <TR> <TD align="right"> 8 </TD> <TD> Pres48.2 </TD> <TD> high48-2.cel </TD> <TD> EstPresent48 </TD> </TR>
		#    </TABLE>
		# <!-- html table generated in R 2.6.0 by xtable 1.5-1 package -->
		#########################
	} #end of if (ExportTargets)
	#
	#
	#
	if (ExportNormalizationMethod){
		Try(NormMethod <- get("NormMethod", envir=affylmGUIenvironment)) #for eg: GCRMA
		Try(HTML.title("<a name=\"NormalizationMethod\">Normalization method</a>",HR=2)) #writes to html: " <h2 > <a name="NormalizationMethod">Normalization method</a></h2>"
		Try(
			if(NormMethod=="RMA"){
				Try(HTMLli(txt="<b>RMA (Robust Multiarray Averaging)</b>"))
			}else if(NormMethod=="PLM"){
				Try(HTMLli(txt="<b>PLM (Robust Probe-level Linear Model)</b>"))
			}else if(NormMethod=="GCRMA"){
				Try(HTMLli(txt="<b>GCRMA (GC Robust Multiarray Averaging)</b>"))
			}else{
				Try(HTMLli(txt="<b>UnKnown (Unrecognised method - check with package maintainer)</b>"))
			}
		) #which writes to the html file for eg: "<br><li><b>GCRMA (GC Robust Multiarray Averaging)</b>"
	} #end of if (ExportNormalizationMethod)
	#
	#
	#
	if(ExportRawIntensityBoxPlot){
		Try(HTML.title("<a name=\"RawIntensityBoxPlot\">Raw (Unnormalized) Intensity Box Plot</a>",HR=2))
		#which writes for eg: <h2 > <a name="RawIntensityBoxPlot">Raw (Unnormalized) Intensity Box Plot</a></h2>
		#
		Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
		#for eg: this is:
		#AffyBatch object
		#size of arrays=640x640 features (8 kb)
		#cdf=HG_U95Av2 (12625 affyids)
		#number of samples=8
		#number of genes=12625
		#annotation=hgu95av2
		#notes=
		####################
		Try(SlideNamesVec  <- get("SlideNamesVec",envir=affylmGUIenvironment)) #for eg: [1] "Abs10.1"  "Abs10.2"  "Pres10.1" "Pres10.2" "Abs48.1"  "Abs48.2"  "Pres48.1" "Pres48.2"
		#
		#This plotFunction creates a boxplot from the raw data
		Try(
			plotFunction <- function(){
				Try(opar<-par(bg="white",cex=0.7))
				Try(boxplot(RawAffyData,col="red",las=2,names=SlideNamesVec))
				Try(title(plotTitle))
				Try(tmp<-par(opar))
			} #end of plotFunction <- function()
		)
		Try(plotTitle <- "Raw intensity distribution for each slide")
		Try(
			HTMLplotUsingFunction(
				Caption                = plotTitle,
				File                   = fileNameWithPath,
				GraphRelativeDirectory = HTMLfileRelativePath ,
				GraphAbsoluteDirectory = HTMLfilePath,
				GraphFileName          = "RawIntensityBoxPlot",
				GraphSaveAs            = "png",
				GraphBorder            = 1,
				Align                  = "left",
				plotFunction           = plotFunction,
				Width                  = plotWidth,
				Height                 = plotHeight,
				PointSize              = plotPointSize,
				BG                     = plotBG,
				res                    = plotRes
			) #end of HTMLplotUsingFunction
		) #end of Try
		#For eg: This writes to the html file:
		# <p align=left><img src='alg_Est_gcrma_3C_files/RawIntensityBoxPlot.png' border=1><br><i> Raw intensity distribution for each slide </i></P>
		#and creates a file "RawIntensityBoxPlot.png" in the "alg_Est_gcrma_3C_files" subdirectory.
	} #end of if(ExportRawIntensityBoxPlot)
	#
	#
	#
	if(ExportNormalizedIntensityBoxPlot){
		Try(HTML.title("<a name=\"NormalizedIntensityBoxPlot\">Normalized Intensity Box Plot</a>",HR=2))
		# for eg: this writes to the HTML file:
		#
		# <h2 > <a name="NormalizedIntensityBoxPlot">Normalized Intensity Box Plot</a></h2>
		#
		Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
		#For eg: this is the normalized data:
		#> summary(NormalizedAffyData.exprs)
		#  low10-1.cel       low10-2.cel       high10-1.cel      high10-2.cel      low48-1.cel       low48-2.cel       high48-1.cel      high48-2.cel
		# Min.   :-0.3586   Min.   :-0.3778   Min.   :-0.2799   Min.   :-0.2883   Min.   :-0.3431   Min.   :-0.3487   Min.   :-0.3648   Min.   :-0.3192
		# 1st Qu.: 2.8701   1st Qu.: 2.8529   1st Qu.: 3.0408   1st Qu.: 3.1354   1st Qu.: 2.8521   1st Qu.: 2.9442   1st Qu.: 2.8054   1st Qu.: 2.9633
		# Median : 4.0612   Median : 4.0319   Median : 4.1881   Median : 4.2567   Median : 3.9914   Median : 4.0573   Median : 3.9331   Median : 4.0457
		# Mean   : 5.1289   Mean   : 5.0827   Mean   : 5.2261   Mean   : 5.2530   Mean   : 5.0292   Mean   : 4.9990   Mean   : 5.0386   Mean   : 5.0201
		# 3rd Qu.: 7.2458   3rd Qu.: 7.1482   3rd Qu.: 7.2749   3rd Qu.: 7.2045   3rd Qu.: 7.0249   3rd Qu.: 6.8045   3rd Qu.: 7.1026   3rd Qu.: 6.8093
 		# Max.   :16.0300   Max.   :16.0264   Max.   :15.8995   Max.   :15.9624   Max.   :15.9858   Max.   :15.9348   Max.   :15.6545   Max.   :15.7622
 		#########################
		Try(SlideNamesVec  <- get("SlideNamesVec",envir=affylmGUIenvironment))
		#for eg: SlideNamesVec is:
		#[1] "Abs10.1"  "Abs10.2"  "Pres10.1" "Pres10.2" "Abs48.1"  "Abs48.2"  "Pres48.1" "Pres48.2"
		#########################
		#
		#This plotFunction will create a boxplot of Normalized data
		Try(
			plotFunction <- function(){
				Try(opar<-par(bg="white",cex=0.7))
				Try(boxplot(data.frame(NormalizedAffyData.exprs),col="blue",las=2,names=SlideNamesVec))
				Try(title(plotTitle))
				Try(tmp<-par(opar))
			} # end of plotFunction <- function()
		)
		Try(plotTitle<-"Normalized intensity distribution for each slide")
		Try(
			HTMLplotUsingFunction(
				Caption = plotTitle,
				File = fileNameWithPath,
				GraphRelativeDirectory = HTMLfileRelativePath,
				GraphAbsoluteDirectory = HTMLfilePath,
				GraphFileName = "NormalizedIntensityBoxPlot",
				GraphSaveAs = "png",
				GraphBorder = 1,
				Align = "left",
				plotFunction=plotFunction,
				Width=plotWidth,
				Height=plotHeight,
				PointSize=plotPointSize,
				BG=plotBG,
				res=plotRes
			) #end of HTMLplotUsingFunction
		) #end of Try
		#For eg: This writes to HTML file:
		# <p align=left><img src='alg_Est_gcrma_3C_files/NormalizedIntensityBoxPlot.png' border=1><br><i> Normalized intensity distribution for each slide </i></P>
		#########################
		#It also creates the file "NormalizedIntensityBoxPlot.png" in the "alg_Est_gcrma_3C_files" subdirectory.
		#
	} #end of if (ExportNormalizedIntensityBoxPlot)
	#
	if(ExportDesignMatrix){
		Try(design <- get("design",envir=affylmGUIenvironment))
		#For eg: This gets the design matrix:
		#             EstAbsent10 EstAbsent48 EstPresent10 EstPresent48
		#low10-1.cel            1           0            0            0
		#low10-2.cel            1           0            0            0
		#high10-1.cel           0           0            1            0
		#high10-2.cel           0           0            1            0
		#low48-1.cel            0           1            0            0
		#low48-2.cel            0           1            0            0
		#high48-1.cel           0           0            0            1
		#high48-2.cel           0           0            0            1
		#########################
		#
		Try(displayVector <- rep("g",ncol(design)+1)) #This creates: [1] "g" "g" "g" "g" "g"
		Try(displayVector[0] <- "s")
		Try(DesignXtable <- xtable(design,display=displayVector))
		#For eg. This creates:
		#% latex table generated in R 2.6.0 by xtable 1.5-1 package
		#% Thu Sep 20 22:42:27 2007
		#\begin{table}[ht]
		#\begin{center}
		#\begin{tabular}{rrrrr}
		#  \hline
		# & EstAbsent10 & EstAbsent48 & EstPresent10 & EstPresent48 \\
		#  \hline
		#low10-1.cel &   1 &   0 &   0 &   0 \\
		#  low10-2.cel &   1 &   0 &   0 &   0 \\
		#  high10-1.cel &   0 &   0 &   1 &   0 \\
		#  high10-2.cel &   0 &   0 &   1 &   0 \\
		#  low48-1.cel &   0 &   1 &   0 &   0 \\
		#  low48-2.cel &   0 &   1 &   0 &   0 \\
		#  high48-1.cel &   0 &   0 &   0 &   1 \\
		#  high48-2.cel &   0 &   0 &   0 &   1 \\
		#   \hline
		#\end{tabular}
		#\end{center}
		#\end{table}
		#########################
		#
		Try(HTML.title("<a name=\"DesignMatrix\">Design Matrix</a>",HR=2))
		# For eg. this writes to HTML file:
		#
		# <h2 > <a name="DesignMatrix">Design Matrix</a></h2>
		#########################
		Try(print(DesignXtable,type="html",file=fileNameWithPath,append=TRUE))
		#For eg. This writes to HTML file:
		#<!-- html table generated in R 2.6.0 by xtable 1.5-1 package -->
		#<!-- Thu Sep 20 22:44:52 2007 -->
		#<TABLE border=1>
		#<TR> <TH>  </TH> <TH> EstAbsent10 </TH> <TH> EstAbsent48 </TH> <TH> EstPresent10 </TH> <TH> EstPresent48 </TH>  </TR>
		#  <TR> <TD align="right"> low10-1.cel </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> low10-2.cel </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> high10-1.cel </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> high10-2.cel </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> low48-1.cel </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> low48-2.cel </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> high48-1.cel </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> </TR>
		#  <TR> <TD align="right"> high48-2.cel </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> </TR>
		#   </TABLE>
		#########################
		#
	} #end of if (ExportDesignMatrix)
	#
	#
	#
	if(ExportContrastsMatrix){
		Try(displayVector <- rep("g",ncol(contrastsMatrix)+1))
		Try(displayVector[0] <- "s")
		Try(ContrastsXtable <- xtable(contrastsMatrix,display=displayVector))
		Try(HTML.title("<a name=\"ContrastsMatrix\">Contrasts Matrix</a>",HR=2))
		Try(print(ContrastsXtable,type="html",file=fileNameWithPath,append=TRUE))
		#For eg. In a similar wat to design table, this routine writes this to the HTML file:
		# <h2 > <a name="ContrastsMatrix">Contrasts Matrix</a></h2><!-- html table generated in R 2.6.0 by xtable 1.5-1 package -->
		#<!-- Thu Sep 20 22:46:48 2007 -->
		#<TABLE border=1>
		#<TR> <TH>  </TH> <TH> (EstAbsent10)-(EstAbsent48) </TH> <TH> (EstPresent10)-(EstPresent48) </TH> <TH> (EstAbsent10)-(EstPresent48) </TH>  </TR>
		#  <TR> <TD align="right"> EstAbsent10 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> </TR>
		#  <TR> <TD align="right"> EstAbsent48 </TD> <TD align="right">  -1 </TD> <TD align="right">   0 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> EstPresent10 </TD> <TD align="right">   0 </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> </TR>
		#  <TR> <TD align="right"> EstPresent48 </TD> <TD align="right">   0 </TD> <TD align="right">  -1 </TD> <TD align="right">  -1 </TD> </TR>
		#   </TABLE>
		#########################
		#
	} #end of if (ExportContrastsMatrix)
	#
	#
	#
	if(ExportMAPlotsContrasts){
		Try(HTML.title("<a name=\"MAPlotsContrasts\">M A Plots for Contrasts</a>",HR=2))
		#For eg. This writes to HTML file:
		#
		# <h2 > <a name="MAPlotsContrasts">M A Plots for Contrasts</a></h2>
		#########################
		Try(A <- rowMeans(NormalizedAffyData.exprs))
		#For eg. This gets rowMeans, as summarized below:
		#> summary(A)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
		#-0.3351  2.9500  4.1050  5.0970  7.0480 15.9100
		#########################
		Try(pch <- 16) #set character height plot
		Try(cex <- 0.2) #set character expansion for plot
		#
		Try(
			#In this example, create an MA plot for each of the three contrast
			for (contrast in (1:NumContrasts)){ #For eg. NumContrasts = 3
				Try(plotTitle<-paste("M A Plot (",ContrastNamesVec[contrast],")",sep="")) # ContrastNamesVec =  "(EstAbsent10)-(EstAbsent48)"   "(EstPresent10)-(EstPresent48)" "(EstAbsent10)-(EstPresent48)"
				Try(HTML.title(plotTitle,HR=2))
				#For eg. this writes to HTML:
				# <h2 > M A Plot ((EstAbsent10)-(EstAbsent48))</h2><p align=left><img src='alg_Est_gcrma_3C_files/contrastMAplot.1.png' border=1><br><i> M A Plot ((EstAbsent10)-(EstAbsent48)) </i></P>
				#
				# <h2 > M A Plot ((EstPresent10)-(EstPresent48))</h2><p align=left><img src='alg_Est_gcrma_3C_files/contrastMAplot.2.png' border=1><br><i> M A Plot ((EstPresent10)-(EstPresent48)) </i></P>
				#
				# <h2 > M A Plot ((EstAbsent10)-(EstPresent48))</h2><p align=left><img src='alg_Est_gcrma_3C_files/contrastMAplot.3.png' border=1><br><i> M A Plot ((EstAbsent10)-(EstPresent48)) </i></P>
				#
				#####################
				#
				Try(M <- fit$coefficients[,contrast])
				#For eg. For contrast no.1, M is:
				#> summary(M)
				#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
				#-5.303e+00 -9.561e-02  7.819e-05  9.169e-02  2.857e-01  1.168e+01
				#> length(M)
				#[1] 12625
				#####################
				#
				#Do an MA plot
				Try(
					plotFunction <- function(){
						Try(opar<-par(bg="white"))
						Try(plot(A,M,pch=pch,cex=cex,xlab="A",ylab="M",main=plotTitle))
						Try(tmp<-par(opar))
					} #end of plotFunction <- function()
				)
				Try(
					HTMLplotUsingFunction(
						Caption = plotTitle,
						File = fileNameWithPath,
						GraphRelativeDirectory = HTMLfileRelativePath,
						GraphAbsoluteDirectory = HTMLfilePath,
						GraphFileName = paste("contrastMAplot.",contrast,sep=""),
						GraphSaveAs = "png",
						GraphBorder = 1,
						Align = "left",
						plotFunction=plotFunction,
						Width=plotWidth,
						Height=plotHeight,
						PointSize=plotPointSize,
						BG=plotBG,
						res=plotRes
					) #end of HTMLplotUsingFunction
				) #end of Try
			} #end of for (contrast in (1:NumContrasts))
		) #end of Try
	} #end of if (ExportMAPlotsContrasts)
	#
	#
	#
	if(ExportTopNNToptables || ExportCompleteToptables){
		Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))
		Try(cdfName     <- strsplit(cleancdfname(cdfName(RawAffyData)),"cdf")[[1]])#Get the cdfname from the RawAffyData
		if(!(cdfName %in% .packages(all.available=TRUE))){#then check to see if the cdfname package is available on this computer
			Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))#if it is not available then install it on this computer over the internet from the Bioc repository
			Try(assign("cdfName",cdfName,affylmGUIenvironment)) #and assign this cdfname to the  affylmGUI environment
		}#end of if (!(cdfName %in% .packages(all.available=TRUE)))
		Try(cdfenv      <- getCdfInfo(RawAffyData)) #Now get the cdfenv from the RawAffyData
		Try(genelist    <- data.frame(ID=I(ls(cdfenv)))) #get the geneID's from cdfenv and put them in the genelist. Note that the genelist = GeneID + GeneSymbol+GeneName
		Try(geneNames   <- get("geneNames"  ,envir=affylmGUIenvironment)) #get the geneNames   from the affylmGUI environment
		Try(geneSymbols <- get("geneSymbols",envir=affylmGUIenvironment)) #get the genesymbols from the affylmGUI environment
		Try(
			if(length(geneNames)==0 || length(geneSymbols)==0){
				Try(tkconfigure(.affylmGUIglobals$ttMain,cursor="watch"))
				Try(RawAffyData <- get("RawAffyData",envir=affylmGUIenvironment))#Get the RawAffyData for this if condition
				Try(cdfName     <- strsplit(cleancdfname(cdfName(RawAffyData)),"cdf")[[1]])#get the cdfname from the RawaffyData object
				if(!(cdfName %in% .packages(all.available=TRUE))){
					Try(install.packages(pkgs=cdfName, lib=.libPaths(), repos=Biobase::biocReposList(), dependencies=c("Depends", "Imports")))
				}
				Try(
					if( (cdfName %in% .packages(all.available=TRUE)) ){
						Require(cdfName)
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
					} #end of else/if( (cdfName %in% .packages(all.available=TRUE)) )
				) #end of Try
			}else{
				#if the geneNames and geneSymbols exist, then create the genelist
				Try(genelist <- cbind(as.matrix(as.character(ls(cdfenv))),as.matrix(geneSymbols),as.matrix(geneNames)))
				Try(colnames(genelist) <- c("ID","Symbol","Name"))
			}#end of else/if(length(geneNames)==0||length(geneSymbols)==0)
		) #end of Try
	}#end of if (ExportTopNNToptables || ExportCompleteToptables)
	#
	if(ExportTopNNToptables){
		Try(
			HTML.title(
				paste("<a name=\"TopNNToptables\">Top ",numberOfGenes," Differentially Expressed Genes for each Contrast in Contrasts Parameterization ",
				ContrastParameterizationNamesVec[contrastParameterizationIndex],"</a>",sep=""),
				HR=2
			) #end of HTML.title
		) #end of Try
		Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
		Try(
			if(!("Amean" %in% names(fit))){
				fit$Amean <- rowMeans(NormalizedAffyData.exprs)
			}
		)
		Try(fit$genes <- genelist)
		#
		for(coef in (1:NumContrasts)){
			Try(options(digits=3))
			###Try(table1 <- topTable2(coef=coef,number=50,                   genelist=genelist,fit=fit))
			Try(table1 <- topTable2(coef=coef,number=numberOfGenes,genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy,fit=fit))
			Try(toptableDisplay <- rep("s",ncol(table1)+1))
			Try(toptableDisplay[1] <- "d")
			Try(
				for (i in (2:(ncol(table1)+1))){
					Try(if (colnames(table1)[i-1]=="M")       toptableDisplay[i] <- "f")
					Try(if (colnames(table1)[i-1]=="A")       toptableDisplay[i] <- "f")
					Try(if (colnames(table1)[i-1]=="t")       toptableDisplay[i] <- "f")
					Try(if (colnames(table1)[i-1]=="P.Value") toptableDisplay[i] <- "e")
					Try(if (colnames(table1)[i-1]=="B")       toptableDisplay[i] <- "f")
				} #end of for (i in (2:(ncol(table1)+1)))
			)
			Try(toptableXtable <- xtable(table1,display=toptableDisplay))
			Try(
				HTML.title(
					paste(
						"Top ",
						numberOfGenes,
						" Differentially Expressed Genes for ",
						ContrastNamesVec[coef],
						" sorted by ",
						sortBy,
						", adjust method = ",
						adjustMethod,
						".",
					sep=""
					),
					HR=3
				) #end of HTML.title
			) #end of Try
			Try(print(toptableXtable,type="html",file=fileNameWithPath,append=TRUE))
		} #end of for (coef in (1:NumContrasts))
	} #end of if (ExportTopNNToptables)
	#
	if(ExportCompleteToptables){
		Try(
			HTML.title(
				paste("<a name=\"CompleteToptables\">Complete Tables of Genes Ranked in order of Evidence for Differential Expression for each contrast in Contrasts Parameterization ",ContrastParameterizationNamesVec[contrastParameterizationIndex],"</a>",sep=""),
				HR=2
			) #end of HTML.title
		) #end of Try
		#
		Try(NormalizedAffyData.exprs <- get("NormalizedAffyData.exprs",envir=affylmGUIenvironment))
		Try(
			if(!("Amean" %in% names(fit))){
				fit$Amean <- rowMeans(NormalizedAffyData.exprs)
			} #end of if(!("Amean" %in% names(fit)))
		)
		Try(fit$genes <- genelist)
		#
		for(coef in (1:NumContrasts)){
			Try(options(digits=3))
			###Try(table1 <- topTable2(coef=coef,number=nrow(genelist),genelist=genelist,fit=fit))
			Try(table1 <- topTable2(coef=coef,number=nrow(genelist),genelist=genelist,adjust.method=adjustMethod,sort.by=sortBy,fit=fit))
			Try(ToptableAbsoluteFilename <- paste(HTMLfilePath ,.Platform$file.sep,"CompleteToptable_Contrast",coef,".xls",sep=""))
			Try(ToptableRelativeFilename <- paste(HTMLfileRelativePath ,.Platform$file.sep,"CompleteToptable_Contrast",coef,".xls",sep=""))
			Try(write.table(table1,file=ToptableAbsoluteFilename,quote=FALSE,col.names=NA,sep="\t"))
			Try(HTML.title(paste("Complete Table of Genes Ranked in order of Evidence for Differential Expression for ",ContrastNamesVec[coef]),HR=3))
			Try(HTMLli(txt=paste("<a href=\"",ToptableRelativeFilename,"\"><b>",paste("CompleteToptable_Contrast",coef,".xls",sep=""),"</b></a>",sep="")))
		} #end of for (coef in (1:NumContrasts))
	} #end of if (ExportCompleteToptables)
	#
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
