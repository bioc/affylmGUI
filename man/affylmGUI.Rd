\title{Graphical User Interface for the limma microarray package}

\name{affylmGUI}

\alias{AboutaffylmGUI}
\alias{affyHelp}
\alias{affylmGUIhelp}
\alias{affylmGUI}
\alias{affylmGUI}
\alias{affyPlotMAcontrast}
\alias{affyPlotMA}
\alias{ALGlimmaUsersGuide}
\alias{ChooseCDF}
\alias{ChooseContrastParameterization}
\alias{chooseDir}
\alias{ChooseEbayesStatistic}
\alias{ComputeContrasts}
\alias{ComputeLinearModelFit}
\alias{CopyGraph}
\alias{DeleteContrastParameterization}
\alias{deleteItemFromList}
\alias{DensityPlotAll}
\alias{DensityPlot}
\alias{evalRcode}
\alias{ExportHTMLreport}
\alias{ExportNormalizedExpressionValues}
\alias{ExportTopTable}
\alias{fixSeps}
\alias{generalPlotFunction}
\alias{GetComponentsToExportInHTMLreport}
\alias{GetContrastNamesForHeatDiagram}
\alias{GetContrastParameterizationName}
\alias{GetContrasts}
\alias{GetContrast}
\alias{GetDEcutoff}
\alias{GetGeneLabelsOptions}
\alias{GetJpegOrPngParams}
\alias{GetJpegOrPngX11Params}
\alias{GetlimmaDataSetName}
\alias{GetLogPLMDataChoice}
\alias{GetMultipleContrasts}
\alias{GetNormalizationMethod}
\alias{getPackageVersion}
\alias{GetParameterizationName}
\alias{GetPlotLabels}
\alias{GetPlotSize}
\alias{GetPlotTitle}
\alias{GetPValueCutoff}
\alias{GetResidualTypeChoice}
\alias{GetRNATypesFrom.ContrastsFromDropDowns.String}
\alias{GetSetNames}
\alias{GetSlideNums}
\alias{GetSlideNum}
\alias{GetWhichProbes}
\alias{GetWtAreaParams}
\alias{GetWtFlagParams}
\alias{HeatDiagramDialog}
\alias{HeatDiagramPlot}
\alias{HTMLplotUsingFunction}
\alias{ImageArrayPlot}
\alias{ImageQualityResidualPlot}
\alias{ImageQualityWeightPlot}
\alias{initGlobals}
\alias{IntensityHistogramAll}
\alias{IntensityHistogram}
\alias{limmaHelp}
\alias{LogOddsPlot}
\alias{NewLimmaFile}
\alias{NormalizedIntensityBoxPlot}
\alias{NormalizeNow}
\alias{nstrstr}
\alias{NUSEPlotAll}
\alias{onDestroy}
\alias{onExit}
\alias{OpenALimmaFile}
\alias{OpenCDFandTargetsfiles}
\alias{OpenCDFFile}
\alias{OpenLimmaFile}
\alias{OpenTargetsFile}
\alias{PlotOptions}
\alias{QQTplot}
\alias{RawIntensityBoxPlot}
\alias{Require}
\alias{Resize}
\alias{RLEPlotAll}
\alias{RNADegradationPlotAll}
\alias{SaveAsLimmaFile}
\alias{SaveGraphAsJpeg}
\alias{SaveGraphAsPDF}
\alias{SaveGraphAsPNG}
\alias{SaveGraphAsPostscript}
\alias{SaveLimmaFile}
\alias{SetupPlotKeyBindings}
\alias{SetupPlotMenus}
\alias{SetWD}
\alias{showChangeLog}
\alias{showCitations}
\alias{showTopTable}
\alias{SimplifyContrastsExpression}
\alias{strstr}
\alias{tclArrayVar}
\alias{TclRequire}
\alias{topTable2}
\alias{toptable2}
\alias{TryReadImgProcFile}
\alias{Try}
\alias{UpDownOrBoth}
\alias{vennDiagramaffylmGUI}
\alias{VennDiagramPlot}
\alias{ViewContrastsMatrixAsPairs}
\alias{ViewContrastsMatrixInTable}
\alias{ViewExistingContrastParameterization}
\alias{ViewRNATargets}

\description{Graphical User Interface for Affy analysis using the limma microarray package}


\usage{
AboutaffylmGUI()
affyHelp()
affylmGUI(BigfontsForaffylmGUIpresentation=FALSE)
affylmGUIhelp()
affyPlotMA()
affyPlotMAcontrast()
ALGlimmaUsersGuide(view=TRUE)
ChooseCDF()
ChooseContrastParameterization()
chooseDir()
ChooseEbayesStatistic()
ComputeContrasts()
ComputeLinearModelFit()
CopyGraph(img)
DeleteContrastParameterization()
deleteItemFromList(list1,itemName=NULL,index=NULL)
DensityPlot()
DensityPlotAll()
evalRcode()
ExportHTMLreport()
ExportNormalizedExpressionValues()
ExportTopTable()
fixSeps(string)
generalPlotFunction(code="",WindowTitle="")
GetComponentsToExportInHTMLreport(contrastParameterizationIndex=NULL)
GetContrast(contrastParameterizationIndex)
GetContrastNamesForHeatDiagram(numContrasts=2,ContrastNames=c("Contrast 1","Contrast 2"))
GetContrastParameterizationName()
GetContrasts(NumContrasts=0)
GetDEcutoff()
GetGeneLabelsOptions()
GetJpegOrPngParams(graphFileType)
GetJpegOrPngX11Params(graphFileType)
GetlimmaDataSetName()
GetLogPLMDataChoice()
GetMultipleContrasts(contrastParameterizationIndex)
GetNormalizationMethod()
getPackageVersion(pkgName)
GetParameterizationName()
GetPlotLabels(plottitle="",xlabel="",ylabel="")
GetPlotSize()
GetPlotTitle(plottitle="")
GetPValueCutoff(p.value=0.01)
GetResidualTypeChoice()
GetRNATypesFrom.ContrastsFromDropDowns.String(string)
GetSetNames(numSets=2,set1="",set2="",set3="")
GetSlideNum(all=FALSE)
GetSlideNums()
GetWhichProbes(includeBoth=FALSE)
GetWtAreaParams()
GetWtFlagParams()
HeatDiagramDialog(parameterName)
HeatDiagramPlot()
HTMLplotUsingFunction(Caption = "", File = .HTML.file, GraphRelativeDirectory = ".", GraphAbsoluteDirectory = NULL, GraphFileName = "", GraphSaveAs = "png", GraphBorder = 1,  Align = "center", plotFunction = NULL,Width=600,Height=600,PointSize=12,BG="white",res=72,\dots)
ImageArrayPlot()
ImageQualityResidualPlot()
ImageQualityWeightPlot()
initGlobals()
IntensityHistogram()
IntensityHistogramAll()
limmaHelp()
LogOddsPlot()
NewLimmaFile()
NormalizedIntensityBoxPlot()
NormalizeNow()
nstrstr(haystack,needle)
NUSEPlotAll()
onDestroy()
onExit()
OpenALimmaFile(FileName)
OpenCDFandTargetsfiles()
OpenCDFFile()
OpenLimmaFile(FileName)
OpenTargetsFile()
PlotOptions()
QQTplot()
RawIntensityBoxPlot()
Require(pkg)
Resize(img,plotFunction)
RLEPlotAll()
RNADegradationPlotAll()
SaveAsLimmaFile()
SaveGraphAsJpeg(initialfile, plotFunction)
SaveGraphAsPDF(initialfile, plotFunction)
SaveGraphAsPNG(initialfile, plotFunction)
SaveGraphAsPostscript(initialfile, plotFunction)
SaveLimmaFile()
SetupPlotKeyBindings(tt,img)
SetupPlotMenus(tt,initialfile,plotFunction,img)
SetWD()
showChangeLog(n = 20)
showCitations()
showTopTable(\dots,export=FALSE)
SimplifyContrastsExpression(string)
strstr(haystack, needle)
tclArrayVar()
TclRequire(tclPkg)
topTable2(fit, coef = 1, number = 10, genelist = NULL,adjust.method = "holm", sort.by = "B")
toptable2(fit, coef = 1, number = 10, genelist = NULL, A = NULL,eb = NULL, adjust.method = "holm", sort.by = "B", \dots)
Try(expr)
TryReadImgProcFile(expr)
UpDownOrBoth()
vennDiagramaffylmGUI(object, include = "both", names, cex = 1.5, mar = rep(1, 4), ...)
VennDiagramPlot()
ViewContrastsMatrixAsPairs(contrastsMatrix, contrastsMatrixList,contrastParameterizationIndex = NULL)
ViewContrastsMatrixInTable(contrastsMatrixList, contrastParameterizationIndex = NULL)
ViewExistingContrastParameterization()
ViewRNATargets()
}

\arguments{
	\item{BigfontsForaffylmGUIpresentation}{affylmGUI:arg1:If set to TRUE, larger fonts are used.  However, some font
	sizes are not controlled by affylmGUI and so must be adjusted in the operating system, e.g.
	in the Control Panel in Windows under Display, Appearance.}
	\item{\dots}{HTMLplotUsingFunction:arg14:and:showTopTable:arg1:and:toptable2:arg9:and:vennDiagramaffylmGUI:arg6}
	\item{Align}{HTMLplotUsingFunction:arg8}
	\item{all}{GetSlideNum:arg1}
	\item{BG}{HTMLplotUsingFunction:arg12}
	\item{Caption}{HTMLplotUsingFunction:arg1}
	\item{code}{generalPlotFunction: arg1: a code}
	\item{ContrastNames}{GetContrastNamesForHeatDiagram:arg2}
	\item{contrastParameterizationIndex}{GetComponentsToExportInHTMLreport:arg1:and:contrastParameterizationIndex:arg1:and:GetContrast:arg1:and:HTMLplotUsingFunction:arg3:and:ViewContrastsMatrixInTable:arg2}
	\item{export}{showTopTable:arg2}
	\item{expr}{Try:arg1:and:TryReadImgProcFile:arg1}
	\item{FileName}{OpenALimmaFile:arg1:and:OpenLimmaFile:arg1}
	\item{File}{HTMLplotUsingFunction:arg2}
	\item{GraphAbsoluteDirectory}{HTMLplotUsingFunction:arg4}
	\item{GraphBorder}{HTMLplotUsingFunction:arg7}
	\item{GraphFileName}{HTMLplotUsingFunction:arg5}
	\item{graphFileType}{GetJpegOrPngParams:arg1:and:GetJpegOrPngX11Params:arg1}
	\item{GraphRelativeDirectory}{HTMLplotUsingFunction:arg3}
	\item{GraphSaveAs}{HTMLplotUsingFunction:arg6}
	\item{haystack}{nstrstr:arg1:and:strstr:arg1}
	\item{Height}{HTMLplotUsingFunction:arg11}
	\item{img}{Resize:arg1:and:SetupPlotMenus:arg4:and:CopyGraph:arg1:and:SetupPlotKeyBindings:arg2}
	\item{includeBoth}{GetWhichProbes:arg1}
	\item{index}{deleteItemFromList:arg3:an index}
	\item{initialfile}{SetupPlotMenus:arg2:and:SaveGraphAsJpeg:arg1:and:SaveGraphAsPDF:arg1:and:SaveGraphAsPNG:arg1:and:SaveGraphAsPostscript:arg1}
	\item{itemName}{deleteItemFromList:arg2:an item name}
	\item{list1}{deleteItemFromList:arg1:a list}
	\item{needle}{nstrstr:arg2:and:strstr:arg2}
	\item{NumContrasts}{GetContrasts:arg1}
	\item{numContrasts}{GetContrastNamesForHeatDiagram:arg1}
	\item{numSets}{GetSetNames:arg1}
	\item{p.value}{GetPValueCutoff:arg1:A p-value cutoff.}
	\item{parameterName}{HeatDiagramDialog:arg1}
	\item{pkgName}{getPackageVersion:arg1}
	\item{pkg}{Require:arg1}
	\item{plotFunction}{Resize:arg2:and:HTMLplotUsingFunction:arg9:and:SetupPlotMenus:arg3:and:SaveGraphAsJpeg:arg2:and:SaveGraphAsPDF:arg2:and:SaveGraphAsPNG:arg2:and:SaveGraphAsPostscript:arg2}
	\item{plottitle}{GetPlotLabels:arg1:and:GetPlotTitle:arg1}
	\item{res}{HTMLplotUsingFunction:arg13}
	\item{set1}{GetSetNames:arg2}
	\item{set2}{GetSetNames:arg3}
	\item{set3}{GetSetNames:arg4}
	\item{string}{fixSeps:arg1:A character string:and:GetRNATypesFrom.ContrastsFromDropDowns.String:arg1:and:SimplifyContrastsExpression:arg1}
	\item{tclPkg}{TclRequire:arg1}
	\item{tt}{SetupPlotMenus:arg1:and:SetupPlotKeyBindings:arg1}
	\item{view}{ALGlimmaUsersGuide:arg1:A boolean determinig if the users'd guide will be viewed}
	\item{Width}{HTMLplotUsingFunction:arg10}
	\item{WindowTitle}{generalPlotFunction: arg2: a title}
	\item{xlabel}{GetPlotLabels:arg2}
	\item{ylabel}{GetPlotLabels:arg3}
	\item{fit}{topTable2:arg1:and:toptable2:arg1}
	\item{coef}{topTable2:arg2:and:toptable2:arg2}
	\item{number}{topTable2:arg3:and:toptable2:arg3}
	\item{genelist}{topTable2:arg4:and:toptable2:arg4}
	\item{A}{toptable2:arg5}
	\item{eb}{toptable2:arg6}
	\item{adjust.method}{topTable2:arg5:and:toptable2:arg7}
	\item{sort.by}{topTable2:arg6:and:toptable2:arg8}
	\item{object}{vennDiagramaffylmGUI:arg1}
	\item{include}{vennDiagramaffylmGUI:arg2}
	\item{names}{vennDiagramaffylmGUI:arg3}
	\item{cex}{vennDiagramaffylmGUI:arg4}
	\item{mar}{vennDiagramaffylmGUI:arg5}
	\item{contrastsMatrix}{ViewContrastsMatrixAsPairs:arg1}
	\item{contrastsMatrixList}{ViewContrastsMatrixAsPairs:arg2:and:ViewContrastsMatrixInTable:arg1}
	\item{n}{showChangeLog:arg1}
	\item{PointSize}{HTMLplotUsingFunction:arg12}
}

\details{
This function launches a Graphical User Interface for the limma package by Gordon Smyth.
The GUI uses Tk widgets (via the R TclTk interface by Peter Dalgaard) in order to provide
a simple interface to the limma functions for linear modelling of microarrays and
identification of differentially expressed genes.
}

\author{James Wettenhall}

\keyword{IO}
