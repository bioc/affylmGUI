\name{affylmGUI-package}
\docType{package}
\alias{affylmGUI-package}
\title{The affylmGUI Package: Graphical User Interface for limma analysis of Affymetrix microarrays}

\description{
The affylmGUI package provides a Graphical User Interface (GUI) for analysis of Affymetrix microarray data using the limma package (Linear Modes for MicroArray data).
}

\details{
The GUI is launched by typing \code{affylmGUI()} at the R prompt.
For detailed help, including example sesssions, see the affylmGUI home page at \url{http://bioinf.wehi.edu.au/affylmGUI}.

The limma package offers R users a command-line interface to state-of-the-art techniques for linear modelling of microarray data and for identifying differentially expressed genes (Ritchie et al, 2015).
The affylmGUI package, while not as powerful as limma to the expert user, offers a simple point-and-click interface to many of the commonly-used limma and affy functions.

To use the affylmGUI package, you need to have R 1.8.1 or later, Tcl/Tk 8.3 or later (ActiveTcl for Windows/Linux or Fink Tcl/Tk for MacOSX (X11)) and the limma, affylmGUI and tkrplot R packages.
At least one of the affy, gcrma or affyPLM Bioconductor packages are also required for normalization and probe-set summarization.
The R2HTML (CRAN) and xtable (CRAN) packages are recommended.
affylmGUI has been tested succesfully on Windows, Red Hat Linux, and on Mac OSX with X11.

Please cite Wettenhall et al (2006) if you use results from affylmGUI in a publication.
Also consider citing Irizarry et al (2003) if RMA normalization is used for Wu et al (2004) if gcrma normalization is used.
}

\author{Created by James Wettenhall between 2003 and September 2006. Maintained by Keith Satterley between September 2006 and July 2015. Currently maintained by Gordon Smyth.}

\references{
Wettenhall, JM, Simpson, KM, Satterley, K, Smyth, GK (2006).
affylmGUI: a graphical user interface for linear modeling of single channel microarray data.
\emph{Bioinformatics} 22, 897-899.

Ritchie, ME, Phipson, B, Wu, D, Hu, Y, Law, CW, Shi, W, and Smyth, GK (2015).
limma powers differential expression analyses for RNA-sequencing and microarray studies.
\emph{Nucleic Acids Research} 43, e47.
\url{http://nar.oxfordjournals.org/content/43/7/e47}

Irizarry, RA, Hobbs, B, Collin, F, Beazer-Barclay, YD, Antonellis, KJ, Scherf, U, Speed, TP (2003).
Exploration, normalization, and summaries of high density oligonucleotide array probe level data.
Biostatistics 4(2), 249-264.

Wu, Z, Irizarry, RA, Gentleman, R, Martinez-Murillo, F, Spencer, F. (2004). A model based background
adjustment for oligonucleotide expression arrays. Journal of the American Statistical Association 99(468),
909-917.
}

\examples{
library(limma)
changeLog(package="affylmGUI")
}

\seealso{
The GUI is launched by \code{\link{affylmGUI}()}.

More documentation is available from \url{http://bioinf.wehi.edu.au/affylmGUI}.

\code{\link{showChangeLog}()} shows the most recent lines from the affylmGUI package changeLog.

See \code{\link{limma-package}} for help about the limma package.
}
