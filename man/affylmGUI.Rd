\title{Graphical User Interface for the limma microarray package}
\description{Graphical User Interface for limma package analysis of Affymetrix microarrays.}

\name{affylmGUI}

\alias{affylmGUI}

\usage{
affylmGUI(BigfontsForaffylmGUIpresentation=FALSE)
}

\arguments{
	\item{BigfontsForaffylmGUIpresentation}{logical, if \code{TRUE} then larger fonts are used.  However, some font
	sizes are not controlled by affylmGUI and so must be adjusted in the operating system, e.g.,
	in the Control Panel in Windows under Display, Appearance.}
}

\details{
The \code{affylmGUI} function launches a Graphical User Interface for the affy, gcrma, affyPLM and limma packages.
The GUI uses Tk widgets (via the R TclTk interface by Peter Dalgaard) in order to provide
a simple interface to limma functions for linear modelling of Affymetrix microarrays and
identification of differentially expressed genes.

Almost all users will type \code{affylmGUI()}.
The use of \code{affylmGUI(Bigfonts=TRUE)} is only intended for the preparation of a talk about the affylmGUI package.
}

\author{
Created by James Wettenhall between 2003 and September 2006.
Maintained by Keith Satterley between September 2006 and July 2015.
Currently maintained by Gordon Smyth.
}

\seealso{
\code{\link{affylmGUI-package}},
\code{\link{internal}}
}
