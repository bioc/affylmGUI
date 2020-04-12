#  UTILITY FUNCTIONS

.cdfName2AnnPkg <- function(x)
#	Construct probe annotation package names from cdfName
#	Gordon Smyth
#	Created 10 April 2020
{
	paste0(x,".db")
}
