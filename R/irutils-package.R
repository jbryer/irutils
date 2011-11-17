#' Utilities for Intitutional Research.
#' This package is designed primarly for institutional researchers, though many
#' of the functions provided are appropriate for a broader audience. Many of the
#' functions are documented in the (forthcoming) manual, Introduction to R and
#' LaTeX for Institutional Research. 
#' 
#' @name itutils-package
#' @aliases irutils
#' @docType package
#' @title Utility functions for Institutional Research
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research
#' @import reshape ggplot2 tools
NULL

#' North American (i.e. Canada, Mexico, and United States) results from the 2009
#' Programme of International Student Assessment (PISA)
#' as provided by the Organization for Economic Co-operation and Development (OECD).
#' See \url{http://www.pisa.oecd.org/} for more information including the code book.
#'
#' @name pisa
#' @docType data
#' @format a data frame 66,690 ovservations of 437 variables from North America.
#' @source Organization for Economic Co-operation and Development
#' @keywords datasets
NULL

sqlrepos <<- NULL
cranMain <<- 'http://cran.r-project.org' #Main CRAN
cranExtra <<- 'http://www.stats.ox.ac.uk/pub/RWin' #Windows Binaries for some packages

.onLoad <- function(libname, pkgname) {
	sqlrepos <<- paste(system.file(package='irutils'), '/data', sep='')
	geolite.location <<- data.frame()
	geolite.blocks <<- data.frame()
}
