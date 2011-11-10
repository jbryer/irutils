#' Database Access Utility Functions
#' 
#' @name itutils-package
#' @aliases irutils
#' @docType package
#' @title Utility functions for Institutional Research
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research
NULL

#' United States results from the 2009 Programme of International Student Assessment (PISA)
#' as provided by the Organization for Economic Co-operation and Development (OECD).
#' See \url{http://www.pisa.oecd.org/} for more information including the code book.
#'
#' @name pisa
#' @docype data
#' @format a data frame 5,233 ovservations of 437 variables.
NULL

sqlrepos <- NULL

.onLoad <- function(libname, pkgname) {
	sqlrepos <- paste(system.file(package='irutils'), '/data', sep='')
}
