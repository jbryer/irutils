#' Database Access Utility Functions
#' 
#' @name itutils-package
#' @aliases irutils
#' @docType package
#' @title Utility functions for Institutional Research
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research
#' @import reshape ggplot2
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

sqlrepos <- NULL

.onLoad <- function(libname, pkgname) {
	sqlrepos <- paste(system.file(package='irutils'), '/data', sep='')
}
