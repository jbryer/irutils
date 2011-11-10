#' Database Access Utility Functions
#' 
#' @name itutils-package
#' @aliases irutils
#' @docType package
#' @title Utility functions for Institutional Research
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research
NULL

sqlrepos <- NULL

.onLoad <- function(libname, pkgname) {
	sqlrepos <- paste(system.file(package='irutils'), '/data', sep='')
}
