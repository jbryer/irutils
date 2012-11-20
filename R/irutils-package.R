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
#' @import reshape ggplot2 tools xtable
NULL

cranMain <- 'http://cran.r-project.org' #Main CRAN
cranExtra <- 'http://www.stats.ox.ac.uk/pub/RWin' #Windows Binaries for some packages

.onAttach <- function(libname, pkgname) {
  	pkgEnv = pos.to.env(match('package:irutils', search()))	
}
