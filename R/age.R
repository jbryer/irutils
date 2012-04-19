#' Returns the age in years based upon the given date.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getAge <- function (dateCol, calcDate) {
    as.numeric((as.Date(calcDate) - dateCol)/365.25)
}

#' Returns a factor variable using the given breaks and labels. Will calculate the
#' age based upon the calcDate if given, otherwise the ageCol will be used.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getAgeGroups <- function (dateCol, calcDate, breaks, labels, 
		ageCol = NULL) {
    if (is.null(ageCol)) {
        age = getAge(dateCol, calcDate)
    }
    else {
        age = ageCol
    }
    cut(age, breaks = breaks, labels = labels)
}

#' Returns the age groupings used by IPEDS.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getAgeGroupsIPEDS <- function (dateCol, calcDate, ageCol = NULL) {
	breaks = c(0, 17, 19, 21, 24, 29, 34, 39, 49, 64, Inf)
	labels = c("<18", "18-19", "20-21", "22-24", "25-29", 
		"30-34", "35-39", "40-49", "50-64", "65+")
    if (is.null(ageCol)) {
        r = getAgeGroups(dateCol, calcDate, breaks, labels)
    } else {
        r = getAgeGroups(breaks=breaks, labels=labels, ageCol=ageCol)
    }
    return(r)
}
