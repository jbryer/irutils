#' This function will add all the IPEDS surveys to the given database (vis-a-vis the
#' connection object). Typically this will be a SQLite database. Note that this
#' function will make efforts to conform differing table structres across years but
#' it is up to the user to verify the data integrity. Typically variables present in
#' one year and not others will simply be identified as missing (i.e. NA). Since
#' IPEDS periodically adds, removes, and changes variables problems may occur.
#' @export
saveIPEDStoDB <- function(conn, surv=surveys$SurveyID, years=2010:2006) {
	data(surveys)
	dbWriteTable(conn, "surveys", surveys, row.names=0)
	for(s in surv) {
		if(dbExistsTable(conn, s)) {
			dbRemoveTable(conn, s)
		}
		for(y in years[order(years, decreasing=TRUE)]) {
			tab = NULL
			tab = tryCatch(getIPEDSSurvey(s, y), error=function(e) { 
				print(paste("Error downloading",s,"for year",y)); return(NULL) } )
			if(!is.null(tab)) {
				tab$year = y
				if(dbExistsTable(conn, s)) {
					fields = dbListFields(conn, s)
					tab = tab[,names(tab) %in% fields]
					tab[,fields[!fields %in% names(tab)]] = NA
					tab = tab[,fields]
					dbWriteTable(conn, s, tab, row.names=FALSE, append=TRUE)
				} else {
					dbWriteTable(conn, s, tab, row.names=FALSE, append=FALSE)
				}
			}
		}
	}
}
