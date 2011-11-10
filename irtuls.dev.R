require(devtools)
require(roxygen2)
require(RSQLite)
require(ipeds)

setwd("~/Dropbox/Projects/irutils")
setwd("C:/Dropbox/My Dropbox/Projects")

#Package building
document("irutils", clean=TRUE)
check_doc("irutils")
build("irutils", binary=FALSE)
build("irutils", binary=TRUE)
install("irutils")
check("irutils")
library(irutils)
ls('package:irutils')


#Setup the SQLite database
data(surveys)
drv = dbDriver("SQLite")
conn = dbConnect(drv, dbname=paste(getwd(), '/irutils/data/ipeds.db', sep=''))
dbWriteTable(conn, "surveys", surveys, row.names=0)
saveIPEDStoDB <- function(conn, surv=surveys$SurveyID, years=2010:2006) {
	for(s in surv) {
		if(dbExistsTable(conn, s)) {
			dbRemoveTable(conn, s)
		}
		for(y in years[order(years, decreasing=TRUE)]) {
			tab = NULL
			tab = tryCatch(getIPEDSSurvey(s, y), error=function(e) { print(paste("Error downloading",s,"for year",y)); return(NULL) } )
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

saveIPEDStoDB(conn, surv=surveys$SurveyID, years=2010:2006)

dbListTables(conn)

dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")

hd = dbReadTable(conn, 'HD')
table(hd$year, useNA='ifany')
dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")


dbDisconnect(conn)

#Build Vignette
setwd("C:/Dropbox/My Dropbox/Projects/irutils")
setwd(paste(getwd(), '/man/doc/', sep=''))
getwd()
Stangle('irutils.Rnw')
Sweave('irutils.Rnw')
texi2dvi('irutils.tex', pdf=TRUE)


