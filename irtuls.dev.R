install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/Projects") #Windows

#Package building
document("irutils")
check_doc("irutils")
build("irutils", binary=FALSE)
build("irutils", binary=TRUE)
install("irutils")
check("irutils")
library(irutils)
ls('package:irutils')

#Load included data
data(students)

#Resave rda files to make them smaller
tools:::resaveRdaFiles('irutils/data')

#Setup the SQLite database
data(surveys)
drv = dbDriver("SQLite")
conn = dbConnect(drv, dbname=paste(getwd(), '/irutils/data/ipeds.db', sep=''))
saveIPEDStoDB(conn, surv=surveys$SurveyID, years=2010:2009)
dbListTables(conn)

dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")

hd = dbReadTable(conn, 'HD')
table(hd$year, useNA='ifany')
dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")

dbDisconnect(conn)

#Test Google geocode


