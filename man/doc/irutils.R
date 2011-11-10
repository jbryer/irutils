### R code from vignette source 'irutils.Rnw'

###################################################
### code chunk number 1: irutils.Rnw:44-45
###################################################
library(ecir, lib.loc='O:/Resources/R/library')


###################################################
### code chunk number 2: irutils.Rnw:55-56
###################################################
getSQLRepos()


###################################################
### code chunk number 3: irutils.Rnw:59-60 (eval = FALSE)
###################################################
## setSQLRepos( "O:/Resources/R Packages/ecir/data")


###################################################
### code chunk number 4: irutils.Rnw:65-66
###################################################
getQueries()


###################################################
### code chunk number 5: irutils.Rnw:71-73
###################################################
getQueryDesc('GraduatesWithinRange')
getParameters('GraduatesWithinRange')


###################################################
### code chunk number 6: irutils.Rnw:76-79
###################################################
channel <<- NULL
dbConnect('live','jbryer','leidseplein')
setwd('C:/Temp')


###################################################
### code chunk number 7: irutils.Rnw:84-85 (eval = FALSE)
###################################################
## graduates = execQuery('GraduatesWithinRange', startDate='01-JUL-2010', endDate='30-JUN-2011')


###################################################
### code chunk number 8: irutils.Rnw:88-89
###################################################
graduates = cacheQuery('GraduatesWithinRange', startDate='01-JUL-2010', endDate='30-JUN-2011')


###################################################
### code chunk number 9: irutils.Rnw:114-116
###################################################
query = getQuery('GraduatesWithinRange')
strwrap(query, width=80, exdent=5)


