### R code from vignette source 'irutils.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(RODBC)
library(ggplot2)

theme_update(panel.background=theme_blank(), panel.grid.major=theme_blank(), panel.border=theme_blank())


###################################################
### code chunk number 2: loadecir
###################################################
library(ecir, lib.loc='O:/Resources/R/library')


###################################################
### code chunk number 3: installPackage (eval = FALSE)
###################################################
## library(devtools)
## install_github('devtools', 'jbryer')


###################################################
### code chunk number 4: loadirutils
###################################################
library(irutils)
ls('package:irutils')


###################################################
### code chunk number 5: getSQLRepos
###################################################
getSQLRepos()


###################################################
### code chunk number 6: setSQLRepos
###################################################
setSQLRepos( "O:/Resources/R Packages/ecir/data")


###################################################
### code chunk number 7: getQueries
###################################################
getQueries()


###################################################
### code chunk number 8: irutils.Rnw:99-101
###################################################
getQueryDesc('GraduatesWithinRange')
getParameters('GraduatesWithinRange')


###################################################
### code chunk number 9: irutils.Rnw:104-107
###################################################
channel <<- NULL
dbConnect('live','jbryer','leidseplein')
#setwd('C:/Temp')


###################################################
### code chunk number 10: irutils.Rnw:112-114 (eval = FALSE)
###################################################
## graduates = execQuery('GraduatesWithinRange', 
## 	startDate='01-JUL-2010', endDate='30-JUN-2011')


###################################################
### code chunk number 11: irutils.Rnw:117-119
###################################################
graduates = cacheQuery('GraduatesWithinRange', 
	startDate='01-JUL-2010', endDate='30-JUN-2011')


###################################################
### code chunk number 12: irutils.Rnw:144-146
###################################################
query = getQuery('GraduatesWithinRange')
strwrap(query, width=80, exdent=5)


###################################################
### code chunk number 13: irutils.Rnw:153-172
###################################################
data(pisa)

items28 = pisa[,substr(names(pisa), 1,5) == 'ST24Q']
names(items28) = c("I read only if I have to.",
   "Reading is one of my favorite hobbies.",
   "I like talking about books with other people.",
   "I find it hard to finish books.",
   "I feel happy if I receive a book as a present.",
   "For me, reading is a waste of time.",
   "I enjoy going to a bookstore or a library.",
   "I read only to get information that I need.",
   "I cannot sit still and read for more than a few minutes.",
   "I like to express my opinions about books I have read.",
   "I like to exchange books with my friends")
for(i in 1:ncol(items28)) {
	items28[,i] = factor(items28[,i], levels=1:4, 
		labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'),
		ordered=TRUE)
}


###################################################
### code chunk number 14: PISAItem28BarchartTable1
###################################################
print(plotBarchartTable(items28, low.color='maroon', high.color='burlywood4'))


###################################################
### code chunk number 15: PISAItem28BarchartTable2
###################################################
p = plotBarchartTable(items28, grouping=pisa$CNT, low.color='maroon', high.color='burlywood4')
print(p)


