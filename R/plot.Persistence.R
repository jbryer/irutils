#'
#' @export
plotPersistence <- function(students, grads, summary=NULL) {
	if(is.null(summary)) {
		long2 = retention(students, grads)
	} else {
		long2 = summary
	}
	
	wt = cast(long2, Month ~ Cohort, value='Enrollments')
	pr = cast(long2, Month ~ Cohort, value='PersistenceRate')
	gr = cast(long2, Month ~ Cohort, value='GraduationRate')
	rr = cast(long2, Month ~ Cohort, value='RetentionRate')
	grt = data.frame(x=gr$Month, y=apply(wt[,2:ncol(wt)] * gr[,2:ncol(gr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	rrt = data.frame(x=rr$Month, y=apply(wt[,2:ncol(wt)] * rr[,2:ncol(rr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	prt = data.frame(x=pr$Month, y=apply(wt[,2:ncol(wt)] * pr[,2:ncol(pr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	s = cbind(grt, rrt[,2], prt[,2])
	names(s) = c('x', 'GraduationRate', 'RetentionRate', 'PersistenceRate')
	s$PersistenceRate2 = (.01 * s$PersistenceRate * (s$RetentionRate-s$GraduationRate) + s$GraduationRate)
	currentPersistenceRate = table(students[which(students$CREATED_DATE == max(students$CREATED_DATE, na.rm=TRUE)),]$PERSIST_FLAG)
	currentPersistenceRate = as.numeric(100 * currentPersistenceRate["Y"] / sum(currentPersistenceRate))
	
	p = ggplot(s, aes(x=x)) + 
		geom_path(aes(y=(RetentionRate-GraduationRate)), colour='black') + 
		geom_ribbon(ymin=0, aes(ymax=(PersistenceRate2-GraduationRate)), fill='green', alpha=.5) +
		geom_ribbon(aes(ymin=(PersistenceRate2-GraduationRate), ymax=(RetentionRate-GraduationRate)), fill='yellow', alpha=.5) +
		geom_ribbon(aes(ymin=(RetentionRate-GraduationRate), ymax=(RetentionRate),), fill='blue', alpha=.1) +
		geom_path(aes(y=PersistenceRate2-GraduationRate), colour='grey') + 
		#geom_path(aes(y=(PersistenceRate)), colour='green') + 
		ylim(c(0,100)) + xlim(c(1,100)) +
		xlab('Months Since Enrollment') + ylab('Percentage') 
	#p = p + geom_hline(yintercept=currentPersistenceRate)
	#p = p + geom_text(y=currentPersistenceRate, label=paste("Current Institutional Persistence Rate: ", round(currentPersistenceRate), "%", sep=''), aes(x=100), colour='black', vjust=-.5, hjust=1, size=3.5)
	p = p + geom_vline(xintercept=15)
	angle = 0
	p = p + geom_text(aes(x=15), y=100, label=paste("15-month Retention Rate: ", round(s[15,'RetentionRate']), "%", sep=''), size=3.5, vjust=-.3, hjust=0, angle=-90)
	y1 = s[15,'RetentionRate'] - s[15,'GraduationRate'] + (s[15,'GraduationRate']) / 2
	p = p + geom_text(aes(x=16), y=y1, label=paste("15-month Completion Rate: ", round(s[15,'GraduationRate']), "%", sep=''), size=3.5, vjust=2, hjust=-.1, angle=-5)
	y2 = (s[15,'PersistenceRate2'] - s[15,'GraduationRate']) / 2
	p = p + geom_text(aes(x=16), y=y2, label=paste(round(s[15,'PersistenceRate2']-s[15,'GraduationRate']), "% enrolled and academcially active", sep=''), size=3.5, hjust=-.1, vjust=2, angle=-5)
	y3 = (s[15,'PersistenceRate2'] - s[15,'GraduationRate']) + (s[15,'PersistenceRate2'] - 2 * s[15,'GraduationRate']) / 2
	p = p + geom_text(aes(x=16), y=y3, label=paste(round(s[15,'RetentionRate']-s[15,'PersistenceRate2']), "% enrolled but academically inactive", sep=''), size=3.5, hjust=-.1, vjust=2, angle=-5)
	
	return(p)
}
