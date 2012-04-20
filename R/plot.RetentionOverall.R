#'
#' @export
plotRetentionOverall <- function(students, grads, summary=NULL, retentionMonths=c(15), 
								 completionMonths=c(36, 48, 72, 96), legend.position='none'
								 ) {
	if(is.null(summary)) {
		long2 = retention(students, grads)
	} else {
		long2 = summary
	}
	
	wt = cast(long2, Month ~ Cohort, value='Enrollments')
	gr = cast(long2, Month ~ Cohort, value='GraduationRate')
	rr = cast(long2, Month ~ Cohort, value='RetentionRate')
	pr = cast(long2, Month ~ Cohort, value='PersistenceRate')
	totals = apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE)
	grt = data.frame(x=gr$Month, y=apply(wt[,2:ncol(wt)] * gr[,2:ncol(gr)], 1, sum, na.rm=TRUE) / totals)
	rrt = data.frame(x=rr$Month, y=apply(wt[,2:ncol(wt)] * rr[,2:ncol(rr)], 1, sum, na.rm=TRUE) / totals)
	prt = data.frame(x=pr$Month, y=apply(wt[,2:ncol(wt)] * pr[,2:ncol(pr)], 1, sum, na.rm=TRUE) / totals)
	
	s = cbind(grt, rrt[,2], prt[,2])
	names(s) = c('x', 'GraduationRate', 'RetentionRate', 'PersistenceRate')
	s$PersistenceRate2 = (.01 * s$PersistenceRate * (s$RetentionRate-s$GraduationRate) + s$GraduationRate)
	
	p = ggplot(s, aes(x=x, y=GraduationRate), stat='identity') + 
		geom_ribbon(ymin=0, aes(ymax=(GraduationRate), fill='Completed'), alpha=.5) +
		geom_ribbon(aes(ymin=GraduationRate, ymax=PersistenceRate2, fill='Active'), alpha=.5) +
		geom_ribbon(aes(ymin=PersistenceRate2, ymax=RetentionRate, fill='Inactive'), alpha=.5) +
		geom_path(aes(y=RetentionRate), colour='black', stat='identity') +
		geom_path(aes(y=GraduationRate), colour='black', stat='identity', linetype=4) +
		geom_path(aes(y=PersistenceRate2), colour='black', stat='identity', linetype=2) +
		ylim(c(0,100)) +
		scale_fill_manual('', values=c('Completed'='blue', 'Inactive'='yellow', 'Active'='green')) +
		opts(legend.position=legend.position) + 
		xlab('Months Since Enrollment') + ylab('Percentage') 
	
	if(!is.null(retentionMonths)) {
		rlabel <- data.frame(
			x=retentionMonths,
			y=rep(100, length(retentionMonths)),
			label=paste(retentionMonths, '-month retention rate: ', format(rrt[retentionMonths,2], digits=3), '%', sep='')
			)
		p = p + geom_vline(data=rlabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
			geom_text(data=rlabel, aes(x=x, y=y, label=label), group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	if(!is.null(completionMonths)) {
		clabel <- data.frame(
			x=completionMonths,
			y=rep(100, length(completionMonths)),
			label=paste(completionMonths, '-month completion rate: ', format(grt[completionMonths,2], digits=3), '%', sep='')
			)
		p = p + geom_vline(data=clabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
			geom_text(data=clabel, aes(x=x, y=y, label=label), group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	
	return(p)
}
