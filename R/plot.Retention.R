#'
#' @export
plotRetention <- function(students, grads, summary=NULL, retentionMonths=c(15), completionMonths=c(36, 48, 72, 96)) {
	if(is.null(summary)) {
		long2 = retention(students, grads)
	} else {
		long2 = summary
	}
	
	wt = cast(long2, Month ~ Cohort, value='Enrollments')
	gr = cast(long2, Month ~ Cohort, value='GraduationRate')
	rr = cast(long2, Month ~ Cohort, value='RetentionRate')
	pr = cast(long2, Month ~ Cohort, value='PersistenceRate')
	grt = data.frame(x=gr$Month, y=apply(wt[,2:ncol(wt)] * gr[,2:ncol(gr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	rrt = data.frame(x=rr$Month, y=apply(wt[,2:ncol(wt)] * rr[,2:ncol(rr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	prt = data.frame(x=pr$Month, y=apply(wt[,2:ncol(wt)] * pr[,2:ncol(pr)], 1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
	prt$y = prt$y + grt$y
	
	p = ggplot(long2, aes(x=Month, y=GraduationRate), stat='identity') + 
		geom_histogram(data=rrt, aes(x=x, y=y), colour='grey', alpha=.2, stat='identity') +
		geom_histogram(data=grt, aes(x=x, y=y), colour='grey', alpha=.2, stat='identity') +
		geom_path(aes(y=RetentionRate, colour=Cohort), alpha=.3) + 
		geom_path(aes(y=GraduationRate, colour=Cohort), linetype=2, alpha=.3) +
		#          geom_path(data=prt, aes(x=x, y=y), colour='black', stat='identity') +
		ylim(c(0,100)) +
		opts(legend.position='none') + 
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
