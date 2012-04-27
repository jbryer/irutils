#' Returns cohort summary.
#' 
#' @export
cohortSummary <- function(dr, grouping='Group') {
	month15 = dr[which(dr$Month == 15),]
	s = month15[!duplicated(month15[,grouping]), c(grouping, 'RetentionRate', 'Enrollments')]
	month36 = dr[which(dr$Month == 36),]
	s = merge(s, month36[!duplicated(month36[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE, suffixes=c('',''))
	month48 = dr[which(dr$Month == 48),]
	s = merge(s, month48[!duplicated(month48[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE, suffixes=c('',''))
	month72 = dr[which(dr$Month == 72),]
	s = merge(s, month72[!duplicated(month72[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE, suffixes=c('',''))
	month96 = dr[which(dr$Month == 96),]
	s = merge(s, month96[!duplicated(month96[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE, suffixes=c('',''))
	names(s) = c('Category', 'Retention', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n')
	return(s)
}

#' Aggregates to fiscal years.
#' 
#' @export
fySummary <- function(ret, rows, grouping=NULL, rateCol='GraduationRate', firstMonth=7) {
	for(i in c('GraduationRate','RetentionRate','PersistenceRate')) {
		r = which(is.nan(ret[,i]) | is.na(ret[,i]))
		if(length(rows) > 0) {
			ret[r,i] = 0
		}
	}
	
	d = unlist(strsplit(as.character(ret$Cohort), '-'))
	year  = as.numeric(d[seq(1,length(d),by=2)])
	month = as.numeric(d[seq(2,length(d),by=2)])
	fy = rep(NA, length(year))
	fy[month < firstMonth] = year[month < firstMonth]
	fy[month >= firstMonth] = year[month >= firstMonth] + 1
	ret$fy = fy
	
	t = ret[rows,]
	t$weighted = t[,rateCol] * t$Enrollments
	bylist = list(t$fy)
	if(!is.null(grouping)) {
		bylist[[2]] = t[,grouping]
	}
	t1 = aggregate(t$weighted, by=bylist, sum)
	t2 = aggregate(t$Enrollments, by=bylist, sum)
	tab = cbind(t1[,1:(ncol(t1)-1)], t1$x / t2$x, t2$x)
	
	#Check only the most recent aggregated cohort. Previos years that may not
	#have enough cohorts will show up once at least one month into the next year
	t3 = aggregate(t$Cohort, by=bylist, length) #This data frame will check that we have at least 3 months in the cohort
	if(ncol(t3) == 3) {
		for(g in unique(t3[,2])) {
			f = max(t3[which(t3[,2] == g),1])
			r = which(t3[,1] == f & t3[,2] == g & t3[,'x'] != 12)
			if(length(r) > 0) {
				tab = tab[-r,]
				t3 = t3[-r,]
			}			
		}
	} else if(ncol(t3) == 2) {
		f = max(t3[,1])
		r = which(t3[,1] == f & t3[,'x'] != 12)
		if(length(r) > 0) {
			tab = tab[-r,]
			t3 = t3[-r,]
		}
	} else {
		stop('Retention only supports up to one grouping variable.')
	}
	
	names(tab)[1] = 'FY'
	names(tab)[(ncol(tab)-1):ncol(tab)] = c(rateCol, 'n')
	if(!is.null(grouping)) {
		names(tab)[2] = grouping
	}
	#tab = cast(tab, FY ~ Level, value='CompletionRate')
	#tab = cbind(tab, cast(t2, Group.1 ~ Group.2, value='x')[,2:4])
	#names(tab)[5:7] = paste(names(tab)[5:7], 'n', sep='.')
	tab = as.data.frame(tab)
	return(tab)
}

#' Aggregates to quarters (i.e. three-months).
#' 
#' @export
quarterlySummary <- function(ret, rows, grouping=NULL, rateCol='GraduationRate', firstMonth=7) {
	for(i in c('GraduationRate','RetentionRate','PersistenceRate')) {
		r = which(is.nan(ret[,i]) | is.na(ret[,i]))
		if(length(rows) > 0) {
			ret[r,i] = 0
		}
	}

	d = unlist(strsplit(as.character(ret$Cohort), '-'))
	year  = as.numeric(d[seq(1,length(d),by=2)])
	month = as.numeric(d[seq(2,length(d),by=2)])
	fy = rep(NA, length(year))
	fy[month < firstMonth] = year[month < firstMonth]
	fy[month >= firstMonth] = year[month >= firstMonth] + 1
	quarter = rep(NA, length(year))
	quarter[month %in% 7:9] = 'Q1'
	quarter[month %in% 10:12] = 'Q2'
	quarter[month %in% 1:3] = 'Q3'
	quarter[month %in% 4:6] = 'Q4'
	ret$fy = fy
	ret$quarter = quarter
	ret$fyq = paste('FY', ret$fy, '-', ret$quarter, sep='')
	
	t = ret[rows,]
	t$weighted = t[,rateCol] * t$Enrollments
	bylist = list(t$fyq)
	if(!is.null(grouping)) {
		bylist[[2]] = t[,grouping]
	}
	t1 = aggregate(t$weighted, by=bylist, sum)
	t2 = aggregate(t$Enrollments, by=bylist, sum)
	tab = cbind(t1[,1:(ncol(t1)-1)], t1$x / t2$x, t2$x)
	
	#Check only the most recent aggregated cohort. Previos years that may not
	#have enough cohorts will show up once at least one month into the next year
	t3 = aggregate(t$Cohort, by=bylist, length) #This data frame will check that we have at least 3 months in the cohort
	if(ncol(t3) == 3) {
		for(g in unique(t3[,2])) {
			f = max(t3[which(t3[,2] == g),1])
			r = which(t3[,1] == f & t3[,2] == g & t3[,'x'] != 3)
			if(length(r) > 0) {
				tab = tab[-r,]
				t3 = t3[-r,]
			}			
		}
	} else if(ncol(t3) == 2) {
		f = max(t3[,1])
		r = which(t3[,1] == f & t3[,'x'] != 3)
		if(length(r) > 0) {
			tab = tab[-r,]
			t3 = t3[-r,]
		}
	} else {
		stop('Retention only supports up to one grouping variable.')
	}
	
	names(tab)[1] = 'FY'
	names(tab)[(ncol(tab)-1):ncol(tab)] = c(rateCol, 'n')
	if(!is.null(grouping)) {
		names(tab)[2] = grouping
	}
	#tab = cast(tab, FY ~ Level, value='CompletionRate')
	#tab = cbind(tab, cast(t2, Group.1 ~ Group.2, value='x')[,2:4])
	#names(tab)[5:7] = paste(names(tab)[5:7], 'n', sep='.')
	tab = as.data.frame(tab)
	return(tab)
}



#' Returns retention summary.
#' 
#' @export
retentionSummary <- function(dr, grouping='Group') {
	month15 = dr[which(dr$Month == 15),]
	month15[is.na(month15$RetentionRate) & month15$Enrollments == 0,'RetentionRate'] = 0
	month15[is.na(month15$GraduationRate) & month15$Enrollments == 0,'GraduationRate'] = 0
	month15[is.na(month15$PersistenceRate) & month15$Enrollments == 0,'PersistenceRate'] = 0
	
	ret15 = aggregate(month15[,c('RetentionRate')], by=list(month15[,grouping]), FUN=mean)
	ret15.n = aggregate(month15[,c('Enrollments')], by=list(month15[,grouping]), FUN=sum)
	names(ret15) = c('Category', 'Retention')
	names(ret15.n) = c('Category', 'n')
	
	month36 = dr[which(dr$Month == 36),]
	month36[is.na(month36$RetentionRate) & month36$Enrollments == 0,'RetentionRate'] = 0
	month36[is.na(month36$GraduationRate) & month36$Enrollments == 0,'GraduationRate'] = 0
	month36[is.na(month36$PersistenceRate) & month36$Enrollments == 0,'PersistenceRate'] = 0
	com36 = aggregate(month36[,c('GraduationRate')], by=list(month36[,grouping]), FUN=mean)
	com36.n = aggregate(month36[,c('Enrollments')], by=list(month36[,grouping]), FUN=sum)
	names(com36) = c('Category', '36-Months')
	names(com36.n) = c('Category', 'n')
	
	month48 = dr[which(dr$Month == 48),]
	month48[is.na(month48$RetentionRate) & month48$Enrollments == 0,'RetentionRate'] = 0
	month48[is.na(month48$GraduationRate) & month48$Enrollments == 0,'GraduationRate'] = 0
	month48[is.na(month48$PersistenceRate) & month48$Enrollments == 0,'PersistenceRate'] = 0
	com48 = aggregate(month48[,c('GraduationRate')], by=list(month48[,grouping]), FUN=mean)
	com48.n = aggregate(month48[,c('Enrollments')], by=list(month48[,grouping]), FUN=sum)
	names(com48) = c('Category', '48-Months')
	names(com48.n) = c('Category', 'n')
	
	month72 = dr[which(dr$Month == 72),]
	month72[is.na(month72$RetentionRate) & month72$Enrollments == 0,'RetentionRate'] = 0
	month72[is.na(month72$GraduationRate) & month72$Enrollments == 0,'GraduationRate'] = 0
	month72[is.na(month72$PersistenceRate) & month72$Enrollments == 0,'PersistenceRate'] = 0
	com72 = aggregate(month72[,c('GraduationRate')], by=list(month72[,grouping]), FUN=mean)
	com72.n = aggregate(month72[,c('Enrollments')], by=list(month72[,grouping]), FUN=sum)
	names(com72) = c('Category', '72-Months')
	names(com72.n) = c('Category', 'n')
	
	month96 = dr[which(dr$Month == 96),]
	month96[is.na(month96$RetentionRate) & month96$Enrollments == 0,'RetentionRate'] = 0
	month96[is.na(month96$GraduationRate) & month96$Enrollments == 0,'GraduationRate'] = 0
	month96[is.na(month96$PersistenceRate) & month96$Enrollments == 0,'PersistenceRate'] = 0
	com96 = aggregate(month96[,c('GraduationRate')], by=list(month96[,grouping]), FUN=mean)
	com96.n = aggregate(month96[,c('Enrollments')], by=list(month96[,grouping]), FUN=sum)
	names(com96) = c('Category', '96-Months')
	names(com96.n) = c('Category', 'n')
	
	tab = merge(ret15, ret15.n, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com36, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com36.n, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com48, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com48.n, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com72, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com72.n, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com96, by='Category', all.x=TRUE, suffixes=c('',''))
	tab = merge(tab, com96.n, by='Category', all.x=TRUE, suffixes=c('',''))
	
	return(tab)
}
