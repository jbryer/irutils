require(ggplot2)
require(xtable)

min.cell.size = 10

#'
#' @export
cohortRetention <- function(students, grads, gradColumn='START_DATE', grouping=NULL) {
    firstWHDate = min(students$CREATED_DATE, na.rm=TRUE)
    lastWHDate = max(students$CREATED_DATE, na.rm=TRUE)
    last = students[which(students$CREATED_DATE == lastWHDate),]
    
    students$PERSIST_FLAG = NULL
    
    grads = grads[which(as.Date(grads[,'START_DATE']) < lastWHDate),]
    
    #This will leave only the first occurance of a student in a particular degree code
    students = students[!duplicated(students[,c('CONTACT_ID_SEQ', 'DEGREE_CODE')]),]
    #By finding duplicates starting at the end, the first occurance will have the Transferred value set to true
    students$Transferred = FALSE
    rows = duplicated(students$CONTACT_ID_SEQ, fromLast=TRUE)
    if(nrow(students[rows,]) > 0 ) { 
        students[rows,]$Transferred = TRUE
        #We can now remove duplciated students, the remaining data frame contains the students' first enrollment
        students = students[!duplicated(students$CONTACT_ID_SEQ, fromLast=FALSE),]
    }
    #Merge in graduates
    students = merge(students, grads[,c('CONTACT_ID_SEQ', 'START_DATE')], by=c('CONTACT_ID_SEQ'), all.x=TRUE)
    #If the START_DATE is not NA, then the student graduated. Note that this does not mean they graduated
    #from their initial DEGREE_CODE (see merge above matches only on CONTACT_ID_SEQ)
    students$Graduated = FALSE
    gr = !is.na(students$START_DATE) & !is.na(students$CREATED_DATE) & students$START_DATE >= students$ENROLL_DATE
    gr[is.na(gr)] = FALSE #TODO: How are the NAs?s
    if(nrow(students[gr,]) > 0) {
        students[gr,]$Graduated = TRUE
    }
    #See if the student is still enrolled as of the last cohort
    last$StillEnrolled = TRUE
    students = merge(students, last[,c('CONTACT_ID_SEQ', 'StillEnrolled', 'PERSIST_FLAG')], by=c('CONTACT_ID_SEQ'), all.x=TRUE)
    se = is.na(students$StillEnrolled)
    if(nrow(students[se,]) > 0) {
        students[se,]$StillEnrolled = FALSE
    }
    #Remove the baseline data
    students = students[!is.na(students$CREATED_DATE),]
    #Remove the first and last cohort month
    students = students[-which(students$CREATED_DATE == firstWHDate),]
    students = students[-which(students$CREATED_DATE == lastWHDate),]
    if(nrow(students) < min.cell.size) {
        return(NULL)
    }
    #Create the status variable that we will plot with
    students$Status = factor('Withdrawn', levels=c('Graduated', 'Graduated Other', 'Still Enrolled', 'Transferred', 'Withdrawn' ))
    if(length(which(students$StillEnrolled) == TRUE) > 0) {
        students[students$StillEnrolled,]$Status = 'Still Enrolled'
    }
    if(length(which(students$StillEnrolled == TRUE & students$Transferred == TRUE)) > 0) {
        students[students$StillEnrolled & students$Transferred,]$Status = 'Transferred'
    }
    if(length(which(students$Graduated == TRUE)) > 0) {
        students[students$Graduated,]$Status = 'Graduated'
    }
    if(length(which(students$Transferred == TRUE & students$Graduated == TRUE)) > 0) {
        students[students$Graduated & students$Transferred,]$Status = 'Graduated Other'
    }
    
    #Are those who are still enrolled persisting?
    students$Persisting = as.logical(NA)
    students[which(students$Status %in% c('Still Enrolled', 'Transferred') & students$PERSIST_FLAG == 'Y'),]$Persisting = TRUE
    students[which(students$Status %in% c('Still Enrolled', 'Transferred') & students$PERSIST_FLAG == 'N'),]$Persisting = FALSE
    
    students$Month = as.factor(format(students$CREATED_DATE, format='%Y-%m'))
    
	buildSummary <- function(s) {
		t = as.data.frame(table(s$Month, s$Status))
	 	if(!('Var1' %in% names(t) & 'Var2' %in% names(t))) {
	 		return(NULL)
	 	}
	  
	    t = cast(t, Var1 ~ Var2, value='Freq')
	    totals = apply(t[,2:ncol(t)], 1, sum)
	    t[,2:ncol(t)] = 100 * t[,2:ncol(t)] / totals
	    GraduationRate = apply(t[,2:3], 1, sum)
	    RetentionRate = apply(t[,2:5], 1, sum)
	    t = melt(t, id='Var1')
	    rownames(t) = 1:nrow(t)
	    
  		t2 = as.data.frame(table(s$Month, s$Persisting))
	 	if(!('Var1' %in% names(t2) & 'Var2' %in% names(t2))) {
	 		return(NULL)
	 	}
  		t2 = cast(t2, Var1 ~ Var2, value='Freq')
	 	if(!'TRUE' %in% names(t2)) {
	 		t2[,'TRUE'] = 0
	 	}
	 	if(!'FALSE' %in% names(t2)) {
	 		t2[,'FALSE'] = 0
	 	}
	    totals2 = apply(t2[,2:3], 1, sum)
	    t2[,2:3] = 100 * t2[,2:3] / totals2
	    PersistenceRate = t2[,'TRUE']
	    
	 	if('Var1' %in% names(t) & 'Var2' %in% names(t)) {
		    summary = cast(t, Var1 ~ Var2, value='value')
		    summary = cbind(summary, GraduationRate, RetentionRate, PersistenceRate, totals)
		    names(summary)[1] = 'Cohort'
		    names(summary)[ncol(summary)] = 'Enrollments'
	 
		 	return(summary)
	 	} else {
	 		return(NULL)
	 	}
	}
	
	results = data.frame()
	
	if(is.null(grouping)) {
		results = buildSummary(students)
	} else {
		groups = unique(students[,grouping])
  		for(g in groups) {
  			s = students[which(students[,grouping] == g),]
	 		if(nrow(s) > 20) {
		 		r = buildSummary(s)
	 			if(!is.null(r)) {
			 		r$Group = g
			 		results = rbind(results, r)
	 			}
	 		}
  		}
	}
	
    return(results)
}


#'
#' @export
retention <- function(students, grads, ...) {
    cohorts = unique(students[!is.na(students$CREATED_DATE),]$CREATED_DATE)
    
    results <- list()
    for(i in length(cohorts):3) {
        result = cohortRetention(students, grads, ...)
    	#result = cohortRetention(students, grads, grouping='DEGREE_CODE')
        if(!is.null(result)) {
            results[[as.character(cohorts[i])]] = result
        }
        remove = -which(students$CREATED_DATE == cohorts[i])
        if(length(remove) > 0) {
            students = students[remove,]
        }
    }
    
    if(length(results) == 0) {
        return(NULL)
    }
    
	monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
    mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
	
	cols = c('Cohort', 'GraduationRate', 'RetentionRate', 'PersistenceRate', 'Enrollments', 'Graduated','Graduated Other','Still Enrolled','Transferred')
	if('Group' %in% names(results[[1]])) {
		cols = c(cols, 'Group')
	}
	
	long2 = results[[1]][,cols]
	long2$Month = (mondf(as.Date(paste(long2$Cohort, '01', sep='-')), as.Date(paste(names(results)[1], '01', sep='-')) ))
	long2$Comparison = names(results)[1]
    for(i in 2:length(results)) {
    	if(nrow(results[[i]]) > 0 & ncol(results[[i]]) > 0) {
	        l = results[[i]][,cols]
			l$Month = (mondf(as.Date(paste(l$Cohort, '01', sep='-')), as.Date(paste(names(results)[i], '01', sep='-')) ))
			l$Comparison = names(results)[i]
	    	long2 = rbind(long2, l)
    	}
    }
    
    return(long2)
}



#'
#' @export
fySummary <- function(ret, rows, grouping=NULL, rateCol='GraduationRate', firstMonth=7) {
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

#'
#' @export
fySummaryLaTeX <- function(fysum, caption=NULL, label='fysum') {
	fysum[is.nan(fysum[,3]),3] = NA
	fyc = cast(fysum, FY ~ Group, value=names(fysum)[3])
	fyn = cast(fysum, FY ~ Group, value=names(fysum)[4])
	fysum = data.frame(Group=fyc[,1])
	for(i in 2:ncol(fyn)) {
		fyc[which(fyn[,i] < 5),i] = NA
	 	fysum = cbind(fysum, fyc[,i], fyn[,i])
	 	names(fysum)[ncol(fysum)-1] = names(fyn)[i]
	 	names(fysum)[ncol(fysum)] = paste(names(fyn)[i], 'n', sep='.')
	}
	for(i in seq(3, ncol(fysum), by=2)) {
		fysum[!is.na(fysum[,i]),i] = paste('(', fysum[!is.na(fysum[,i]),i], ')', sep='')
	}
	fysum[,1] = as.integer(fysum[,1])
	
	addtorow <- list()
	addtorow$pos <- list()
	addtorow$pos[[1]] <- c(-1)
	addtorow$command <- c("\\hline ")
	for(i in seq(2, ncol(fysum), by=2)) {
		addtorow$command[1] = paste(addtorow$command[1], ' & \\multicolumn{2}{c}{', names(fysum[i]), '}', sep='')
	}
	addtorow$command[1] = paste(addtorow$command[1], ' \\\\ \\hline', sep='')
	align = c('l', 'l', rep(c('r@{}', '>{ \\tiny}l'), (ncol(fysum) - 1) / 2) )
	x = xtable(fysum, caption=caption, label=label, align=align)
	print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller')
}


#'
#' @export
cohortSummary <- function(dr, grouping='Group') {
    month15 = dr[which(dr$Month == 15),]
    s = month15[!duplicated(month15[,grouping]), c(grouping, 'RetentionRate', 'Enrollments')]
    month36 = dr[which(dr$Month == 36),]
    s = merge(s, month36[!duplicated(month36[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE)
    month48 = dr[which(dr$Month == 48),]
    s = merge(s, month48[!duplicated(month48[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE)
    month72 = dr[which(dr$Month == 72),]
    s = merge(s, month72[!duplicated(month72[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE)
    month96 = dr[which(dr$Month == 96),]
    s = merge(s, month96[!duplicated(month96[,grouping]), c(grouping, 'GraduationRate', 'Enrollments')], by=grouping, all.x=TRUE)
    names(s) = c('Category', 'Retention', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n')
    return(s)
}

#'
#' @export
cohortSummaryLaTeX <- function(ret, category='Category', category.align="l", 
        caption='Retention Summary', label='retentionSummary', 
        sparkline.retention.rect=NULL, sparkline.completion.rect=NULL, 
        sparkline.width=10, sparkline.height=NULL, completion.month=48) {

    sparklineTex <- function(df, width=sparkline.width, height=sparkline.height, 
            rect=NULL, sparkdot.color='red', n=24) {
        if(nrow(df) > n) {
        	df = df[(nrow(df)-n):(nrow(df)),]
        	df$x = 0:(nrow(df)-1)
        }
        df$x = df$x / max(df$x)
        str = paste(
            ifelse(is.null(height), '', paste('\\renewcommand{\\sparklineheight}{', height, '}', sep='')),
            paste('\\begin{sparkline}{', width, '}', sep=''),
            ifelse(is.null(rect), '', paste('\\sparkrectangle ', rect[1], ' ', rect[2], sep='')),
            paste('\\sparkdot', df[nrow(df),'x'], round(df[nrow(df),'y'], digits=2), sparkdot.color, sep=' '),
            '\\spark ', paste(round(df$x, digits=2), round(df$y, digits=2), collapse=' '), ' /',
            '\\end{sparkline}',
        sep=' ')
        return(str)
    }

    retsum = cohortSummary(ret)
    for(i in c(3,5,7,9, 11)) { 
        retsum[which(retsum[,i] < min.cell.size), (i-1)] = NA
        retsum[!is.na(retsum[,i]),i] = paste(' (', retsum[!is.na(retsum[,i]),i], ')', sep='')
    }
    
    retsum$RetentionSparkline = as.character(NA)
    retsum$CompletionSparkline = as.character(NA)
    if(!is.null(sparkline.retention.rect)) {
        #sparkline.retention.rect = (sparkline.retention.rect - .50) / 1/(.50)
    }
    if(!is.null(sparkline.completion.rect)) {
        #sparkline.completion.rect = (sparkline.completion.rect - .50) / 1/(.5)
    }
    for(i in retsum[,category]) {
        tmp = ret[which(ret[,'Group'] == i & ret[,'Month'] == 15),]
        coords = data.frame(x=as.integer(tmp$Cohort), y=(tmp$RetentionRate/100))
        coords$x = abs(coords$x - max(coords$x) - 1)
        #coords$y = (coords$y - .5) * 1/.5
		coords = coords[!is.nan(coords$y),]
		coords = coords[!is.na(coords$y),]
        if(nrow(coords) >= min.cell.size & mean(tmp$Enrollments) >= min.cell.size) {
            retsum[which(retsum[,category] == i), 'RetentionSparkline'] = sparklineTex(coords, rect=sparkline.retention.rect)
        }

        tmp = ret[which(ret[,'Group'] == i & ret[,'Month'] == completion.month),]
        coords = data.frame(x=as.integer(tmp$Cohort), y=(tmp$GraduationRate/100))
        coords$x = abs(coords$x - max(coords$x) - 1)
        #coords$y = (coords$y - .5) * 1/.5
        if(nrow(coords) >= min.cell.size & mean(tmp$Enrollments) >= min.cell.size) {
            retsum[which(retsum[,category] == i), 'CompletionSparkline'] = sparklineTex(coords, rect=sparkline.completion.rect)
        }
    }

    names(retsum) = c(category, 'Rates', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n', 'RetentionSparkline', 'CompletionSparkline')
    retsum = retsum[,c(1:3, (ncol(retsum)-1), 4:(ncol(retsum)-2), ncol(retsum))]
    addtorow <- list()
    addtorow$pos <- list()
    addtorow$pos[[1]] <- c(-1)
    addtorow$command <- c(paste('\\hline & & & & \\multicolumn{9}{c}{Completion Rate} \\\\ \\cline{5-13} ',
        category, ' & \\multicolumn{3}{c}{Retention Rate} & \\multicolumn{2}{c}{36-Months} & \\multicolumn{2}{c}{48-Months} & \\multicolumn{2}{c}{72-Months} & \\multicolumn{2}{c}{96-Months} & Past Two Years\\\\ \\hline ', 
        sep=''))
    x = xtable(retsum, caption=caption, label=label, digits=2, align=c('l', category.align, 'r@{}', '>{ \\tiny}l', 'c', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'c'))
    p = capture.output(print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller'))
    p = p[3:length(p)]
    p = gsub('$\\backslash$', '\\', p, fixed=TRUE)
    p = gsub('\\{', '{', p, fixed=TRUE)
    p = gsub('\\}', '}', p, fixed=TRUE)
    return(p)
}

#'
#' @export
retentionSummaryLaTeX <- function(ret, category='Category', category.align="l", caption='Retention Summary', label='retentionSummary') {
    retsum = retentionSummary(ret)
    for(i in c(3,5,7,9, 11)) { 
        retsum[which(retsum[,i] < min.cell.size), (i-1)] = NA
        retsum[!is.na(retsum[,i]),i] = paste(' (', retsum[!is.na(retsum[,i]),i], ')', sep='')
    }

    names(retsum) = c(category, 'Rates', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n')
    addtorow <- list()
    addtorow$pos <- list()
    addtorow$pos[[1]] <- c(-1)
    addtorow$command <- c(paste('\\hline & & & \\multicolumn{8}{c}{Completion Rate} \\\\ \\cline{4-11} ',
        category, ' & \\multicolumn{2}{c}{Retention Rate} & \\multicolumn{2}{c}{36-Months} & \\multicolumn{2}{c}{48-Months} & \\multicolumn{2}{c}{72-Months} & \\multicolumn{2}{c}{96-Months} \\\\ \\hline ', 
        sep=''))
    x = xtable(retsum, caption=caption, label=label, digits=2, align=c('l', category.align, 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l'))
    print(x, table.placement='h!',include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller')
}

#'
#' @export
retentionSummary <- function(dr, grouping='Group') {
    month15 = dr[which(dr$Month == 15),]
	month15[is.na(month15$RetentionRate) & month15$Enrollments == 0,'RetentionRate'] = 0
	month15[is.na(month15$GraduationRate) & month15$Enrollments == 0,'GraduationRate'] = 0
	month15[is.na(month15$PersistenceRate) & month15$Enrollments == 0,'PersistenceRate'] = 0

	ret15 = aggregate(month15[,c('RetentionRate')], by=list(month15[,grouping]), FUN=mean)
    ret15.n = aggregate(month15[,c('Enrollments')], by=list(month15[,grouping]), FUN=sum)
    
	month36 = dr[which(dr$Month == 36),]
	month36[is.na(month36$RetentionRate) & month36$Enrollments == 0,'RetentionRate'] = 0
	month36[is.na(month36$GraduationRate) & month36$Enrollments == 0,'GraduationRate'] = 0
	month36[is.na(month36$PersistenceRate) & month36$Enrollments == 0,'PersistenceRate'] = 0
    com36 = aggregate(month36[,c('GraduationRate')], by=list(month36[,grouping]), FUN=mean)
    com36.n = aggregate(month36[,c('Enrollments')], by=list(month36[,grouping]), FUN=sum)
    
	month48 = dr[which(dr$Month == 48),]
	month48[is.na(month48$RetentionRate) & month48$Enrollments == 0,'RetentionRate'] = 0
	month48[is.na(month48$GraduationRate) & month48$Enrollments == 0,'GraduationRate'] = 0
	month48[is.na(month48$PersistenceRate) & month48$Enrollments == 0,'PersistenceRate'] = 0
    com48 = aggregate(month48[,c('GraduationRate')], by=list(month48[,grouping]), FUN=mean)
    com48.n = aggregate(month48[,c('Enrollments')], by=list(month48[,grouping]), FUN=sum)
    
	month72 = dr[which(dr$Month == 72),]
	month72[is.na(month72$RetentionRate) & month72$Enrollments == 0,'RetentionRate'] = 0
	month72[is.na(month72$GraduationRate) & month72$Enrollments == 0,'GraduationRate'] = 0
	month72[is.na(month72$PersistenceRate) & month72$Enrollments == 0,'PersistenceRate'] = 0
    com72 = aggregate(month72[,c('GraduationRate')], by=list(month72[,grouping]), FUN=mean)
    com72.n = aggregate(month72[,c('Enrollments')], by=list(month72[,grouping]), FUN=sum)
    
	month96 = dr[which(dr$Month == 96),]
	month96[is.na(month96$RetentionRate) & month96$Enrollments == 0,'RetentionRate'] = 0
	month96[is.na(month96$GraduationRate) & month96$Enrollments == 0,'GraduationRate'] = 0
	month96[is.na(month96$PersistenceRate) & month96$Enrollments == 0,'PersistenceRate'] = 0
    com96 = aggregate(month96[,c('GraduationRate')], by=list(month96[,grouping]), FUN=mean)
    com96.n = aggregate(month96[,c('Enrollments')], by=list(month96[,grouping]), FUN=sum)
    
	tab = merge(ret15, ret15.n, by='Group.1', all.x=TRUE)
    tab = merge(tab, com36, by='Group.1', all.x=TRUE)
    tab = merge(tab, com36.n, by='Group.1', all.x=TRUE)
    tab = merge(tab, com48, by='Group.1', all.x=TRUE)
    tab = merge(tab, com48.n, by='Group.1', all.x=TRUE)
    tab = merge(tab, com72, by='Group.1', all.x=TRUE)
    tab = merge(tab, com72.n, by='Group.1', all.x=TRUE)
    tab = merge(tab, com96, by='Group.1', all.x=TRUE)
    tab = merge(tab, com96.n, by='Group.1', all.x=TRUE)
    
	names(tab) = c('Category', 'Retention', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n')
    return(tab)
}

# require(compiler)
# cohortRetention <- cmpfun(cohortRetention)
# retention <- cmpfun(retention)
# retentionSummary <- cmpfun(retentionSummary)
# retentionSummaryLaTeX <- cmpfun(retentionSummaryLaTeX)
