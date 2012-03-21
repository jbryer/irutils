#' Prints LaTeX of retention summary.
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


#' Prints LaTeX of cohort summary.
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

#' Prints LaTeX of a quarterly aggregate.
#' 
#' @export
quarterSummaryLaTeX <- function(quartersum, caption=NULL, label='quartersum', numQuarters=8) {
	#quartersum[is.nan(quartersum[,3]),3] = NA
	fyc = cast(quartersum, Group ~ FY, value=names(quartersum)[3])
	fyn = cast(quartersum, Group ~ FY, value=names(quartersum)[4])
	quartersum = data.frame(Group=fyc[,1])
	for(i in 2:ncol(fyn)) {
		fyc[which(fyn[,i] < 5),i] = NA
		quartersum = cbind(quartersum, fyc[,i], fyn[,i])
		names(quartersum)[ncol(quartersum)-1] = names(fyn)[i]
		names(quartersum)[ncol(quartersum)] = paste(names(fyn)[i], 'n', sep='.')
	}
	for(i in seq(3, ncol(quartersum), by=2)) {
		quartersum[!is.na(quartersum[,i]),i] = paste('(', quartersum[!is.na(quartersum[,i]),i], ')', sep='')
	}
	#quartersum[,1] = as.integer(quartersum[,1])
	quartersum = quartersum[,c(1, ((ncol(quartersum))-(2*numQuarters)+1):ncol(quartersum))]
	
	addtorow <- list()
	addtorow$pos <- list()
	addtorow$pos[[1]] <- c(-1)
	addtorow$command <- c("\\hline ")
	for(i in seq(2, ncol(quartersum), by=2)) {
		addtorow$command[1] = paste(addtorow$command[1], ' & \\multicolumn{2}{c}{', names(quartersum[i]), '}', sep='')
	}
	addtorow$command[1] = paste(addtorow$command[1], ' \\\\ \\hline', sep='')
	align = c('l', 'l', rep(c('r@{}', '>{ \\tiny}l'), (ncol(quartersum) - 1) / 2) )
	x = xtable(quartersum, caption=caption, label=label, align=align)
	print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller')
}


#' Prints LaTeX of fiscal year summary.
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
