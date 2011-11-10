#'
#' @export
plotCohortRetention <- function(students, grads, labelPoints=FALSE, textsize=3, 
        reverse=TRUE, title="Cohort Based Retention", xlab='Monthly Cohort', 
        ylab1='Percentage', ylab2='Cohort Size', legend.position=c(.1,.7), 
		legend.justification='left',summary=NULL, 
		retentionMonths=c(15), completionMonths=c(36, 48, 72, 96), 
		plot.histogram=TRUE, ...) 
{
    if(is.null(summary)) {
        results = cohortRetention(students, grads, ...)
    } else {
        results = summary
    }
    
    t = melt(results[,1:6], id='Cohort')
    
    lastWHDate = max(students$CREATED_DATE, na.rm=TRUE)
    students$Month = as.factor(format(students$CREATED_DATE, format='%Y-%m'))
    
    df = results[,c('Cohort', 'GraduationRate', 'RetentionRate')]
    #xint <- t[(nrow(t)-15),]$Cohort #TODO: This won't plot unless the variable is in the global environment
    
    #Bottom part of the plot
    plot1 = ggplot(t, aes(x=Cohort, y=value, stat='identity')) + geom_bar(aes(fill=variable), alpha=.5) + 
        opts(axis.text.x=theme_text(angle=-90, size=unit(8,'points'), hjust=0)) + xlab(xlab) + ylab(ylab1) + 
        scale_fill_manual(paste('Status as of', lastWHDate), values=c('Withdrawn'='pink', 'Transferred'='lightgreen', 'Still Enrolled'='green', 'Graduated Other'='lightblue', 'Graduated'='blue')) + 
        opts(legend.key.width=unit(.2,"cm"), legend.text=theme_text(size=8), legend.title=theme_text(size=9,hjust=0), legend.position=legend.position, legend.justification=legend.justification, legend.background=theme_rect(colour='white', fill='white')) +
        geom_path(data=df, aes(x=Cohort, y=GraduationRate, fill=NULL, label=NULL, group=1), colour='blue', stat='identity') + 
        geom_path(data=df, aes(x=Cohort, y=RetentionRate, fill=NULL, label=NULL, group=1), colour='black', stat='identity')
    if(labelPoints) {
        plot1 = plot1 + geom_text(data=df, aes(x=Cohort, y=GraduationRate, label=round(GraduationRate, digits=0)), hjust=.5, vjust=-1, colour='blue', size=textsize) +
            geom_text(data=df, aes(x=Cohort, y=RetentionRate, label=round(RetentionRate, digits=0)), hjust=.5, vjust=2, colour='black', size=textsize)
    }
    
    if(reverse) { plot1 = plot1 + xlim(rev(levels(t$Cohort))) }
    
    if(!is.null(retentionMonths)) {
        rlabel <- data.frame(
            x=df[(nrow(df)-retentionMonths),'Cohort'],
            y=rep(100, length(retentionMonths)),
            label=paste(retentionMonths, '-month retention rate: ', format(df[(nrow(df)-retentionMonths),'RetentionRate'], digits=3), '%', sep='')
        )
        plot1 = plot1 + geom_vline(data=rlabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
            geom_text(data=rlabel, aes(x=x, y=100, label=label), group=1, size=3, vjust=-.3, hjust=0, angle=-90)
    }
    if(!is.null(completionMonths)) {
        clabel <- data.frame(
            x=df[(nrow(df)-completionMonths),'Cohort'],
            y=rep(100, length(completionMonths)),
            label=paste(completionMonths, '-month completion rate: ', format(df[(nrow(df)-completionMonths),'GraduationRate'], digits=3), '%', sep='')
        )
        plot1 = plot1 + geom_vline(data=clabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
           geom_text(data=clabel, aes(x=x, y=y, label=label), group=1, size=3, vjust=-.3, hjust=0, angle=-90)
    }
    
    #Top part of the graph (histogram of new enrollments)
    if(plot.histogram) {
      df2 = results[,c('Cohort', 'Enrollments')]
      plot2 = ggplot(df2, aes(x=Cohort, y=Enrollments), stat='identity') + 
          geom_bar(colour='grey', fill='grey', alpha=.7) + 
          opts(axis.ticks=theme_blank(), axis.text.x=theme_blank()) + 
          geom_text(aes(label=Enrollments), angle=-90, vjust=.5, hjust=-.1, size=textsize) + 
          ylab(ylab2) + xlab(NULL)
    
      if(reverse) { plot2 = plot2 + xlim(rev(levels(df2$Cohort))) }
    }
    
    theme_update(panel.background=theme_blank(), panel.grid.major=theme_blank(), panel.border=theme_blank())
    
    if(plot.histogram) {
      useGridExtra = FALSE
      if(useGridExtra) {
          #leg = ggplotGrob(plot1 + opts(keep="legend_box"))
          #legend = gTree(children=gList(leg), cl="legendGrob")
          #widthDetails.legendGrob <- function(x) unit(5, "cm")
          grid.arrange(plot2 + opts(plot.margin=unit(c(0,0,-1,0), "cm"), axis.text.y=theme_blank()), 
          plot1 + opts(plot.margin=unit(c(0,0,0,0), "cm"), axis.text.y=theme_blank()),
              #legend = legend,
             main = title, 
            heights = c(.2,.8), ncol=1, nrow=2
          )
      } else {
          empty <- plyr::empty
          Layout <- grid.layout(nrow = 2, ncol = 1)
          vplayout <- function(...) {
              grid.newpage()
              pushViewport(viewport(layout = Layout))
          }
          subplot <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y) }
          mplot <- function(p1, p2) {
              vplayout()
              print(p2 + opts(main=title), vp = subplot(1, 1))
              print(p1, vp = subplot(2, 1))
          }
      
          theme_update(panel.background=theme_blank(), panel.grid.major=theme_blank(), panel.border=theme_blank())
          mplot(plot1 + opts(plot.margin=unit(c(-10,.5,0,.3), "lines")), plot2 + opts(plot.margin=unit(c(0,0,9,0), "lines")))
      }
    } else {
      return(plot1)
    }
}


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
        scale_fill_manual('', c('Completed'='blue', 'Inactive'='yellow', 'Active'='green')) +
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
	
ggplot_size_legend <- function(p, legend_size=0.1, legend_text_size=10) { 
	Layout <- grid.layout(nrow = 1, ncol = 2, widths = unit(c(1-legend_size, legend_size), 
		c("null", "null")), heights = unit(1, "null"))
	vplayout <- function(...) { 
		grid.newpage() 
		pushViewport(viewport(layout = Layout)) 
	} 
	subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) 
	#create two plots, one with legend, one without 
	pl <- p + opts(legend.position = "none") 
	pn <- p + theme_grey(legend_text_size) + opts(keep = "legend_box") 
	#print the plot 
	vplayout() 
	print(pl, vp = subplot(1, 1)) 
	print(pn, vp = subplot(1, 2)) 
}

# require(compiler)
# plotCohortRetention <- cmpfun(plotCohortRetention)
# plotRetention <- cmpfun(plotRetention)
# plotPersistence <- cmpfun(plotPersistence)
# plotRetentionOverall <- cmpfun(plotRetentionOverall)
