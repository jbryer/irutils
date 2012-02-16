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
