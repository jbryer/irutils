#' Constructor function to create a likert class.
#'
#' TODO: Need more documentation 
#'
#' @export
#' @param items data frame containing the likert based items. The variables
#'        in the data frame should be factors.
#' @param grouping (optional) should the results be summarized by the given
#'        grouping variable.
#' @param nlevels number of possible levels. Only necessary if there are missing levels.
likert <- function(items, grouping=NULL, nlevels=length(levels(items[,1]))) {
	lowrange = 1 : ceiling(nlevels / 2 - nlevels %% 2)
	highrange = ceiling(nlevels / 2 + 1 ) : nlevels
	
	results <- data.frame()
	if(!is.null(grouping)) {
		results = data.frame(
			Group = rep(unique(grouping), each=nlevels),
			Response = rep(1:nlevels, length(unique(grouping)))
			)
		for(i in 1:ncol(items)) {
			t = as.data.frame(table(grouping, items[,i]))
			t = cast(t, Var2 ~ grouping, value='Freq', add.missing=TRUE)
			t = apply(t, 2, FUN=function(x) { x / sum(x) * 100 } )
			t = melt(t)
			results = cbind(results, t[,3])
		}
		
		names(results)[3:ncol(results)] = names(items)
		
		results2 = data.frame(Group=rep(unique(results$Group), each=ncol(items)),
							  Item=rep(names(items), length(unique(results$Group))), 
							  low=rep(NA, ncol(items) * length(unique(results$Group))), 
							  high=rep(NA, ncol(items) * length(unique(results$Group))) )
		for(g in unique(results$Group)) {
			results2[which(results2$Group == g),]$low = apply(results[results$Response %in% lowrange & results$Group == g,3:ncol(results)], 2, sum)
			results2[which(results2$Group == g),]$high = apply(results[results$Response %in% highrange & results$Group == g,3:ncol(results)], 2, sum)
		}
		
		results = melt(results, id=c('Group', 'Response'))
		results$Response = factor(results$Response, levels=1:nlevels, labels=levels(items[,i]))
		names(results) = c('Group', 'Response', 'Item', 'Value')
	} else {
		results = data.frame(Response=1:nlevels)
		for(i in 1:ncol(items)) {
			t = table(items[,i])
			t = (t / sum(t) * 100)
			results = cbind(results, as.data.frame(t)[2])
			names(results)[ncol(results)] = names(items)[i]
		}		
		results = as.data.frame(t(results))
		results = results[2:nrow(results),]
		results$item = row.names(results)
		row.names(results) = 1:nrow(results)
		results2 = data.frame(item=results$item, 
							  low=apply(results[,lowrange], 1, sum),
							  high=apply(results[,highrange], 1, sum)
							  )
		results = melt(results, id.vars='item')
	}
	
	r = list(results=results, items=items, grouping=grouping, nlevels=nlevels, results2=results2)
	class(r) = 'likert'
	return(r)
}

#' Prints summary table.
#' @export
#' @S3method plot likert
print.likert <- function(likert, ...) {
	cast(likert$results, item ~ variable)
}

#' Plots a set of likert items
#'
#' @param likert the likert items to plot
#' @param low.color color corresponding to the lowest value likert items
#' @param high.color color corresponding to the highest value likert items
#' @param neutral.color color for middle values. Only used when there are an odd
#'        number of levels.
#' @param text.size size or text labels
#' @param type whether to plot a bar or heat map graphic
#' @export
#' @S3method plot likert
plot.likert <- function(likert, low.color='blue', high.color='red', 
			neutral.color='white', text.size=2, type=c('bar','heat'), ...)
{
	if(type[1] == 'bar') {
		plot.likert.bar(likert, low.color=low.color, high.color=high.color,
						neutral.color=neutral.color, text.size=text.size, ...)
	} else {
		plot.likert.heat(likert, low.color=low.color, high.color=high.color,
						text.size=text.size, ...)
	}
}

plot.likert.bar <- function(likert, low.color='blue', high.color='red', 
							neutral.color='white', text.size=2)
{
	lowrange = 1 : ceiling(likert$nlevels / 2 - likert$nlevels %% 2)
	highrange = ceiling(likert$nlevels / 2 + 1 ) : likert$nlevels
	ramp = colorRamp(c(low.color, neutral.color))
	ramp = rgb( ramp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
	bamp = colorRamp(c(neutral.color, high.color))
	bamp = rgb( bamp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
	cols = NULL
	if(likert$nlevels %% 2 != 0) {
		cols = c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
	} else {
		cols = c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])
	}
	
	if(!is.null(likert$grouping)) {
		p = ggplot(likert$results, aes(y=Value, x=Group, group=Item)) + 
			scale_fill_manual('Response', values=cols, 
					breaks=unique(likert$results$Response),
					labels=levels(likert$items[,i])) + 
			geom_bar(stat='identity', aes(fill=Response)) + 
			geom_text(data=likert$results2, aes(x=Group, y=0, label=paste(round(low), '%', sep=''), group=Item), size=text.size, hjust=1) +
			geom_text(data=likert$results2, aes(x=Group, y=100, label=paste(round(high), '%', sep=''), group=Item), size=text.size, hjust=-.2) +
			coord_flip() +
			ylab('Percentage') + xlab('') + ylim(c(-5,105)) +
			opts(axis.ticks=theme_blank()) +
			facet_wrap(~ Item, ncol=1)
	} else {
		p = ggplot(likert$results, aes(y=value, x=item, group=item)) + 
			scale_fill_manual('Response', values=cols, 
					breaks=unique(likert$results$variable), 
					labels=levels(likert$items[,i])) + 
			geom_bar(stat='identity', aes(fill=variable)) + 
			geom_text(data=likert$results2, aes(x=item, y=0, label=paste(round(low), '%', sep='')), size=text.size, hjust=1) +
			geom_text(data=likert$results2, aes(x=item, y=100, label=paste(round(high), '%', sep='')), size=text.size, hjust=-.2) +
			coord_flip() +
			ylab('Percentage') + xlab('') + ylim(c(-5,105)) +
			opts(axis.ticks=theme_blank())
	} 
	return(p)
}

#' Function creates and returns a tabular heatmap ideal for summarizing likert
#' survey items.
#' @param items the items to summarize.
#' @export
plot.likert.heat <- function(likert, low.color='white', high.color='blue', text.size=2) {	
	p = ggplot(likert$results, aes(x=item, y=variable, fill=value, label=paste(value, '%', sep=''))) + 
		geom_text(aes(y='Mean (SD)', x=item, label=paste(format(mean, digits=3, drop0trailing=FALSE), ' (', format(sd, digits=2, drop0trailing=FALSE), ')', sep='')), size=text.size) +
		geom_tile() + geom_text(size=text.size) + coord_flip() + 
		scale_fill_gradient("Percent", low=low.color, high=high.color, limits=c(0,100)) + 
		xlab('') + ylab('') + 
		opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), axis.ticks=theme_blank())
	return(p)
}
