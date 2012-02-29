#' Constructor function to create a likert class.
#'
#' This function will provide various summary statistics about a set of likert
#' items. The resulting object will have the following items:
#' \itemize{
#'    \item results this data frame will contain a column 'Item', 'Group' (if a 
#'          grouping variable was specified, and a column for each level of the
#'          items (e.g. agree, disagree, etc.). The value within each cell corresponds
#'          to the percentage of responses for that level and group.
#'    \item items a copy of the original items data frame.
#'    \item grouping a copy of the original grouping vector.
#'    \item nlevels the number of levels used in the calculations.
#'    \item summary this data frame provides additional summary information. It
#'          will contain 'Item' and 'Group' columns similiar to the results data
#'          frame as well as a column 'low' corresponding to the sum of levels below
#'          neutral, a column 'high' corresponding to the sum of levels above
#'          neutral, and columns 'mean' and 'sd' corresponding to the mean and
#'          standard deviation, respectively, of the results. The numeric values
#'          are determined by as.numeric which will use the values of the factors.
#' }
#'
#' @export
#' @param items data frame containing the likert based items. The variables
#'        in the data frame should be factors.
#' @param grouping (optional) should the results be summarized by the given
#'        grouping variable.
#' @param nlevels number of possible levels. Only necessary if there are missing levels.
#' @return a likert class with the following elements: results, items, grouping,
#'        nlevels, and summary.
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
							  high=rep(NA, ncol(items) * length(unique(results$Group))),
							  mean=rep(NA, ncol(items) * length(unique(results$Group))),
							  sd=rep(NA, ncol(items) * length(unique(results$Group))) )
		for(g in unique(results$Group)) {
			results2[which(results2$Group == g),]$low = apply(
				results[results$Response %in% lowrange & 
					results$Group == g,3:ncol(results)], 2, sum)
			results2[which(results2$Group == g),]$high = apply(
				results[results$Response %in% highrange & 
					results$Group == g,3:ncol(results)], 2, sum)
			for(i in names(items)) {
				results2[which(results2$Group == g & results2$Item == i), 'mean'] = 
					mean(as.numeric(items[which(grouping == g), i]), na.rm=TRUE)
				results2[which(results2$Group == g & results2$Item == i), 'sd'] = 
					sd(as.numeric(items[which(grouping == g), i]), na.rm=TRUE)
			}
		}
		
		results$Response = factor(results$Response, levels=1:nlevels, 
								  labels=levels(items[,i]))
		results = melt(results, id=c('Group', 'Response'))
		results = cast(results, Group + variable ~ Response)
		results = as.data.frame(results)
		names(results)[2] = 'Item'
	} else {
		results = data.frame(Response=1:nlevels)
		means = numeric()
		sds = numeric()
		for(i in 1:ncol(items)) {
			t = table(items[,i])
			t = (t / sum(t) * 100)
			means[i] = mean(as.numeric(items[,i]), na.rm=TRUE)
			sds[i] = sd(as.numeric(items[,i]), na.rm=TRUE)
			results = cbind(results, as.data.frame(t)[2])
			names(results)[ncol(results)] = names(items)[i]
		}		
		results = as.data.frame(t(results))
		names(results) = as.character(unique(items[!is.na(items[,1]),1]))
		results = results[2:nrow(results),]
		results$Item = row.names(results)
		row.names(results) = 1:nrow(results)
		results2 = data.frame(Item=results$Item, 
							  low=apply(results[,lowrange], 1, sum),
							  high=apply(results[,highrange], 1, sum),
							  mean=means, sd=sds)
		#results = melt(results, id.vars='Item')
	}
	
	r = list(results=results, items=items, grouping=grouping, nlevels=nlevels, 
			 summary=results2)
	class(r) = 'likert'
	return(r)
}

#' Prints results table.
#'
#' @param x the likert class to print.
#' @export
#' @method print likert
#' @S3method print likert
print.likert <- function(x, ...) {
	return(x$results)
}

#' Prints summary table.
#'
#' @param x the likert class to summarize.
#' @export
#' @method summary likert
#' @S3method summary likert
summary.likert <- function(x, ...) {
	return(x$summary)
}

#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param low.color color corresponding to the lowest value likert items
#' @param high.color color corresponding to the highest value likert items
#' @param neutral.color color for middle values. Only used when there are an odd
#'        number of levels.
#' @param text.size size or text labels
#' @param type whether to plot a bar or heat map graphic
#' @export
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, low.color='blue', high.color='red', 
			neutral.color='white', text.colour='white', text.size=2, 
			type=c('bar','heat'), ...)
{
	if(type[1] == 'bar') {
		plot.likert.bar(likert, low.color=low.color, high.color=high.color,
						neutral.color=neutral.color, text.size=text.size, ...)
	} else {
		plot.likert.heat(likert, low.color=low.color, high.color=high.color,
						text.size=text.size, ...)
	}
}

#' Internal method.
#' @seealso plot.likert
plot.likert.bar <- function(likert, low.color='blue', high.color='red', 
							neutral.color='white', text.size=2, ...)
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
		results = melt(likert$results, id=c('Group', 'Item'))
		p = ggplot(results, aes(y=value, x=Group, group=variable)) + 
			scale_fill_manual('Response', values=cols, 
					breaks=unique(results$variable),
					labels=levels(likert$items[,i])) + 
			geom_bar(stat='identity', aes(fill=variable)) + 
			geom_text(data=likert$summary, aes(x=Group, y=0, 
												label=paste(round(low), '%', sep=''), 
												group=Item), size=text.size, hjust=1) +
			geom_text(data=likert$summary, aes(x=Group, y=100, 
												label=paste(round(high), '%', sep=''), 
												group=Item), size=text.size, hjust=-.2) +
			coord_flip() +
			ylab('Percentage') + xlab('') + ylim(c(-5,105)) +
			opts(axis.ticks=theme_blank()) +
			facet_wrap(~ Item, ncol=1)
	} else {
		results = melt(likert$results, id.vars='Item')
		p = ggplot(results, aes(y=value, x=Item, group=Item)) + 
			scale_fill_manual('Response', values=cols, 
					breaks=unique(results$variable), 
					labels=levels(likert$items[,i])) + 
			geom_bar(stat='identity', aes(fill=variable)) + 
			geom_text(data=likert$summary, aes(x=Item, y=0, 
												label=paste(round(low), '%', sep='')), 
					  size=text.size, hjust=1) +
			geom_text(data=likert$summary, aes(x=Item, y=100, 
												label=paste(round(high), '%', sep='')), 
					  size=text.size, hjust=-.2) +
			coord_flip() +
			ylab('Percentage') + xlab('') + ylim(c(-5,105)) +
			opts(axis.ticks=theme_blank())
	} 
	return(p)
}

#' Internal method.
#' @seealso plot.likert
plot.likert.heat <- function(likert, low.color='blue', high.color='red', 
				neutral.color='white', text.colour='white', text.size=2, ...) {
	if(!is.null(likert$grouping)) {
		stop('likert plots with grouping are not supported.')
	}

	results = melt(likert$results, id.vars='Item')
	
	p = ggplot(results, aes(x=Item, y=variable, fill=value, 
								   label=paste(format(value, digits=2, 
								   		drop0trailing=FALSE), '%', sep=''))) + 
 		geom_text(data=likert$summary, aes(y='Mean (SD)', x=Item, fill='white',
 				  label=paste(format(mean, digits=3, drop0trailing=FALSE), 
 							' (', format(sd, digits=2, drop0trailing=FALSE), ')', sep='')), 
 				  size=text.size) +
		geom_tile() + geom_text(size=text.size, colour=text.colour) + coord_flip() + 
		scale_fill_gradient("Percent", low=low.color, high=high.color, limits=c(0,100)) + 
		xlab('') + ylab('') + 
		opts(panel.grid.major=theme_blank(), 
			 panel.grid.minor=theme_blank(), 
			 axis.ticks=theme_blank())
	return(p)
}
