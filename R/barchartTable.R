#' Creates a bar chart using percentages ideal for likert items.
#' @param items the items to summarize.
#' @export
barchartTable <- function(items, nlevels=length(which(!is.na(unique(items[,i])))), 
			low.color='blue', high.color='red', neutral.color='white')
{
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
	lowrange = 1 : ceiling(nlevels / 2 - nlevels %% 2)
	highrange = ceiling(nlevels / 2 + 1 ) : nlevels
	results2 = data.frame(item=results$item, 
		low=apply(results[,lowrange], 1, sum),
		high=apply(results[,highrange], 1, sum)
	)
	results = melt(results, id.vars='item')
	ramp = colorRamp(c(low.color, neutral.color))
	ramp = rgb( ramp(seq(0, 1, length=((nlevels+1)/2) )), max=255)
	bamp = colorRamp(c(neutral.color, high.color))
	bamp = rgb( bamp(seq(0, 1, length=((nlevels+1)/2) )), max=255)
	cols = NULL
	if(nlevels %% 2 != 0) {
		cols = c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
	} else {
		cols = c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])
	}
	p = ggplot(results, aes(y=value, x=item, group=item)) + 
		scale_fill_manual('Response', values=cols, 
				breaks=unique(results$variable), 
				labels=levels(items[,i])) + 
		geom_bar(stat='identity', aes(fill=variable)) + 
		geom_text(data=results2, aes(x=item, y=0, label=paste(round(low), '%', sep='')), size=3, hjust=1) +
		geom_text(data=results2, aes(x=item, y=100, label=paste(round(high), '%', sep='')), size=3, hjust=-.2) +
		coord_flip() +
		ylab('Percentage') + xlab('') + ylim(c(-5,105))
		opts(axis.ticks=theme_blank())
	return(p)
}
