#' Function creates and returns a tabular heatmap ideal for summarizing likert
#' survey items.
#' @param items the items to summarize.
#' @export
plotHeatmapTable <- function(items, nlevels=length(levels(items[,1])),
			low.color='white', high.color='blue', text.size=2) 
{	
	results = data.frame(Response=1:nlevels)
	for(i in 1:ncol(items)) {
		t = table(items[,i])
		t = round(t / sum(t) * 100)
		results = cbind(results, as.data.frame(t)[2])
		names(results)[ncol(results)] = names(items)[i]
	}
	results = as.data.frame(t(results))
	results = results[2:nrow(results),]
	items2 = items
	for(i in 1:ncol(items2)) { 
		levels(items2[,i]) = 1:nlevels
		items2[,i] = as.integer(items2[,i])
	}
	results$mean = apply(items2, 2, mean, na.rm=TRUE)
	results$sd = apply(items2, 2, sd, na.rm=TRUE)
	results$item = rownames(results)
	rownames(results) = 1:nrow(results)
	results = melt(results, id=c('item','mean','sd'))
	results$variable = factor(results$variable, levels=paste('V', 1:nlevels, sep=''), labels=levels(items[,1]))
	p = ggplot(results, aes(x=item, y=variable, fill=value, label=paste(value, '%', sep=''))) + 
		geom_text(aes(y='Mean (SD)', x=item, label=paste(format(mean, digits=3, drop0trailing=FALSE), ' (', format(sd, digits=2, drop0trailing=FALSE), ')', sep='')), size=text.size) +
		geom_tile() + geom_text(size=text.size) + coord_flip() + 
		scale_fill_gradient("Percent", low=low.color, high=high.color, limits=c(0,100)) + 
		xlab('') + ylab('') + 
		opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), axis.ticks=theme_blank())
	return(p)
}

