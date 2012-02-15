#' Creates a pie chart using ggplot2.
#'
#' Use pie charts with care. See http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=00018S
#' on Edward Tufte's website for good arguments against the use of pie charts.
#' For a contrary point-of-view, see Spence's article, No Humble Pie: The Origins and
#' Usage of a Statistical Chart (http://www.psych.utoronto.ca/users/spence/Spence%202005.pdf).
#' 
#' @param df the data frame.
#' @param col the name of the column to generate the pie chart for.
#' @param label the label for the legend.
#' @export
pie <- function(df, col, label=col) {
	df$pie = df[,col]
	l = levels(df$pie)
	t = table(df$pie)
	levels(df$pie) = paste(names(t), " ", format(100*t/sum(t),digits=1), "%", sep="")
	p = ggplot(df, aes(x=factor(1), fill=pie)) + 
		geom_bar(width=1) + coord_polar(theta="y") + xlab("") + ylab("") + 
		scale_fill_hue(name=label, breaks=levels(df$pie), labels=levels(df$pie)) + 
		opts(axis.text.y=theme_blank(), axis.text.x=theme_blank(), axis.ticks=theme_blank())
	print(p)
}
