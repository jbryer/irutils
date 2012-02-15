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
