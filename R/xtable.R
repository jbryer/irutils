# This file contains functions that wrap the xtable package.

#' Creates table LaTeX table of proportions for the two given variables. The
#' resulting table has two columns for each unique value in y, one corresponding
#' to the overall row column proportions and the second adjusted proportion (i.e.
#' proportion excluding missing values).
#' @export
adjustedTableLaTeX <- function(x, y, caption="", label="", ...) {
	t1 = prop.table(table(x, y, useNA='no'), 2) * 100
	t1 = rbind(t1, NA)
	row.names(t1)[nrow(t1)] = 'Missing'
	t1['Missing',1:ncol(t1)] = NA
	t2 = prop.table(table(x, y, useNA='ifany'), 2) * 100
	row.names(t2)[is.na(row.names(t2))] = 'Missing'
	t = cbind(t2, t1)
	cols = c()
	for(i in 1:(ncol(t)/2)) { cols = c(cols, i, (i + (ncol(t)/2))) }
	t = t[,cols]
	t = as.data.frame(t)
	addtorow = list()
	addtorow$pos = list()
	addtorow$pos[[1]] = c(-1)
	addtorow$command = paste(c("\\hline ",
							   paste( '& \\multicolumn{2}{c}{', names(t)[seq(1, ncol(t), by=2)], '}', sep='' ),
							   "\\\\ \\cline{2-", ncol(t), "} ",
							   rep(" & Percent & Adjusted", (ncol(t)/2)),
							   " \\\\"), collapse="" )
	xt = xtable(t, caption=caption, label=label, align=c('l', rep('r', ncol(t))))
	print(xt, hline.after=c(-1, nrow(xt)), add.to.row=addtorow, include.colnames=FALSE, ...)
}
