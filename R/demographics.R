#' 
#' @export
demographics <- function(df, useNA='ifany') {
	results = data.frame(Category=character(), Frequency=numeric(), Percentage=numeric())
	
	for(i in seq_len(ncol(df))) {
		t = as.data.frame(table(df[,i], useNA=useNA), stringAsFactors=FALSE)
		t$Var1 = as.character(t$Var1)
		names(t) = c('Category', 'Frequency')
		t$Percentage = t$Frequency / sum(t$Frequency) * 100
		m = which(is.na(t$Category))
		if(length(m) > 0) {
			t[m,'Category'] = 'Missing'
		}
		results = rbind(results, 
						data.frame(Category=names(df)[i], Frequency=NA, Percentage=NA),
						t)
	}
	class(results) = c("demographics", "data.frame")
	return(results)
}

#'
#' @export
xtable.demographics <- function(x, caption = NULL, label = NULL, align = NULL,
								digits = NULL, display = NULL, ...) {
	cat('using xtable...')
	
}

if(FALSE) {
	require(xtable)
	demographics(students[,c('Ethnicity','Gender','School')])
}
