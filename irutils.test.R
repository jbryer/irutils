library(irutils)
library(ggplot2)

theme_update(panel.background=theme_blank(), panel.grid.major=theme_blank(), panel.border=theme_blank())


#Testing the plotting functions
#How much do you agree or disagree with these statements about reading?
data(pisa)

items28 = pisa[,substr(names(pisa), 1,5) == 'ST24Q']
head(items28); ncol(items28)
names(items28) = c("I read only if I have to.",
				   "Reading is one of my favorite hobbies.",
				   "I like talking about books with other people.",
				   "I find it hard to finish books.",
				   "I feel happy if I receive a book as a present.",
				   "For me, reading is a waste of time.",
				   "I enjoy going to a bookstore or a library.",
				   "I read only to get information that I need.",
				   "I cannot sit still and read for more than a few minutes.",
				   "I like to express my opinions about books I have read.",
				   "I like to exchange books with my friends")
for(i in 1:ncol(items28)) {
	items28[,i] = factor(items28[,i], levels=1:4, 
						 labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'), ordered=TRUE)
}

l28 = likert(items28)
print(l28)
plot(l28)
plot(l28, type='heat')

l28g = likert(item28, grouping = pisa$CNT)

plotBarchartTable(items28, low.color='maroon', high.color='burlywood4')
plotBarchartTable(items28, grouping=pisa$CNT, low.color='maroon', high.color='burlywood4')
plotHeatmapTable(items28)


#How often do you read these materials because you want to?
items29 = pisa[,substr(names(pisa), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
for(i in 1:ncol(items29)) {
	items29[,i] = factor(items29[,i], levels=1:5, 
						 labels=c('Never or almost never', 'A few times a year', 'About once a month', 
						 		 'Several times a month', 'Several times a week'), ordered=TRUE)
}

plotBarchartTable(items29, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")
plotBarchartTable(items29, grouping=pisa$CNT, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")
plotHeatmapTable(items29) + opts(title="How often do you read these materials because you want to?")
