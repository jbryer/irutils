library(foreign)

pisa = read.spss('pisausa2009.sav', to.data.frame=TRUE)
save(pisa, file='data/pisa.Rdata')
names(pisa); nrow(pisa)

#How much do you agree or disagree with these statements about reading?
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
	items28[,i] = factor(items28[,i], levels=1:4, labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'), ordered=TRUE)
}

barchartTable(items28, low.color='red', high.color='blue')


#How often do you read these materials because you want to?
items29 = pisa[,substr(names(pisa), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
for(i in 1:ncol(items29)) {
	items29[,i] = factor(items29[,i], levels=1:5, labels=c('Never or almost never', 'A few times a year', 'About once a month', 'Several times a month', 'Several times a week'), ordered=TRUE)
}

barchartTable(items29, low.color='red', high.color='blue') + opts(title="How often do you read these materials because you want to?")
heatmapTable(items29) + opts(title="How often do you read these materials because you want to?")

