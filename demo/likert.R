data(pisana)

items28 = pisana[,substr(names(pisana), 1,5) == 'ST24Q']
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
						 labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'),
						 ordered=TRUE)
}

l28 = likert(items28)
summary(l28)

plot(l28, low.color='maroon', high.color='burlywood4')
plot(l28, type='heat', text.size=5)


l28g = likert(items28, grouping=pisana$CNT)
summary(l28g)

plot(l28g)

