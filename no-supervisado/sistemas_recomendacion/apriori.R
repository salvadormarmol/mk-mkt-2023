library(arules)

tr<-read.transactions("test.csv",format="basket",sep=",")
inspect(tr)
image(tr)
itemFrequencyPlot(tr, support = 0.1)
length(tr)
rules <- apriori(tr, parameter= list(supp=0.6, conf=0.5))
inspect(rules)
