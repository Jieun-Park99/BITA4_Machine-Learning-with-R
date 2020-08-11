setwd("C:/Users/danan/Desktop/Bitamin/ML_R")

#install.packages("arules")
library(arules)
groceries = read.transactions("groceries.csv", sep=',')
summary(groceries)

inspect(groceries[1:3])
itemFrequency(groceries[,1:3])
image(groceries[1:5])

itemFrequencyPlot(groceries, support=0.1)
itemFrequencyPlot(groceries, topN=10)

rule = apriori(groceries, parameter = list(support=0.1, confidence=0.8))
rule2 = apriori(groceries,
                parameter=list(support=0.006, confidence=0.25, minlen=2))


#install.packages("arulesViz")
library(arulesViz)
plot(rule2, method="graph", control=list(type='items'))
plot(rule2, method="grouped", control=list(type='items'))
plot(rule2, method="scatterplot", control=list(type='items'))

plot(rule2, method="graph", control=list(type='items'))
plot(rule2, method="graph", control=list(type= 'itemsets'))


summary(rule2)
inspect(sort(rule2,by='lift')[1:5])
inspect(sort(rule2,by='confidence')[1:5])

berry_rule = subset(rule2, items %in% "berries")
inspect(sort(berry_rule, by='lift'))
inspect(sort(berry_rule, by='confidence'))
