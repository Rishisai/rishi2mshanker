library(arules)
library(readr)
data <- read.csv(file = "C:\\Users\\Rishi\\Documents\\groceries_v2.csv")
View(data)
y=read.transactions(file="C:\\Users\\Rishi\\Documents\\groceries_v2.csv", rm.duplicates= TRUE, format='basket',sep=',',cols=1)
#txn <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(y,parameter = list(sup = 0.01, conf = 0.5,target="rules"))
inspect(basket_rules)#Visualize the rules
