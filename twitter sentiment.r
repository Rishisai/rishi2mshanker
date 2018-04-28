data = twitter_sanders_apple2
data$class = NULL
str(data)

data$text = gsub("&amp", "", data$text)
data$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$text)
data$text = gsub("@\\w+", "", data$text)
data$text = gsub("[[:punct:]]", "", data$text)
data$text = gsub("[[:digit:]]", "", data$text)
data$text = gsub("http\\w+", "", data$text)
data$text = gsub("[ \t]{2,}", "", data$text)
data$text = gsub("^\\s+|\\s+$", "", data$text) 
data$text <- tolower(data$text)


install.packages("rJava")
options(java.parameters = "- Xmx1024m")
install.packages("RSentiment")
library(RSentiment)

result <- data.frame(matrix(nrow = 479, ncol = 2))
for(i in 1:479){
  score = calculate_score(data$text[i])
  result[i,1] = i
  result[i,2] = score
}

result1 = result

sort(unique(result$X2))
result1$X1 = NULL
result1$X2 <- ordered(result1$X2,
                     levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                     labels = c("Neg4","Neg3","Neg2","Neg1", "Pos0","Pos1","Pos2","Pos3","Pos4","Pos5"))
result1$X2 = gsub('.{1}$', '', result1$X2)

#Placing all missclassifiers as N/a's
ifelse(result1$X2==twitter_sanders_apple2$class,1,ifelse(result1$X2==twitter_sanders_apple2$class,0,NA))

match(result1$X2,twitter_sanders_apple2$class)
sum((match(result1$X2,twitter_sanders_apple2$class)))
