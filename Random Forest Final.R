str(Cork_weather)

Cork_weather <- read.csv("test.csv")
sum(is.na(Cork_weather))
colSums((is.na(Cork_weather)))
mean = mean(Cork_weather$vappr, na.rm = TRUE)
Cork_weather$vappr[is.na(Cork_weather$vappr)] = 10.98

colSums((is.na(Cork_weather)))
mean = mean(Cork_weather$temp, na.rm = TRUE)
Cork_weather$temp[is.na(Cork_weather$temp)] = 10.21


Cork_weather$date = NULL
library(caret)
set.seed(18129633)
sample <- createDataPartition(Cork_weather$vappr, p = .75, list = FALSE) 
train <- Cork_weather[sample, ]
test <- Cork_weather[-sample, ]

library(randomForest)
rfModel = randomForest(vappr~. ,data=train)

library(e1071)
confusionMatrix(predict(rfModel,test), test$vappr)



