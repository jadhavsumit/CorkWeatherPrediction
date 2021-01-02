library(C50)
cFiftymodel = C5.0(x = train[,1:2], y = train$vappr)
summary(cFiftymodel)
plot(cFiftymodel)
pred = predict(cFiftymodel, test[,1:2])
table(pred, test$vappr)




rulesModel = cFiftymodel = C5.0(x = train$vappr, y = test$vappr, rules = TRUE)
summary(rulesModel)

str(pred)

pred = predict(rulesModel, test[,2:3])
table(pred, Cork_weather$vappr)