set.seed(42)
data <- data.frame(
  cat1 = sample(LETTERS[1:7],500,replace = TRUE),
  cat2 = sample(letters[18:23],500,replace = TRUE),
  cat3 = sample(LETTERS[8:14],500,replace = TRUE),
  cat4 = sample(letters[14:19],500,replace = TRUE),
  num1 = rnorm(500,4,2),
  num2 = rbeta(500,2,4,0),
  num3 = rf(500,2,24,0)
)

data[sample(1:500,50),'num1'] <- NA
data[data$cat4 == 'p','cat3'] <- NA
data[data$cat1 == 'A','num3'] <- NA
data[data$num2 >= 0.7,'num2'] <- NA
nrow(data)
sum(is.na(data))
colSums(is.na(data))
summary(data)

library(visdat)
vis_dat(data)
vis_miss(data)
library(Amelia)
missmap(data)
library(ggplot2)
library(naniar)

gg_miss_var(data)

library(VIM)

aggr(data,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)

library(naniar)
ggplot(data, aes(x = cat1,y = cat4)) +
  geom_miss_point()+
  facet_wrap(~cat2)

library(ggplot2)

ggplot(data, aes(x = cat3,y = cat4)) +
  geom_miss_point()+
  facet_wrap(~cat2)

ggplot(data, aes(x = cat1,y = num1)) +
  geom_miss_point()

ggplot(data, aes(x = cat1,y = num3)) +
  geom_miss_point()


#MCAR

newdata <- data[complete.cases(data),]
nrow(newdata)
summary(newdata)
colSums(is.na(data))
data[is.na(data$num1),'num1'] <- mean(data$num1,na.rm = TRUE)
colSums(is.na(data))

#the mode

theMode <- unique(data[!is.na(data$cat3),'cat3'])[which.max(tabulate(match(data[!is.na(data$cat3),'cat3'],
                                                                           unique(data[!is.na(data$cat3),'cat3']))))]
data[is.na(data$cat3),'cat3'] <- theMode
print(theMode)

sum(is.na(data$cat3))

data[sample(1:500,50),'num1'] <- NA
data[data$cat4 == 'p','cat3'] <- NA
data[data$cat1 == 'p','num3'] <- NA


library(Amelia)
data.imputed <- amelia(data,m=3,noms = c(1,2,3,4))

new_data <- data.imputed$imputations$imp1
sum(is.na(new_data))
head(new_data)



############imputations using mice

data[sample(1:500,50),'num1'] <- NA
data[data$cat4 == 'p','cat3'] <- NA
data[data$cat1 == 'A','num3'] <- NA

library(mice)
data.imputed <- mice(data,m=3,maxit=3)
new_data <- complete(data.imputed,3)
sum(is.na(new_data))
head(new_data)
