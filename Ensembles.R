wine <- read.csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
  header=F)
names(wine) <- c(
  "Alcohol",
  "MalicAcid",
  "Ash",
  "Height",
  "Alcalinity",
  "Magnesium",
  "TotalPhenols",
  "Flavanoids",
  "NonflavanoidPhenols",
  "Proanthocyanins",
  "ColorIntensity",
  "Hue",
  "OD280OD315",
  "Proline"
)
wine$Alcohol <- factor(wine$Alcohol)

barplot(table(wine$Alcohol))

boxplot(wine[, c(2:13)])

sapply(wine, FUN=function(x) {sum(is.na(x))})

wineKNN<- wine

summary(wineKNN)

set.seed(1337)
library(caret)

index <- createDataPartition(wineKNN$Alcohol, p = .75, list = FALSE)

trainKNN <- wineKNN[index,]

validationKNN <- wineKNN[-index,]

(ENSEMBLES PAGE 04)