library(e1071)
library(party)
library(randomForest)
library(lubridate)

set.seed(7)
data <- read.csv(file.choose())
trainData <- data
#trainData <- subset(trainData, Category=="LARCENY/THEFT" | Category == "OTHER OFFENSES",select= Dates:Y)

Dates1  = strptime(trainData$Dates,"%Y-%m-%d %H:%M:%S")
trainData$Year = as.factor(Dates1$year)
trainData$Month = as.factor(Dates1$mon)
trainData$Hour = as.factor(format(ymd_hms(Dates1), "%H"))
trainData$Category <- as.factor(as.character(trainData$Category))

trainData["Dates"] <- NULL

data_ctree <- ctree(myFormula, data = trainData)
data_naiveBayes <- naiveBayes(Category ~ DayOfWeek + PdDistrict + hour + mon + year, data = trainData)
#data_svm <- svm(myFormula, data = trainData)
data_liblinear <- LiblineaR(trainData, target = trainData$Category, type = 7, verbose = FALSE)

ctreePred <- predict(data_ctree, newdata = testData)
naiveBayesPred <- predict(data_naiveBayes, newdata = testData)
#svmPred <- predict(data_svm, newdata = testData)

judge <- function(ctreePred, naiveBayesPred, svmPred) {
  pred <- ctreePred
  for(i in 1:length(ctreePred)) {
    if(sum(c(ctreePred[i], naiveBayesPred[i], svmPred[i])) > 4.5) {
      pred[i] <- levels(svmPred)[2]
    } else {
      pred[i] <- levels(svmPred)[1]
    }
  }
  return (pred)
}

#pred <- judge(ctreePred, naiveBayesPred, svmPred)

resultTable1 <- table(ctreePred, testData$Category)
resultTable2 <- table(naiveBayesPred, testData$Category)
#resultTable3 <- table(svmPred, testData$Category)

myFuc <- function(table) {
  correctSum <- 0
  for(i in 1:length(table[1,])) {
    correctSum <- correctSum + table[i,i]
  }
  return (correctSum/sum(table))
}

myFuc(resultTable1)
myFuc(resultTable2)
#myFuc(resultTable3)

