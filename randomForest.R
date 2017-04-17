set.seed(1243)
myFormula <- Category ~ DayOfWeek + PdDistrict + hour + mon + year + X:Y
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.4, 0.6))
trainData <- data[ind == 1,]

myFuc2 <- function(data) {
  data <- subset(data, Category=="LARCENY/THEFT" | Category == "OTHER OFFENSES",select= Dates:Y)
  Dates1  = strptime(as.character(data$Dates),"%Y-%m-%d %H:%M:%S")
  data$year = Dates1$year
  data$mon = Dates1$mon
  data$hour = as.numeric(format(ymd_hms(Dates1), "%H"))
  data$Category <- as.factor(as.character(data$Category))
  return (data)
}
trainData <- myFuc2(trainData)
ind <- sample(2, nrow(trainData), replace = TRUE, prob = c(0.5, 0.5))
testData <- trainData[ind == 2,]
trainData <- trainData[ind == 1,]

data_randomForest <- randomForest(myFormula, data = trainData)
testPred <- predict(data_randomForest, newdata = testData)
resultTable <- table(testPred, testData$Category)
resultTable

myAcc <- function(table) {
  correctSum <- 0
  for(i in 1:length(table[1,])) {
    correctSum <- correctSum + table[i,i]
  }
  return (correctSum/sum(table))
}
myAcc(resultTable)
confusionMatrix(testPred, testData$Category, positive = "1")
