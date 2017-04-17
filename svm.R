set.seed(2)
myFormula <- Category ~ DayOfWeek + PdDistrict + hour + mon + year + X:Y
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.1, 0.9))
trainData <- data[ind == 1,]
trainData <- subset(trainData, Category=="LARCENY/THEFT" | Category == "OTHER OFFENSES",select= Dates:Y)

Dates1  = strptime(as.character(trainData$Dates),"%Y-%m-%d %H:%M:%S")

trainData$year = Dates1$year
trainData$mon = Dates1$mon
trainData$hour = as.numeric(format(ymd_hms(Dates1), "%H"))

trainData$Category <- as.factor(as.character(trainData$Category))

data_svm <- svm(myFormula, data = trainData)
testPred <- predict(data_svm, newdata = trainData)
resultTable <- table(testPred, trainData$Category)
resultTable

myFuc <- function(table) {
  correctSum <- 0
  for(i in 1:length(table[1,])) {
    correctSum <- correctSum + table[i,i]
  }
  return (correctSum/sum(table))
}
myFuc(resultTable)
confusionMatrix(testPred, trainData$Category, positive = "1")