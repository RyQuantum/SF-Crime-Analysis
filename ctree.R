set.seed(1)
myFormula <- Category ~ DayOfWeek + PdDistrict +X:Y + hour + mon + year
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.1, 0.9))
trainData <- data[ind == 1,]
trainData <- subset(trainData, Category=="LARCENY/THEFT" | Category == "OTHER OFFENSES",select= Dates:Y)

Dates1  = strptime(as.character(trainData$Dates),"%Y-%m-%d %H:%M:%S")

trainData$year = Dates1$year
trainData$mon = Dates1$mon
trainData$hour = as.numeric(format(ymd_hms(Dates1), "%H"))
trainData["Dates"] = NULL
trainData$Category <- as.factor(as.character(trainData$Category))

data_ctree <- ctree(myFormula, data = trainData)
plot(data_ctree, type="simple")
testPred <- predict(data_ctree, newdata = trainData)
resultTable <- table(testPred, trainData$Category)
resultTable
rate <- (resultTable[1,1] + resultTable[2,2])/(resultTable[1,1] + resultTable[2,1]+ resultTable[1,2]+ resultTable[2,2])
rate

confusionMatrix(testPred, trainData$Category, positive = "1")