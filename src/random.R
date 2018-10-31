DFT.data <-read.csv(file.choose(),header = TRUE, sep=",")

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DFT.data), size=0.5*nrow(DFT.data))

#training data 
train50 <- DFT.data[indexes,]
#test data 
test50 <- DFT.data[-indexes,]

# attach(DFT.data)


library(randomForest)
rf50 <- randomForest(formula = Status ~., data = train50, ntree=200, importance=T, proximity=T)

plot(rf50, main="")
Test50_rf_pred <- predict(rf50, test50, type="class")
table(Test50_rf_pred, test50$Status)
importance(rf50)
varImpPlot(rf50,  main="", cex=0.8)