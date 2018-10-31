DFT.data <- ddr

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DFT.data), size=0.5*nrow(DFT.data))

#training data 
train50 <- DFT.data[indexes,]
#test data 
test50 <- DFT.data[-indexes,]

# attach(DFT.data)


library(randomForest)
rf50 <- randomForest(formula = Status ~., data = train50, ntree=60, mtry = 20,  importance=T, proximity=T)
# 
# plot(rf50, main="")
Test50_rf_pred <- predict(rf50, test50, type="class")
rand_conf <<-  table(Test50_rf_pred, test50$Status)
# # importance(rf50)
# varImpPlot(rf50,  main="", cex=0.8)

rand_err <<- 100-100*sum(diag(rand_conf) / sum(rand_conf))

# 
# 
# rand_pred <- prediction(as.vector(rf50$votes[,2]), train50$Status) 
# rand_perf <- performance(pred,"tpr","fpr")
# plot(rand_perf)
# abline(0,1)