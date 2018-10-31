# load package FactoMineR and ggplot2
require(FactoMineR)
require(ggplot2)
# DF.data <-read.csv(file.choose(),header = TRUE, sep=",")

require(xgboost)
require(data.table)

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))

#training data 
train50 <- DF.data[indexes,]
#test data 
test50 <- DF.data[-indexes,]

setDT(train50)
setDT(test50)
labels <- train50$Status
ts_label <- test50$Status
new_tr <- model.matrix(~.+0,data = train50[,-c("Status"),with=F])
new_ts <- model.matrix(~.+0,data = test50[,-c("Status"),with=F])

#convert factor to numeric
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1




train <- xgb.DMatrix(data = new_tr,label = labels)
test <- xgb.DMatrix(data = new_ts,label=ts_label)

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.3,
  gamma=0,
  max_depth=6,
  min_child_weight=1,
  subsample=1,
  colsample_bytree=1
)

xgbcv <- xgb.cv(params = params
                ,data = train
                ,nrounds = 100
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every_n = 10
                ,early_stopping_rounds = 20
                ,maximize = F
)

######################min(xgbcv$test_error_mean)



xgb1 <- xgb.train(
  params = params
  ,data = train
  ,nrounds = 30
  ,watchlist = list(val=test,train=train)
  ,print_every_n = 10
  ,early_stopping_rounds = 3
  ,maximize = F
  ,eval_metric = "error"
)

xgbpred <- predict(xgb1,test)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

library(caret)

confusionMatrix(
  factor(xgbpred, levels = 1:2223),
  factor(ts_label, levels = 1:2223)
)

#confusionMatrix(xgbpred, ts_label)

mat <- xgb.importance(feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:15])


#################display confusion and accuracy.