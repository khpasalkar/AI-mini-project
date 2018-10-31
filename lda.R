DF.data <-read.csv('CleanCreditScoring.csv',header = TRUE, sep=",")

attach(DF.data)

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))
#training data contient les données indexé par indexes
train50 <- DF.data[indexes,]
#test data contient le rest
test50 <- DF.data[-indexes,]

# Linear Discriminant Analysis
ldafit <- lda(Status ~ Seniority + Age + Income + Debt + Amount + Finrat + 
                seniorityR + expensesR + assetsR + priceR + savingsR + Home + Marital + Records + 
                Job, data = train50)

# 
# ldafit
# plot(ldafit)
# 

lda.pred <- predict(ldafit, data=Test50)
ldaclass <- lda.pred$class

lda_conf <<- table(ldaclass, test50$Status)
lda_err <<- 100-100*sum(diag(lda_conf)) / sum(lda_conf)

 lda_pred <- prediction(lda.pred$posterior[,2], train50$Status) 
 lda_perf <- performance(lda_pred,"tpr","fpr")
 # plot(perf,colorize=TRUE)

