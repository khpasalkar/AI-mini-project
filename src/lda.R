

DF.data <-read.csv(file.choose(),header = TRUE, sep=",")

View(DF.data)


attach(DF.data)

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))
#training data contient les données indexé par indexes
train50 <- DF.data[indexes,]
#test data contient le rest
test50 <- DF.data[-indexes,]

#install le package Mass
library(MASS)

# Linear Discriminant Analysis
ldafit <- lda(Status ~ Seniority + Age + Income + Debt + Amount + Finrat + 
                seniorityR + expensesR + assetsR + priceR + savingsR + Home + Marital + Records + 
                Job, data = train50)
ldafit
plot(ldafit)
lda.pred <- predict(ldafit, data=Test50)
ldaclass <- lda.pred$class
table(ldaclass, test50$Status)
detach(DF.data)
