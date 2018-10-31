
ct = rpart(Status ~ ., data=dd)

n = nrow(dd)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

ct1 = rpart(Status ~ ., data = dd[learn,], method="class",
            parms = list(prior=c(0.50, 0.50), split='gini'), 
            control = rpart.control(cp=0.001, xval=10, maxdepth=15))



min.xe = which(ct1$cptable[,4] == min(ct1$cptable[,4]))



ct2 = rpart(Status ~ ., 
            data = dd[learn,],
            parms = list(prior=c(0.50, 0.50), split='gini'), 
            control = rpart.control(cp=0.00285, xval=0, maxdepth=15))


#### calculate error rate in the learning sample
# (this will give a matrix)
ct2.learn = predict(ct2, data=dd[learn,])

# create a vector with predicted status
ct2.learnp = rep("", nlearn)
ct2.learnp[ct2.learn[,1] < 0.5] = "pred_neg"
ct2.learnp[ct2.learn[,1] >= 0.5] = "pred_pos"

# let's make a table
status_learn = table(dd$Status[learn], ct2.learnp)

#### classification error
# 100 * sum(diag(status_learn)) / nlearn

# calculate error rate in the testing sample
#(this will give a matrix)
ct2.test = predict(ct2, newdata=dd[-learn,]) #ERR

ct2.testp = rep("", ntest)
ct2.testp[ct2.test[,1] < 0.5] = "pred_neg"  #ERR
ct2.testp[ct2.test[,1] >= 0.5] = "pred_pos" #ERR
# let's make a table

status_test = table(dd$Status[-learn], ct2.testp)

# classification error
# show
# 100 * sum(diag(status_test)) / ntest

# we'll repeat the same but changing the cp=0.002
ct3 = rpart(Status ~ ., 
            data = dd[learn,],
            parms = list(prior=c(0.50, 0.50), split='gini'), 
            control = rpart.control(cp=0.002, xval=0, maxdepth=15))

# show
# par(mar = c(1,1,2,0.5))
# plot(ct3, margin=0.05, compress=TRUE, main="Decision Tree", uniform = TRUE)
# text(ct3, use.n=TRUE, all=TRUE, cex=0.6)

# calculate error rate in the learning sample
# (this will give a matrix)
ct3.learn = predict(ct3, data=ddtot[learn,])
# create a vector with predicted status
ct3.learnp = rep("", nlearn)
ct3.learnp[ct3.learn[,1] < 0.5] = "pred_neg"
ct3.learnp[ct3.learn[,1] >= 0.5] = "pred_pos"

#### let's make a table
# show 
# table(dd$Status[learn], ct3.learnp)
# # classification error
# 100 * sum(diag(table(dd$Status[learn], ct3.learnp))) / nlearn

# # calculate error rate in the testing sample #ERR
# # (this will give a matrix)
ct3.test = predict(ct3, newdata=dd[-learn,])
# # create a vector with predicted status
ct3.testp = rep("", ntest)
ct3.testp[ct3.test[,1] < 0.5] = "pred_neg"
ct3.testp[ct3.test[,1] >= 0.5] = "pred_pos"

# # let's make a table
# show
# table(dd$Status[-learn], ct3.testp)
# # classification error
# 100 * sum(diag(table(dd$Status[-learn], ct3.testp))) / ntest

# # concentration curve #ERR
# # the positive predictions on the test sample
# pred.test = ct2.test[,1]
# # the number of individuals in each value
# totn = table(-pred.test) / ntest
# ac_totn = 100 * cumsim(as.numeric(totn))

# ranking the predictions
# rank_pred.test = rank(pred.test)
# how many positive are in each leave?
Status.test = dd$Status[-learn]
table(Status.test)
npos = table(Status.test)[1]

# tapply(Status.test == "good", ran_pred.test, sum) ##ERR
# ac_true.pos = 100 * cumsum(rev(as.numeric(tabla))) / npos

# ROC curve
nneg = ntest - npos
#ac_fals.pos = 100 * cumsum(rev()) #ERR
