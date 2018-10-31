dd = ddr
n = nrow(dd)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

# "whole enchilada" logistic regression modeldd
gl1 = glm(Status ~ ., data=dd[learn,], family=binomial)

# Apply logistic regression after removing some variables

# new model
glf <- glm(formula = Status ~ Seniority + Age + Income + Debt + Amount + Finrat + 
             seniorityR + expensesR + assetsR + priceR + savingsR + Home + Marital + Records + 
             Job, family = binomial, data = dd[learn, ]) 


# re-expressed fitted values
glf$fitted.values = 1 - glf$fitted.values

# create vector for predictions
glfpred = rep(NA, length(glf$fitted.values))
glfpred[glf$fitted.values < 0.5] = 1
glfpred[glf$fitted.values >= 0.5] = 0

# error rate
error_rate.learn = 100*sum(diag(table(dd$Status[learn], glfpred))) / nlearn 

# let's use the test data to get predictions
glft = predict(glf, newdata=dd[-learn,])
pt = 1 / (1 + exp(-glft))
pt = 1 - pt

# vector of predicted values
glftpred = rep(NA, length(pt))
glftpred[pt < 0.5] = 1
glftpred[pt >= 0.5] = 0
# shiny confusion matrix
logistic_conf <<- table(dd$Status[-learn], glftpred)
error_rate.test = 100 - 100*sum(diag(table(dd$Status[-learn], glftpred))) / ntest 
logistic_err <<- error_rate.test
# Concentration curve

ac_tot = 100*(1:ntest) / ntest

pt.ord = order(pt, decreasing=T)

Status_test = dd$Status[-learn]
npos = table(Status_test)[2]

ac_pos_test = 100*cumsum(Status_test[pt.ord] == "good") / npos

# plot(ac_pos_test,ac_tot,  type="l", lwd=2, 
#     main="Concentration Curve")

# ROC Curve

nneg = ntest - npos

ac_neg_test = 100*cumsum(Status_test[pt.ord]=="bad") / nneg

#plot(ac_pos_test,ac_neg_test,  type="l", lwd=2, main="ROC Curve", col="blue")
