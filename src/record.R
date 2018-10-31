record <- function(x){
  dd =x;
# ================================================================================
# Recoding (categorizing) continuous variables 
# ================================================================================

# The next stage involves recoding continuous variables into 
# categorical values. This step assumes that you have 
# previously explored the distributions of the continuous 
# variables in order to determine appropriate cutoffs

seniorityR = cut(dd$Seniority, breaks=c(-1,1,3,8,14,99))
timeR = cut(dd$Time, breaks=c(0,12,24,36,48,99))
ageR = cut(dd$Age, breaks=c(0,25,30,40,50,99))
expensesR = cut(dd$Expenses, breaks=c(0,40,50,60,80,9999))
incomeR = cut(dd$Income, breaks=c(0,80,110,140,190,9999))
assetsR = cut(dd$Assets, breaks=c(-1,0,3000,5000,8000,999999))
debtR = cut(dd$Debt, breaks=c(-1,0,500,1500,2500,999999))
amountR = cut(dd$Amount, breaks=c(0,600,900,1100,1400,99999))
priceR = cut(dd$Price, breaks=c(0,1000,1300,1500,1800,99999))
finratR = cut(dd$Finrat, breaks=c(0,50,70,80,90,100))
savingsR = cut(dd$Savings, breaks=c(-99,0,2,4,6,99))

# let's label the categorized variables
levels(seniorityR) = paste("sen", levels(seniorityR))
levels(timeR) = paste("time", levels(timeR))
levels(ageR) = paste("age", levels(ageR))
levels(expensesR) = paste("exp", levels(expensesR))
levels(incomeR) = paste("inc", levels(incomeR))
levels(assetsR) = paste("asset", levels(assetsR))
levels(debtR) = paste("debt", levels(debtR))
levels(amountR) = paste("am", levels(amountR))
levels(priceR) = paste("priz", levels(priceR))
levels(finratR) = paste("finr", levels(finratR))
levels(savingsR) = paste("sav", levels(savingsR))

# add categorized variables to dataframe
dd$seniorityR = seniorityR
dd$timeR = timeR
dd$ageR = ageR
dd$expensesR = expensesR
dd$incomeR = incomeR
dd$assetsR = assetsR
dd$debtR = debtR
dd$amountR = amountR
dd$priceR = priceR
dd$finratR = finratR
dd$savingsR = savingsR


# Once we have preprocessed the data, we can save it in a new file
# This is the file we'll be using in the next parts
write.csv(dd, "CleanCreditScoring.csv", row.names=FALSE)
}