
rm(list =ls(all= T))
# setwd("E:\\Myth_Insofe")
source("output_mith.log", echo=T)

library(lubridate)
library(Smisc)
library(laeken)
library(DMwR)
library(car)
library(plyr)
library(caTools)

student_data <- read.csv("train.csv",header = T , sep = ",")

str(student_data)
summary(student_data)

#Convert to NA's
# School.Board.in.Tenth , Score.in.Twelth => 0's
#Score.in.Domain => -1
pMiss_na <- function(x){sum(is.na(x))/length(x)*100}
pMiss_null <- function(x){sum(is.null(x))/length(x)*100}

var <- c("School.Board.in.Tenth","Score.in.Twelth")
student_data[,var] <- sapply(student_data[,var],function(x) ifelse(x=="0",NA,x))

var <- c("Score.in.Domain")
student_data[,var] <- sapply(student_data[,var],function(x) ifelse(x==-1,NA,x))

apply(student_data,2,pMiss_na)
apply(student_data,2,pMiss_null)

# Convert to factors 
# ->CollegeCode,CollegeTier,Year.of.Graduation.Completion,CityCode,CityTier,Year.of.Graduation.Completion

student_data$CollegeCode <- as.character(student_data$CollegeCode)
student_data$CollegeTier <- as.character(student_data$CollegeTier)
student_data$Year.Of.Twelth.Completion <- as.character(student_data$Year.Of.Twelth.Completion)
student_data$CityCode <- as.character(student_data$CityCode)
student_data$CityTier <- as.character(student_data$CityTier)
student_data$Year.of.Graduation.Completion <- as.character(student_data$Year.of.Graduation.Completion)
student_data$Date.Of.Birth <- as.character(student_data$Date.Of.Birth)

# Found its more meaningfull to add this column into the dataset
# student_data$age <- 
student_data$age <-  (as.numeric(student_data$Year.of.Graduation.Completion) -  as.numeric(year(lubridate::mdy(formatDT(c(student_data$Date.Of.Birth), posix = FALSE)$date))))


# Dropping off ID columns as they do not impact the Model development
#Age is calculated- removing DOB
student_data$Date.Of.Birth <- NULL
student_data$Candidate.ID<- NULL
str(student_data)
summary(student_data)



#Impute NA's in the dataset

student_data_Imp<-centralImputation(student_data[, !names(student_data) %in% "Pay_in_INR"])
apply(student_data,2,pMiss_na)

student_data <- data.frame(cbind(student_data$Pay_in_INR,student_data_Imp))
student_data <- rename(student_data, c("student_data.Pay_in_INR"="Pay_in_INR"))
str(student_data)

set.seed(12345)

sample = sample.split(student_data$Pay_in_INR, SplitRatio = 0.70)

train = subset(student_data, sample == TRUE)
test  = subset(student_data, sample == FALSE)

except_targt <- student_data[, names(student_data) != "Pay_in_INR"]  
columns_non_trgt <- colnames(except_targt)
columns_non_trgt

#################### MODEL BUILD ###############
# Model 02
#Normal regression
lin.reg2 <- lm(Pay_in_INR ~ Gender +Score.in.Tenth+
                 School.Board.in.Tenth+Year.Of.Twelth.Completion+
                 Score.in.Twelth+Board.in.Twelth+
                 CollegeCode+CollegeTier+
                 Graduation+Discipline+
                 GPA.Score.in.Graduation+CityCode+
                 CityTier+State+
                 Year.of.Graduation.Completion+Score.in.English.language+
                 Score.in.Logical.skill+Score.in.Quantitative.ability+
                 Score.in.Domain+Score.in.ComputerProgramming+
                 Score.in.ElectronicsAndSemicon+Score.in.ComputerScience+
                 Score.in.MechanicalEngg+Score.in.ElectricalEngg+
                 Score.in.TelecomEngg+Score.in.CivilEngg+
                 Score.in.conscientiousness+Score.in.agreeableness+
                 Score.in.extraversion+Score.in.nueroticism+
                 Score.in.openess_to_experience+age, data = train)

# Inspect the model
summary(lin.reg2)

test.pred.lin2 <- exp(predict(lin.reg2,test))
RMSE.lin.reg2 <- sqrt(mean((test.pred.lin2-test$rain)^2))
RMSE.lin.reg2

MAE.lin.reg2 <- mean(abs(test.pred.lin2-test$rain))
MAE.lin.reg2


# Model 03
### Applying VIF

log_reg_step = stepAIC(lin.reg2, direction = "both")

logreg_AIC=lm(Pay_in_INR ~ .,data = train)

# All Subsets Regression
library(leaps)
attach(mydata)
leaps<-regsubsets(y~.,data=mydata,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")

prob_train_AIC <- predict(logreg_AIC, type="response")
pred_AIC <- prediction(prob_train_AIC, train$Pay_in_INR)

perf_AIC <- performance(pred_AIC, measure="tpr", x.measure="fpr")
plot(perf_AIC, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.1))

preds_test_AIC <- ifelse(prob_train_AIC > 0.4, "1", "0")
table(preds_test_AIC,train$Pay_in_INR)


prob_test_AIC<-predict(logreg_AIC,test1,type="response")
pred_test_AIC<-ifelse(prob_test_AIC>0.4,"1","0")
table(pred_test_AIC,test1$Pay_in_INR)



#Calculating Area under curve
perf_auc_AIC <- performance(pred_AIC, measure="auc")
auc_AIC <- perf_auc_AIC@y.values[[1]]
print(auc_AIC)


conf_matrix1<-table(test1$Pay_in_INR,pred_test_AIC)
print(conf_matrix1)
specif<-round(conf_matrix1[1,1]/sum(conf_matrix1[1,]),4)
sensit<-round(conf_matrix1[2,2]/sum(conf_matrix1[2,]),4)
accu<-round(sum(diag(conf_matrix1))/sum(conf_matrix1),4)
print(c("accuracy",accu,"specificity",specif,"sensitivity",sensit))


vif(logreg_AIC)



# Model 01
# Normal Reg with log
lin.reg1 <- lm(log(Pay_in_INR+1) ~., data = train)

# Inspect the model
summary(lin.reg1)

exp(lin.reg1$coefficients["train.age"])
test.pred.lin1 <- exp(predict(lin.reg1,test))-1
RMSE.lin.reg1 <- sqrt(mean((test.pred.lin1-test$Pay_in_INR)^2))
RMSE.lin.reg1

MAE.lin.reg1 <- mean(abs(test.pred.lin1-test$Pay_in_INR))
MAE.lin.reg1

# Model 04

# Needed to grow a tree
library(rpart)
# To draw a pretty tree (fancyRpartPlot function)
library(rattle)

# rpart function applied to a numeric variable => regression tree
rt <- rpart(Pay_in_INR ~ ., data=train)

# Full-grown tree with 8 splits using 6 different variables 
# (Not running the line below - do it to see the tree)
# fancyRpartPlot(rt)
  
# As always, predict and evaluate on the test set
 test.pred.rtree <- predict(rt,test) 

RMSE.rtree <- sqrt(mean((test.pred.rtree-test$rain)^2))
RMSE.rtree
#[1] 11.37786

MAE.rtree <- mean(abs(test.pred.rtree-test$rain))
MAE.rtree
#[1] 6.140937

#Now that we have a full-grown tree, let's see if it's possible to prune it.

# Check cross-validation results (xerror column)
# It corresponds to 2 splits and cp = 0.088147
printcp(rt)








