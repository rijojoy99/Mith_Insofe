
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
library(randomForest)

student_data <- read.csv("train.csv",header = T , sep = ",")
actual_test <- read.csv("test.csv",header = T,sep=",")
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

student_data$CollegeCode <- as.factor(student_data$CollegeCode)
student_data$CollegeTier <- as.factor(student_data$CollegeTier)
student_data$Gender <- as.factor(student_data$Gender)
# student_data$Year.Of.Twelth.Completion <- as.character(student_data$Year.Of.Twelth.Completion)
student_data$CityCode <- as.factor(student_data$CityCode,levels = levels(actual_test$CityCode))
student_data$CityTier <- as.factor(student_data$CityTier,levels = levels(actual_test$CityTier))
student_data$Year.of.Graduation.Completion <- as.factor(student_data$Year.of.Graduation.Completion,levels = levels(actual_test$Year.of.Graduation.Completion))
student_data$Date.Of.Birth <- as.character(student_data$Date.Of.Birth,levels = levels(actual_test$Date.Of.Birth))
student_data$Graduation<- as.factor(student_data$Graduation,levels = levels(actual_test$Graduation))
# student_data$Score.in.nueroticism<- factor(student_data$Score.in.nueroticism,levels = levels(actual_test$Score.in.nueroticism))
student_data$Score.in.Domain <- as.factor(student_data$Score.in.Domain,levels = levels(actual_test$Score.in.Domain))
student_data$State  <- factor(student_data$State,levels = levels(actual_test$State))
# student_data$Score.in.nueroticism  <- as.factor(student_data$Score.in.nueroticism)
# Dropping off ID columns as they do not impact the Model development
#Age is calculated- removing DOB
student_data$Date.Of.Birth <- NULL
student_data$Candidate.ID<- NULL
str(student_data)
summary(student_data)

# student_data$Score.in.nueroticism <- NULL
student_data$State <- NULL
# student_data$Score.in.Domain <- NULL

# Found its more meaningfull to add this column into the dataset
# student_data$age <- 
student_data$age <-  (as.numeric(student_data$Year.of.Graduation.Completion) -  as.numeric(year(lubridate::mdy(formatDT(c(student_data$Date.Of.Birth), posix = FALSE)$date))))


str(student_data)
library(dummies)
summary(student_data)
str(student_data)



student_data$Graduation<- factor(student_data$Graduation)

#Impute NA's in the dataset
student_data[, !names(student_data) %in% "Pay_in_INR"] <- sapply(student_data[, !names(student_data) %in% "Pay_in_INR"],function(x) ifelse(x=="",NA,x))
student_data_Imp<-centralImputation(student_data[, !names(student_data) %in% "Pay_in_INR"])
apply(student_data,2,pMiss_na)

student_data <- data.frame(cbind(student_data$Pay_in_INR,student_data_Imp))
student_data <- rename(student_data, c("student_data.Pay_in_INR"="Pay_in_INR"))
str(student_data)

# student_data$Year.of.Graduation.Completion <- factor(student_data$Year.of.Graduation.Completion)

# student_data$Score.in.nueroticism <- NULL

write.csv(x = student_data,file = "After_NA.csv")

str(student_data)
set.seed(12345)

sample = sample.split(student_data$Pay_in_INR, SplitRatio = 0.70)

train = subset(student_data, sample == TRUE)
test  = subset(student_data, sample == FALSE)

str(train)

rf <- randomForest(Pay_in_INR ~., data = train, importance = TRUE, ntree=100)

test_predicted <- predict(rf,actual_test)
write.csv(x = test_predicted,file = "predicted_val.csv")


which.min(rf$mse)
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
test.pred.forest <- predict(rf,test)

RMSE.forest <- sqrt(mean((test.pred.forest-test$Pay_in_INR)^2))
RMSE.forest
MAE.forest <- mean(abs(test.pred.forest-test$Pay_in_INR))
MAE.forest
# Create a data frame with the error metrics for each method
accuracy <- data.frame(
  Method = c("Random forest"),
           RMSE   = c(RMSE.forest),
           MAE    = c(MAE.forest)) 

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE.forest,2)
accuracy$MAE <- round(accuracy$MAE,2) 

test_predicted <- predict(rf,actual_test)
write.csv(x = test_predicted,file = "predicted_val.csv")
