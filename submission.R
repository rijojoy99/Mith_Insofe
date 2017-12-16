
rm(list =ls(all= T))
setwd("E:\\My Libraries\\Documents\\Hadoop")

library(lubridate)
library(Smisc)
library(laeken)
library(DMwR)
library(car)
library(plyr)
library(caTools)
library(randomForest)
library(dummies)

student_data <- read.csv("train.csv",header = T , sep = ",")
actual_test <- read.csv("test.csv",header = T,sep=",")
str(student_data)
summary(student_data)

#Convert to NA's
# School.Board.in.Tenth , Score.in.Twelth => 0's
#Score.in.Domain => -1
pMiss_na <- function(x){sum(is.na(x))/length(x)*100}
pMiss_null <- function(x){sum(is.null(x))/length(x)*100}



# Found its more meaningfull to add this column into the dataset

student_data$Date.Of.Birth_1 <- as.character(student_data$Date.Of.Birth)
student_data$age <-  (as.numeric(student_data$Year.of.Graduation.Completion) -  as.numeric(year(lubridate::mdy(formatDT(c(student_data$Date.Of.Birth_1), posix = FALSE)$date))))
# View(student_data)
student_data$Date.Of.Birth_1 <- NULL
str(student_data)
# Convert to factors 
# ->CollegeCode,CollegeTier,Year.of.Graduation.Completion,CityCode,CityTier,Year.of.Graduation.Completion

student_data$Gender <- as.factor(student_data$Gender)
student_data$CollegeCode <- as.factor(student_data$CollegeCode)
student_data$CollegeTier <- as.factor(student_data$CollegeTier)
student_data$CityCode <- as.factor(student_data$CityCode) #,levels = levels(actual_test$CityCode))
student_data$CityTier <- as.factor(student_data$CityTier) #,levels = levels(actual_test$CityTier))
student_data$Year.of.Graduation.Completion <- as.factor(student_data$Year.of.Graduation.Completion)
student_data$Graduation<- as.factor(student_data$Graduation)
student_data$State  <- factor(student_data$State,levels = levels(actual_test$State))
student_data$Year.Of.Twelth.Completion <- factor(student_data$Year.Of.Twelth.Completion)

# Dropping off ID columns as they do not impact the Model development
#Age is calculated- removing DOB

student_data$Candidate.ID <- NULL
student_data$Date.Of.Birth <- NULL
student_data$CityCode <- NULL

# student_data$State <- NULL
str(student_data)
summary(student_data)

str(student_data)

col_list <- c( "Score.in.Tenth",
  "School.Board.in.Tenth",
  "Year.Of.Twelth.Completion",
  "Score.in.Twelth",
  "Board.in.Twelth",
  "GPA.Score.in.Graduation",
  "Score.in.English.language",
  "Score.in.Logical.skill",
  "Score.in.Quantitative.ability",
  "Score.in.ComputerProgramming",
  "Score.in.ElectronicsAndSemicon",
  "Score.in.ComputerScience",
  "Score.in.MechanicalEngg",
  "Score.in.ElectricalEngg",
  "Score.in.TelecomEngg",
  "Score.in.CivilEngg",
  "Score.in.conscientiousness",
  "Score.in.agreeableness",
  "Score.in.extraversion",
  "Score.in.nueroticism",
  "Score.in.openess_to_experience",
  "Score.in.Domain",
  "age"
)


str(student_data)
sum(is.na(student_data))
#Impute NA's in the dataset
var <- c("School.Board.in.Tenth","Score.in.Twelth")
student_data[,var] <- sapply(student_data[,var],function(x) ifelse(x=="0",NA,x))

var <- c("Score.in.Domain")
student_data[,var] <- sapply(student_data[,var],function(x) ifelse(x==-1,NA,x))

apply(student_data,2,pMiss_na)
apply(student_data,2,pMiss_null)
student_data[, names(student_data) %in% col_list] <- sapply(student_data[, names(student_data) %in% col_list],function(x) ifelse(x=="",NA,x))
student_data[, names(student_data) %in% col_list]<-knnImputation(student_data[, names(student_data) %in% col_list], k = 5)
apply(student_data,2,pMiss_na)


student_data$CollegeCode <- as.numeric(student_data$CollegeCode )
summary(student_data)

apply(student_data,2,pMiss_na)

str(student_data)
summary(student_data)
set.seed(12345)

sample = sample.split(student_data$Pay_in_INR, SplitRatio = 0.80)

train = subset(student_data, sample == TRUE)
test  = subset(student_data, sample == FALSE)

str(train)
sum(is.na(student_data))

rf <- randomForest(Pay_in_INR ~., data = student_data, importance = TRUE, ntree=400,
                   mtry = 6,keep.forest=TRUE, votes = T,
                   oob_score = TRUE,na.action=na.exclude)

which.min(rf$mse)
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
test.pred.forest <- predict(rf,test)
RMSE.forest <- sqrt(mean((test.pred.forest-test$Pay_in_INR)^2, na.rm = TRUE ))
RMSE.forest
MAE.forest <- mean(abs(test.pred.forest-test$Pay_in_INR))
MAE.forest

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

mape(test$Pay_in_INR,test.pred.forest)

varImpPlot(rf,
           sort = T,
           main="Variable Importance",
           n.var=20)

plot(rf)


test_predicted <- predict(rf,test)
# write.csv(x = test_predicted,file = "predicted_val_2.csv")



# Round the values and print the table

actual_test$Date.Of.Birth_1 <- as.character(actual_test$Date.Of.Birth)
actual_test$age <-  (as.numeric(actual_test$Year.of.Graduation.Completion) -  as.numeric(year(lubridate::mdy(formatDT(c(actual_test$Date.Of.Birth_1), posix = FALSE)$date))))
str(actual_test)


actual_test$CollegeCode <- as.factor(actual_test$CollegeCode)
actual_test$CollegeTier <- as.factor(actual_test$CollegeTier)
actual_test$Gender <- as.factor(actual_test$Gender)
actual_test$CityCode <- as.factor(actual_test$CityCode) #,levels = levels(actual_test$CityCode))
actual_test$CityTier <- as.factor(actual_test$CityTier) #,levels = levels(actual_test$CityTier))
actual_test$Year.of.Graduation.Completion <- as.factor(actual_test$Year.of.Graduation.Completion)
actual_test$Date.Of.Birth <- as.character(actual_test$Date.Of.Birth,levels = levels(actual_test$Date.Of.Birth))
actual_test$Graduation<- as.factor(actual_test$Graduation) #,levels = levels(actual_test$Graduation))
actual_test$State  <- factor(actual_test$State,levels = levels(actual_test$State))
actual_test$Year.Of.Twelth.Completion <- factor(actual_test$Year.Of.Twelth.Completion)

test_predicted <- roundpredict(rf,actual_test)
write.csv(x = test_predicted,file = "predicted_val_2.csv")
