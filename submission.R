
rm(list =ls(all= T))
# setwd("E:\\Myth_Insofe")

library(lubridate)
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


# Dropping off ID columns as they do not impact the Model development
student_data$Candidate.ID<- NULL
str(student_data)
summary(student_data)
# student_data$age <- 
student_data$dob <- as.Date(substr(as.character(student_data$Date.Of.Birth), 1, 10),format = "%m/%d/%Y")

student_data$age <-  (as.numeric(student_data$Year.of.Graduation.Completion) -  as.numeric(lubridate::year(student_data$dob)))

student_data$Date.Of.Birth <- NULL
str(student_data)


# Convert to factors 
# ->CollegeCode,CollegeTier,Year.of.Graduation.Completion,CityCode,CityTier,Year.of.Graduation.Completion

student_data$CollegeCode <- as.character(student_data$CollegeCode)
student_data$CollegeTier <- as.character(student_data$CollegeTier)
student_data$Year.of.Graduation.Completion <- as.character(student_data$Year.of.Graduation.Completion)
student_data$CityCode <- as.character(student_data$CityCode)
student_data$CityTier <- as.character(student_data$CityTier)
student_data$Year.of.Graduation.Completion <- as.character(student_data$Year.of.Graduation.Completion)
summary(student_data)
student_data$dob<- NULL
write.csv(x = student_data,file = "After_NA.csv")


