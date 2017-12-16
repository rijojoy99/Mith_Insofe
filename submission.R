
rm(list =ls(all= T))
setwd("E:\\Myth_Insofe")

library(lubridate)
student_data <- read.csv("train.csv",header = T , sep = ",")

str(student_data)
summary(student_data)

View(student_data)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(student_data,2,pMiss)


# Dropping off ID columns as they do not impact the Model development
student_data$Candidate.ID<- NULL
str(student_data)
# student_data$age <- 
student_data$dob <- as.Date(substr(as.character(student_data$Date.Of.Birth), 1, 10),format = "%m/%d/%Y")
student_data$age <-  (as.numeric(student_data$Year.of.Graduation.Completion) -  as.numeric(lubridate::year(student_data$dob)))

str(student_data)
