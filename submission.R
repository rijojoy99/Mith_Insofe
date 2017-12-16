
rm(list =ls(all= T))
setwd("E:\\Myth_Insofe")

student_data <- read.csv("train.csv",header = T , sep = ",")

str(student_data)
summary(student_data)

View(student_data)
