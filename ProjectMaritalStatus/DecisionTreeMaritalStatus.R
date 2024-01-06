library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(skimr)
library(caTools)
library(e1071)

data <- read.csv("D:\\NSBM\\2nd Year\\Semester 1\\Data Programming in R\\Assignments\\Group Project\\HR_Analytics.csv")
str(data)

data$EmpID <- NULL
data$Over18 <- NULL
data$StandardHours <- NULL
data$EmployeeCount <- NULL
data$JobLevel <- NULL

split <- createDataPartition(y=data$MaritalStatus , p = 0.75, list = FALSE)
train <- data[split, ]
test <- data[-split, ]

dim(train)
dim(test)

set.seed(900)

dec_tree <- rpart(formula = MaritalStatus   ~.,
                  data = train,
                  method = "class",
                  xval = 10)

rpart.plot(dec_tree, yesno = TRUE)

naiveclassifier <- naiveBayes(MaritalStatus   ~ ., data=train)
predresults <- predict(naiveclassifier , newdata = test)

conftable <- table(predresults, test$MaritalStatus  )
conftable

confusionMatrix(conftable)
