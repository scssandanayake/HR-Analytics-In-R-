library(caTools)
library(e1071)
library(lattice)
library(caret)


data1 <- read.csv("D:\\NSBM\\2nd Year\\Semester 1\\Data Programming in R\\Assignments\\Group Project\\HR_Analytics.csv")

str(data1)

data1$AgeGroup <-as.integer(factor(data1$AgeGroup))
data1$Attrition <-as.integer(factor(data1$Attrition))
data1$BusinessTravel <-as.integer(factor(data1$BusinessTravel))
data1$Department <-as.integer(factor(data1$Department))
data1$EducationField <-as.integer(factor(data1$EducationField))
data1$Gender <-as.integer(factor(data1$Gender))
data1$JobRole <-as.integer(factor(data1$JobRole))
data1$MaritalStatus <-as.integer(factor(data1$MaritalStatus))
data1$SalarySlab <-as.integer(factor(data1$SalarySlab))
data1$Over18 <-as.integer(factor(data1$Over18))
data1$OverTime <-as.integer(factor(data1$OverTime))

str(data1)

data1 <- na.omit(data1)
sum(is.na(data1))

data1$EmpID <- NULL
data1$Over18 <- NULL

data$StandardHours <- NULL
data$EmployeeCount <- NULL
data$JobLevel <- NULL


set.seed(150)
split <- sample.split(data1$MaritalStatus, SplitRatio = 0.75)
train <- subset(data1, split == TRUE)
test  <- subset(data1, split == FALSE)


classifier <- naiveBayes ( MaritalStatus~., data = train)
pre <- predict(classifier, newdata = test)

contable <- table(pre,test$MaritalStatus)

confusionMatrix(contable)
