library(randomForest)

data1 <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\ProjectAge\\dataAssignment.csv")

str(data1)

Age_group <- table(data1$AgeGroup)

Age_group


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

data2 <- data1[, !names(data1) %in% c("EmpID")]
data2 <- data1[, !names(data1) %in% c("Age")]

sum(is.na(data2))
data2 <- na.omit(data2)
sum(is.na(data2))

set.seed(222)
ind <- sample(2, nrow(data2), replace = TRUE, prob = c(0.7, 0.3))
train <- data2[ind==1,]
test <- data2[ind==2,]

rf <- randomForest(AgeGroup~., data=train, proximity=TRUE) 
print(rf)
randomForest(formula = data2$AgeGroup ~ ., data = train)

# Check the data type of AgeGroup
class(train$AgeGroup)

# Check unique levels and their counts
table(train$AgeGroup)

# Check for non-numeric values in AgeGroup
non_numeric_AgeGroup <- train$AgeGroup[!is.numeric(train$AgeGroup)]
non_numeric_AgeGroup
str(data2)

plot(rf)

library(mlbench)
library(caTools)
library(caret)
library(e1071)
library(naivebayes)


p1 <- predict(rf, train$AgeGroup)
accuracy_without_preprocessing <- mean(p1 == train$AgeGroup)
accuracy_without_preprocessing

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
MeanDecreaseGini

