library(caret)
library(rpart.plot)
library(tidyverse)                #workflow
library(skimr)
library(caTools)


#setting dataset

dataset <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\ProjectAge\\dataAssignment.csv")
str(dataset)

dataset <- dataset[, !names(dataset) %in% c("EmpID")]
dataset <- dataset[, !names(dataset) %in% c("JobRole")] # by this we can  increase the accuracy of this analysis
#dataset <- dataset[, !names(data2) %in% c("AgeGroup")]

#training data & testing data

split <- createDataPartition(y=dataset$Attrition , p = 0.75, list = FALSE)
train <- dataset[split, ]
test <- dataset[-split, ]

#split <- sample.split(dataset$Purchase, SplitRatio = 0.75)
#train <- subset(dataset, split == TRUE)
#test <- subset(dataset, split == FALSE)

dim(train)
dim(test)

#the classifier
set.seed(150)

dec_tree <- rpart(formula = Attrition   ~.,
                  data = train,
                  method = "class",
                  xval = 10)

# drawing the tree
rpart.plot(dec_tree, yesno = TRUE)

library(e1071)

naiveclassifier <- naiveBayes(Attrition   ~ ., data=train)
predresults <- predict(naiveclassifier , newdata = test)

#the confusion matrix

conftable <- table(predresults, test$Attrition  )
conftable

confusionMatrix(conftable)

