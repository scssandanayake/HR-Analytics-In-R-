# Data Visualization part starts from here

library(ggplot2)
library(dplyr)
library(tidyverse)

data <- read.csv("D:\\NSBM\\2nd Year\\Semester 1\\Data Programming in R\\Assignments\\Group Project\\HR_Analytics.csv")

View(data)
str(data)

# ENCODING CATEGORICAL VARIABLE (NOT USED)

#data$MaritalStatus = factor(data$MaritalStatus, level = c("Single","Divorced","Married"),
#                           labels = c("S","D","M"))

#MStatus_mapping <- c("S"=0,"D"=1,"M"=2)

# CONVERT THE FACTOR VARIABLE TO NUMERIC USING THE MAPPING

#data$NumericMStatus <- MStatus_mapping[data$MaritalStatus]


#####

data %>%
  ggplot(aes(x = factor(MaritalStatus), y = Age, fill = factor(AgeGroup))) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot comparison of Marital Status and Age",
       x = "Marital Status",
       y = "Age") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

#####

data %>%
  ggplot(aes(x = MaritalStatus, fill = Gender)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Marital Status based on Gender",
       x = "Marital Status",
       y = "No. of Employees")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

##### 

ggplot(data, aes(x=DistanceFromHome, fill = factor(MaritalStatus))) +
  geom_density() +
  facet_grid(MaritalStatus ~ .)

#####

data %>%
  ggplot(aes(x = MaritalStatus, y = RelationshipSatisfaction, fill = Gender)) +
  geom_violin(draw_quantiles = TRUE, alpha = 0.8) +
  labs(title = "Relationship Satisfaction based on Marital Status and Gender",
       x = "Marital Staus",
       y = "Relationship Satisfaction Level")+
  scale_fill_manual(values = c("pink","skyblue")) +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#####

# Assuming 'data' contains the necessary columns

data %>%
  ggplot(aes(x = RelationshipSatisfaction, fill= Gender)) +
  geom_bar(position = "dodge", alpha = 0.6) +
  facet_wrap(~ AgeGroup, scales = "free_y", ncol = 2) +  # Customize the number of columns and scales
  labs(title = "Relationship Satisfaction with different Age Groups",
       x = "Relationship Satisfaction Level",
       y = "Employee Count") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 9),  # Customize facet label appearance
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )+
  scale_fill_brewer(palette = "Dark2")

#####

# Data Visualization part ends here

#####

# Data Analytics through Models part starts from here

#####

# Naive Bayes' Classifier for Marital Status

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

#####

# Decison Tree Classification for Marital Status

#####

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

#####

# Data Analytics through models part ends here