library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
#View(data)
#str(data)

########## VISUALIZATION ################
#total working years (experience) vs Age

Experience <- table(data$TotalWorkingYears)
Experience

#bar plot
barplot(Experience,
        main = "Barplot - Employee total working years Distribution",
        xlab = "Total working years",
        ylab = "Number of Employees",
        col = "skyblue")

#histogram
hist(data$TotalWorkingYears, 
     main = "Histrogram - Employee total working years Distribution",
     xlab = "Total working years" ,
     ylab = "Number of Employees" ,
     col = "lightgreen")


#pie chart
pie(Experience)

#box plot 1
boxplot(data$TotalWorkingYears, 
        main="Employee total working years Distribution",
        xlab="Total working years",
        ylab="Employees",
        col = "purple",
        border = "black",
        notch = TRUE,
        horizontal= TRUE
)

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Age))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= Age,
                 colour = Age))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Total Working Years",
    y = "Age"
  )

#Boxplots for all age groups with age distribution
ggplot(data, aes(x = factor(Age), y =TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for <=25 age groups with age distribution
data %>%
  filter(Age<=25) %>% 
  ggplot(aes(x = factor(Age), y =TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.6,fill= "red") +
  geom_point(alpha = 0.8,size = 3)+
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for 25-35 age groups with age distribution
data %>%
  filter(Age > 25 & Age <= 35) %>%
  ggplot(aes(x = factor(Age), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "orange") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for 35-45 age groups with age distribution
data %>%
  filter(Age > 35 & Age <= 45) %>%
  ggplot(aes(x = factor(Age), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.5,fill= "green") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for 45-55 age groups with age distribution
data %>%
  filter(Age > 45 & Age <= 55) %>%
  ggplot(aes(x = factor(Age), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "skyblue") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for 55-60 age groups with age distribution
data %>%
  filter(Age > 55 & Age <= 60) %>%
  ggplot(aes(x = factor(Age), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "pink") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

#bar plot total working years (experience) vs Age with Attrition
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Age, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Age distribution with Attrition",
       x = "Total Working Years", y = "Age") +
  theme_minimal()

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$JobLevel))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= Age,
                 colour = AgeGroup))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Job level distribution with age groups",
    x = "Total Working Years",
    y = "Job Level"
  )

#section 3 (main insights)
#bar plot total working years (experience) vs Job Level with Job Satisfaction
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Job Satisfaction",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Monthly Income
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = MonthlyIncome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Monthly Income",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#Boxplots for all Job Levels with age distribution
ggplot(data, aes(x = factor(TotalWorkingYears), y =JobLevel, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Total Working Year",
       y = "Job Level") +
  theme_minimal()

############### LOGISTIC REGRESSION MODEL ################################

library(caTools)
library(ROCR)
library(dplyr)
library(caret)

dataset <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
summary(dataset)

#data cleaning and pre-processing
sum(is.na(dataset))
dataset <- na.omit(dataset)

class(dataset$Attrition)
unique(dataset$Attrition)
dataset <- dataset %>%
  mutate(Attrition = ifelse(Attrition == "No",0,1))
str(dataset)

# Splitting dataset
set.seed(123)
split <- sample.split(dataset, SplitRatio = 0.7) # 30/70
split

train_reg <- subset(dataset, split == "TRUE")
test_reg <- subset(dataset, split == "FALSE")

# Training model
logistic_model <- glm(Attrition ~ JobLevel + TotalWorkingYears,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

predict_reg <- predict(logistic_model,
                       test_reg, type = "response")
predict_reg


# Changing probabilities for prediction
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Keep probabilities for ROC curve
predict1_reg <- predict(logistic_model, test_reg, type = "response")


# Evaluating model accuracy using confusion matrix
confusion_matrix <-table(test_reg$Attrition, predict_reg)
print(confusion_matrix)

missing_classerr <- mean(predict_reg != test_reg$Attrition)
print(paste('Accuracy =', 1 - missing_classerr))

contable <- confusionMatrix(as.factor(predict_reg), as.factor(test_reg$Attrition))
print(contable)


# ROC-AUC Curve
ROCPred <- prediction(predict1_reg, test_reg$Attrition)
ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)




  
  



