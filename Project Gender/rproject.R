
library(dplyr)
library(ggplot2)
library(tidyverse)



data <- read.csv("C:\\Users\\NMC\\Downloads\\archive (4)\\HR_Analytics.csv")


print(data)



ggplot(data,aes(x=Gender))+
  geom_bar(fill="black")+
  
theme_bw()+
labs(x="Gender",
     y='',
     title="Gender difference")



Hr<-data
Hr
Hr %>% 
  filter(DailyRate>1000) %>% 
  select(EducationField,DailyRate) %>% 
  arrange(DailyRate)->edu_rate

print(edu_rate)
  library(dplyr)

Hr <- data 

Hr %>%
  select(Department, EducationField,Gender) %>%
  mutate(Department=recode(Department,
                           "Human Resources"="HR",
                           "Research & Development"="Re&Dev")) %>% 
  arrange(Department)->gender_edu 
  
print(gender_edu)
tail(gender_edu)
gender_edu %>%
  group_by(Department, Gender) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  facet_wrap(~Department) +
  coord_polar("y") +
  ggtitle("Gender Distribution ") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

gender_edu %>%
  group_by(Department, EducationField) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = EducationField)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  facet_wrap(~Department) +
  coord_polar("y") +
  ggtitle("Edu  Distribution ") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())


Hr<-data
Hr %>% 
  select(HourlyRate,Gender,JobRole) %>% 
 
  arrange(Gender)->hourly_rate_gender

print(hourly_rate_gender)

print(ggplot(hourly_rate_gender, aes(x = JobRole, y = HourlyRate, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge", width = 1.0) + 
        labs(title = "Hourly Rate Difference Between Genders ",
             x = "Job Role",
             y = "Hourly Rate") +
        theme_minimal())


#  evironment satisfaction

Hr<-data
Hr %>% 
  select(Gender,EnvironmentSatisfaction) %>% 
  arrange(Gender)->environment_satisfaction

print(environment_satisfaction)

ggplot(environment_satisfaction, aes(x = Gender, fill = factor(EnvironmentSatisfaction))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Environmental Satisfaction by Gender",
       x = "Gender",
       y = "Count",
       fill = "Environmental Satisfaction") +
  theme_minimal()


#Job_satisfacgion
Hr<-data
Hr %>% 
  select(Gender,JobSatisfaction) %>% 
  arrange(Gender)->Job_satisfacgion

print(Job_satisfacgion)

ggplot(Job_satisfacgion, aes(x = Gender, fill = factor(JobSatisfaction))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Job satisfaction by Gender",
       x = "Gender",
       y = "Count",
       fill = "Job _satisfaction") +
  theme_minimal()



#work life balance

Hr <- data
Hr %>% 
  select(Gender, WorkLifeBalance) %>% 
  arrange(Gender) -> work_lifebalance

print(work_lifebalance)

ggplot(work_lifebalance, aes(x = Gender, fill = factor(WorkLifeBalance))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Work-Life Balance by Gender",
       x = "Gender",
       y = "Count",
       fill = "Work-Life Balance") +
  theme_minimal()


#job enolement 

Hr <- data
Hr %>% 
  select(Gender, JobInvolvement) %>% 
  arrange(Gender) -> Job_involement

print(Job_involement)

ggplot(Job_involement, aes(x = Gender, fill = factor(JobInvolvement))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Job involment by Gender",
       x = "Gender",
       y = "Count",
       fill = "Job involement") +
  theme_minimal()



 library(ggplot2)


ggplot(Monthly_inome, aes(x = Gender, y = MonthlyIncome, color = Gender)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot for Monthly Income by Gender",
       x = "Gender",
       y = "Monthly Income",
       color = "Gender") +
  theme_minimal()


# Assuming you have the necessary libraries loaded, if not, install and load them:
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Filter data for females
female_data <- Monthly_inome %>% filter(Gender == "Female")

# Create a scatter plot for MonthlyIncome for females
ggplot(female_data, aes(x = Gender, y = MonthlyIncome, color = Gender)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot for Monthly Income (Female Only)",
       x = "Gender",
       y = "Monthly Income",
       color = "Gender") +
  theme_minimal()


library(ggplot2)
library(dplyr)


female_data <- Monthly_inome %>% filter(Gender == "Female")

ggplot(female_data, aes(x = Gender, y = MonthlyIncome, color = Gender)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot for Monthly Income (Female Only)",
       x = "Gender",
       y = "Monthly Income",
       color = "Gender") +
  theme_minimal()



male_data <- Monthly_inome %>% filter(Gender == "Male")

ggplot(male_data, aes(x = Gender, y = MonthlyIncome)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7, color = "blue") +
  labs(title = "Scatter Plot for Monthly Income (Males Only)",
       x = "Gender",
       y = "Monthly Income") +
  theme_minimal()

  
  
  

  
 #navi bayes classification for gender

data2 <- read.csv("C:\\Users\\NMC\\Downloads\\archive (4)\\HR_Analytics.csv")

str(data2)

data2$AgeGroup <- as.integer(factor(data2$AgeGroup))
data2$Attrition <- as.integer(factor(data2$Attrition))
data2$BusinessTravel <- as.integer(factor(data2$BusinessTravel))
data2$Department <- as.integer(factor(data2$Department))
data2$EducationField <- as.integer(factor(data2$EducationField))
data2$Gender <- as.integer(factor(data2$Gender))
data2$JobRole <- as.integer(factor(data2$JobRole))
data2$MaritalStatus <- as.integer(factor(data2$MaritalStatus))
data2$SalarySlab <- as.integer(factor(data2$SalarySlab))
data2$Over18 <- as.integer(factor(data2$Over18))
data2$OverTime <- as.integer(factor(data2$OverTime))
data2$JobLevel <- as.integer(factor(data2$JobLevel))  

str(data2)

sum(is.na(data2))
data2 <- na.omit(data2)
sum(is.na(data2))

str(data2)

library(caTools)

set.seed(150)
split <- sample.split(data2$Gender, SplitRatio = 0.75)
train <- subset(data2, split == TRUE)  
test <- subset(data2, split == FALSE) 

library(e1071)

classifier <- naiveBayes(Gender ~ ., data = train)
pre <- predict(classifier, newdata = test)

contable <- table(pre, test$Gender)
contable

library(caret)

confusionMatrix(contable)

#################################################

# Assuming you have loaded the required packages
install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)


data_cluster <- Hr %>%
  select(Gender, Age, YearsAtCompany)  


data_cluster$Gender <- as.numeric(factor(data_cluster$Gender))


set.seed(123)
kmeans_model <- kmeans(data_cluster[, 2:3], centers = 2)  


data_cluster$cluster <- as.factor(kmeans_model$cluster)


ggplot(data_cluster, aes(x = Age, y =YearsAtCompany , color = cluster)) +
  geom_point() +
  labs(title = "Gender Clustering",
       x = "Age",
       y = "years at the company",
       color = "Cluster") +
  theme_minimal()



#############################################################33

install.packages(c("dplyr", "cluster"))
library(dplyr)
library(cluster)


data_cluster <- Hr %>%
  select(Gender, Age, YearsInCurrentRole)  


data_cluster$Gender <- as.numeric(factor(data_cluster$Gender))


set.seed(123)
kmeans_model <- kmeans(data_cluster[, 2:3], centers = 2)  


data_cluster$cluster <- as.factor(kmeans_model$cluster)


clusplot(data_cluster[, 2:3], kmeans_model$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)





