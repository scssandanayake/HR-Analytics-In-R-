library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
#View(data)
#str(data)

#total working years (experience) vs Age

Experience <- table(data$TotalWorkingYears)
Experience

#section 1
#scatter plot total working years (experience) vs Age
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Age))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Total Working Years",
    y = "Age"
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

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Age))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= Age,
                 colour = AgeGroup))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Total Working Years",
    y = "Age"
  )

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Age))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size=TotalWorkingYears,
                 colour = TotalWorkingYears))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Total Working Years",
    y = "Age"
  )

#check bell curve
#scatter plot total working years (experience) vs Age
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Age))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth()+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Total Working Years",
    y = "Age"
  )

########################################################################################

#section 2
#age hist
hist(data$Age, 
     main = "Histrogram - Employee age Distribution",
     xlab = "Age Ranges" ,
     ylab = "NO. Of Employees" ,
     col = "orange")


age_count <- table(data$Age)
#age plot
barplot(age_count, main = "Barplot - Employee age Distribution ",
        xlab = "Age",
        ylab = "NO. Of Employees",
        col = "lightgreen")

########################################################################################

#section 3 (main insights)
#bar plot total working years (experience) vs Age with age group
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Age, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Age distribution with age group",
       x = "Total Working Years", y = "Age") +
  theme_minimal()

#bar plot total working years (experience) vs Age with Attrition
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Age, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Age distribution with Attrition",
       x = "Total Working Years", y = "Age") +
  theme_minimal()

###########################################

#bar plot total working years (experience) vs Age
ggplot(data=data, aes(x =TotalWorkingYears, y =Age)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Total working years Vs.Age distribution",
       x = "Total Working Years",
       y = "Age") +
  theme_minimal()    ### uncool 1000,2000.4000,6000 on age

#bar plot total working years (experience) vs Age
ggplot(data, aes(x = factor(TotalWorkingYears), fill = factor(Age))) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "count") +
  labs(title = "Bar plot - Total working years Vs.Age distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()


#histogram total working years (experience) vs Age
ggplot(data, aes(x = TotalWorkingYears, fill = factor(Age))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Histogram - Total working years Vs.Age distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()

#bar plots total working years (experience) of employees with age group
ggplot(data, aes(x = TotalWorkingYears)) +
  geom_bar() +
  facet_wrap(~ AgeGroup) +
  labs(title = "Barplots - Total working years of employees by age groups",
       x = "Total Working Year",
       y = "Employee count") +
  theme(strip.background = element_blank())

##########################################################################################

#section 4
#boxplot
data %>%
  ggplot(aes(TotalWorkingYears,Age))+
  geom_boxplot()+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= Age,
                 colour = AgeGroup))+
  #facet_wrap(~Type)+
  #coord_flip()+                     #rotate the box plot
  theme_bw()+
  labs(title = "Boxplot Comparison of total working years and age")


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

#####################################################################################################
#end 
