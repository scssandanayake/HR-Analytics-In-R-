library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
#View(data)
#str(data) 

#total working years (experience) vs JobLevel

Experience <- table(data$TotalWorkingYears)
Experience

#section 1
#scatter plot total working years (experience) vs JobLevel
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Education))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth(method =lm , se=)+
  labs(
    title = "Total working years Vs.Education Level distribution",
    x = "Total Working Years",
    y = "Education Level"
  )

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Education))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size=Education,
                 colour = EducationField))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Education Level distribution with educatuion field",
    x = "Total Working Years",
    y = "Education Level"
  )


ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Education))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= Age,
                 colour = AgeGroup))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Education Level distribution with age groups",
    x = "Total Working Years",
    y = "Education Level"
  )

#check bell curve
#scatter plot total working years (experience) vs JobLevel
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$Education))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth()+
  labs(
    title = "Total working years Vs.Education Level distribution",
    x = "Total Working Years",
    y = "Education Level"
  )



########################################################################################

#section 2
#age hist
hist(data$Education, 
     main = "Histrogram - Employee Education Level Distribution",
     xlab = "Education Level" ,
     ylab = "NO. Of Employees" ,
     col = "lightblue")


job_level <- table(data$Education)
#age plot
barplot(job_level, main = "Barplot - Employee Education Level Distribution ",
        xlab = "Education Level",
        ylab = "NO. Of Employees",
        col = "lightgreen")

########################################################################################

#section 3 (main insights)
#bar plot total working years (experience) vs Education Level with Job Satisfaction
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Job Satisfaction",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()

#bar plot total working years (experience) vs Education Level with Marital Status
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = MaritalStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Marital Status",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()

#bar plot total working years (experience) vs Education Level with Monthly Income
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = MonthlyIncome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Monthly Income",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()

#bar plot total working years (experience) vs Education Level with Salary Slab
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = SalarySlab)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Salary Slab",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()

#bar plot total working years (experience) vs Education Level with Performance Rating
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = PerformanceRating)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Performance Rating",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()

#bar plot total working years (experience) vs Education Level with Gender
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$Education, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs. Education Level with Gender",
       x = "Total Working Years", y = "Education Level") +
  theme_minimal()                #############   me tika hadanna thiyenawa

############################################

#bar plot total working years (experience) vs Education Level
ggplot(data=data, aes(x =TotalWorkingYears, y =Education)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Total working years Vs.Education Level distribution",
       x = "Total Working Years",
       y = "Education Level") +
  theme_minimal()  ### uncool 1000,2000.4000,6000 on age

#bar plot total working years (experience) vs Education Level
ggplot(data, aes(x = factor(TotalWorkingYears), fill = factor(Education))) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "count") +
  labs(title = "Bar plot - Total working years Vs.Education Level distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()


#histogram total working years (experience) vs Education Level
ggplot(data, aes(x = TotalWorkingYears, fill = factor(Education))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Histogram - Total working years Vs.Education Level distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()

#bar plots total working years (experience) of employees with Education Level
ggplot(data, aes(x = TotalWorkingYears)) +
  geom_bar() +
  facet_wrap(~ Education) +
  labs(title = "Barplots - Total working years of employees by Education levels",
       x = "Total Working Year",
       y = "No. of employees") +
  theme(strip.background = element_blank())

##########################################################################################

#section 4
#boxplot Job Level
data %>%
  ggplot(aes(TotalWorkingYears,Education))+
  geom_boxplot()+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= 2,
                 colour = EducationField))+
  #facet_wrap(~Type)+
  #coord_flip()+                     #rotate the box plot
  theme_bw()+
  labs(title = "Boxplot Comparison of total working years and Education Level")



#Boxplots for all Education Levels with age distribution
ggplot(data, aes(x = factor(Education), y =TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for all Education Levels with age distribution
ggplot(data, aes(x = factor(Education), y =TotalWorkingYears, fill = factor(Gender))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Education Levels in <=25 age groups
data %>%
  #filter(Age<=25) %>%
  ggplot(aes(x = factor(Education), y =TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.6,fill= "red") +
  geom_point(alpha = 0.8,size = 3)+
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Education Levels in 25-35 age groups 
data %>%
  #filter(Age > 25 & Age <= 35) %>%
  ggplot(aes(x = factor(Education), y = TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.7,fill= "orange") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Education Levels in 35-45 age groups 
data %>%
  #filter(Age > 35 & Age <= 45) %>%
  ggplot(aes(x = factor(Education), y = TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.5,fill= "green") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Education Levels in 45-55 age groups 
data %>%
  #filter(Age > 45 & Age <= 55) %>%
  ggplot(aes(x = factor(Education), y = TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.7,fill= "skyblue") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Education Levels in 55-60 age groups 
data %>%
  #filter(Age > 55 & Age <= 60) %>%
  ggplot(aes(x = factor(Education), y = TotalWorkingYears, fill = factor(EducationField))) +
  geom_boxplot(alpha = 0.7,fill= "pink") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Education Level and total working years",
       x = "Education Levels",
       y = "Total Working Year") +
  theme_minimal()

#####################################################################################################
#end end end 
