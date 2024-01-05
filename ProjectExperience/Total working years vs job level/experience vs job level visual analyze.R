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
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$JobLevel))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Job level distribution",
    x = "Total Working Years",
    y = "Job Level"
  )

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$JobLevel))+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size=JobLevel,
                 colour = JobLevel))+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Job level distribution",
    x = "Total Working Years",
    y = "Job Level"
  )
  
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

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$JobLevel))+
    geom_point(alpha = 0.7,            #here we mapping the geom points only
               aes(size=TotalWorkingYears,
                   colour = TotalWorkingYears))+
    geom_smooth(method = lm, se=F)+
    labs(
      title = "Total working years Vs.Job level distribution",
      x = "Total Working Years",
      y = "Job Level"
    )


#check bell curve
#scatter plot total working years (experience) vs JobLevel
ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$JobLevel))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth()+
  labs(
    title = "Total working years Vs.Job Level distribution",
    x = "Total Working Years",
    y = "Job Level"
  )

################################################################

#scatter plot  Job Level vs total working years (experience)
ggplot(data = data,aes(x=data$JobLevel,y=data$TotalWorkingYears))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth(method = lm, se=F)+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Job Level",
    y = "Total Working Years"
  )

#check bell curve
#scatter plot  Job Level vs total working years (experience)
ggplot(data = data,aes(x=data$JobLevel,y=data$TotalWorkingYears))+
  geom_point(size = 2,colour = "red",alpha = 0.5)+
  geom_smooth()+
  labs(
    title = "Total working years Vs.Age distribution",
    x = "Job Level",
    y = "Total Working Years"
  )


########################################################################################

#section 2
#age hist
hist(data$JobLevel, 
     main = "Histrogram - Employee Job Level Distribution",
     xlab = "Job Level" ,
     ylab = "NO. Of Employees" ,
     col = "lightpink")


job_level <- table(data$JobLevel)
#age plot
barplot(job_level, main = "Barplot - Employee Job Level Distribution ",
        xlab = "Job Level",
        ylab = "NO. Of Employees",
        col = "lightgreen")

########################################################################################

#section 3 (main insights)
#bar plot total working years (experience) vs Job Level with Job Satisfaction
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Job Satisfaction",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Marital Status
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = MaritalStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Marital Status",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Monthly Income
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = MonthlyIncome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Monthly Income",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Salary Slab
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = SalarySlab)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Salary Slab",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Performance Rating
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = PerformanceRating)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Performance Rating",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

#bar plot total working years (experience) vs Job Level with Gender
ggplot(data = data, aes(x =data$TotalWorkingYears, y = data$JobLevel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total working years Vs.Job Level with Gender",
       x = "Total Working Years", y = "Job Level") +
  theme_minimal()

############################################

#bar plot total working years (experience) vs Job Level
ggplot(data=data, aes(x =TotalWorkingYears, y =JobLevel)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Total working years Vs.Job Level distribution",
       x = "Total Working Years",
       y = "Job Level") +
  theme_minimal()  ### uncool 1000,2000.4000,6000 on age

#bar plot total working years (experience) vs Job Level
ggplot(data, aes(x = factor(TotalWorkingYears), fill = factor(JobLevel))) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "count") +
  labs(title = "Bar plot - Total working years Vs.Job Level distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()


#histogram total working years (experience) vs Job Level
ggplot(data, aes(x = TotalWorkingYears, fill = factor(JobLevel))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Histogram - Total working years Vs.Job Level distribution",
       x = "Total Working Year",
       y = "No. of employees") +
  theme_minimal()

#bar plots total working years (experience) of employees with Job Level
ggplot(data, aes(x = TotalWorkingYears)) +
  geom_bar() +
  facet_wrap(~ JobLevel) +
  labs(title = "Barplots - Total working years of employees by age groups",
       x = "Total Working Year",
       y = "No. of employees") +
  theme(strip.background = element_blank())

##########################################################################################

#section 4
#boxplot Job Level
data %>%
  ggplot(aes(TotalWorkingYears,JobLevel))+
  geom_boxplot()+
  geom_point(alpha = 0.7,            #here we mapping the geom points only
             aes(size= JobLevel,
                 colour = AgeGroup))+
  #facet_wrap(~Type)+
  #coord_flip()+                     #rotate the box plot
  theme_bw()+
  labs(title = "Boxplot Comparison of total working years and Job Level")

#Boxplots for all Job Levels with age distribution
ggplot(data, aes(x = factor(TotalWorkingYears), y =JobLevel, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Total Working Year",
       y = "Job Level") +
  theme_minimal()


#Boxplots for all Job Levels with age distribution
ggplot(data, aes(x = factor(JobLevel), y =TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for all Job Levels with age distribution
ggplot(data, aes(x = factor(JobLevel), y =TotalWorkingYears, fill = factor(Gender))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(alpha = 0.7)+
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Job Levels in <=25 age groups
data %>%
  filter(Age<=25) %>% 
  ggplot(aes(x = factor(JobLevel), y =TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.6,fill= "red") +
  geom_point(alpha = 0.8,size = 3)+
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Job Levels in 25-35 age groups 
data %>%
  filter(Age > 25 & Age <= 35) %>%
  ggplot(aes(x = factor(JobLevel), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "orange") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Job Levels in 35-45 age groups 
data %>%
  filter(Age > 35 & Age <= 45) %>%
  ggplot(aes(x = factor(JobLevel), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.5,fill= "green") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Job Levels in 45-55 age groups 
data %>%
  filter(Age > 45 & Age <= 55) %>%
  ggplot(aes(x = factor(JobLevel), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "skyblue") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Level",
       y = "Total Working Year") +
  theme_minimal()

#Boxplots for Job Levels in 55-60 age groups 
data %>%
  filter(Age > 55 & Age <= 60) %>%
  ggplot(aes(x = factor(JobLevel), y = TotalWorkingYears, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "pink") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of Job Level and total working years",
       x = "Job Levels",
       y = "Total Working Year") +
  theme_minimal()

#####################################################################################################
#end end end 









#testing testing

#Boxplots for 55-60 age groups with age distribution
data %>%
  filter(Age > 55 & Age <= 60) %>%
  ggplot(aes(x = factor(TotalWorkingYears), y = JobLevel, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "purple") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()

data %>%
  filter(Age > 45 & Age <= 55) %>%
  ggplot(aes(x = factor(TotalWorkingYears), y = Age, fill = factor(AgeGroup))) +
  geom_boxplot(alpha = 0.7,fill= "purple") +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "Boxplot Comparison of age and total working years",
       x = "Age",
       y = "Total Working Year") +
  theme_minimal()









mpg %>%
  filter(cty<25) %>%              #filtering the city variable values less than 25 and piping it 
  ggplot(aes(displ,cty))+
  geom_point(alpha = 0.7,
             aes(colour = drv,
                 size = trans))+
  geom_smooth(method = lm,se =F)+
  geom_smooth(method = lm)+
  #geom_smooth()+
  facet_wrap(~year, nrow=1)+
  labs(x= "Engine size",
       y= "MPG in the city",
       title= "fuel effciency")+
  theme_bw()
