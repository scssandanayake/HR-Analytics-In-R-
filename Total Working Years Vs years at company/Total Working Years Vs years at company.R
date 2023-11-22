library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
str(data)
view(data)

ggplot(data = data,aes(x=data$YearsAtCompany,y=data$TotalWorkingYears))+geom_point()+geom_smooth(method = lm, se=FALSE)+
  labs(
    title = "years at company Vs.Total Working Years",
    x = "years at company",
    y = "Total Working Years"
  )

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$YearsAtCompany))+geom_point()+geom_smooth(method = lm, se=FALSE)+
  labs(
    title = "Total Working Years Vs. years at company",
    x = "Total Working Years",
    y = "years at company"
  )

hist(data$YearsAtCompany, 
     main = "years at company Vs. Number of employees",
     xlab = "years at company" ,
     ylab = "NO. of employees" ,
     col = "lightgreen")

hist(data$TotalWorkingYears, 
     main = "Total Working Years Vs. Number of employees",
     xlab = "Monthly Income" ,
     ylab = "NO. Of Employees" ,
     col = "lightblue")

YearsAtCompany_count <- table(data$YearsAtCompany)

barplot(YearsAtCompany_count, main = "Histogram - years at company Vs. Number of employees ",
        xlab = "years at company",
        ylab = "NO. Of Employees",
        col = "navyblue")

TotalWorkingYears_count <- table(data$TotalWorkingYears)

barplot(TotalWorkingYears_count, main = "Histogram - Monthly income Vs. Number of employees ",
        xlab = "Monthly Income",
        ylab = "NO. Of Employees",
        col = "red")

pie(YearsAtCompany_count)

#this pie chart is ugly
pie(TotalWorkingYears_count)