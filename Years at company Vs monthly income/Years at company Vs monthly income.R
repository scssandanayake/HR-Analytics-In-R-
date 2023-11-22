library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
str(data)
view(data)

ggplot(data = data,aes(x=data$YearsAtCompany,y=data$MonthlyIncome))+geom_point()+geom_smooth(method = lm, se=FALSE)+
  labs(
    title = "years at company Vs.Monthly income",
    x = "years at company",
    y = "Monthly Income"
  )

hist(data$YearsAtCompany, 
     main = "years at company Vs. Number of employees",
     xlab = "years at company" ,
     ylab = "NO. of employees" ,
     col = "yellow")

hist(data$MonthlyIncome, 
     main = "Monthly income Vs. Number of employees",
     xlab = "Monthly Income" ,
     ylab = "NO. Of Employees" ,
     col = "orange")

YearsAtCompany_count <- table(data$YearsAtCompany)

barplot(YearsAtCompany_count, main = "Histogram - years at company Vs. Number of employees ",
        xlab = "years at company",
        ylab = "NO. Of Employees",
        col = "navyblue")

MonthlyIncome_count <- table(data$MonthlyIncome)

#this bar-plot is ugly
barplot(MonthlyIncome_count, main = "Histogram - Monthly income Vs. Number of employees ",
        xlab = "Monthly Income",
        ylab = "NO. Of Employees",
        col = "red")

pie(YearsAtCompany_count)

#this pie chart is ugly
pie(MonthlyIncome_count)
