library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
str(data)
view(data)

ggplot(data = data,aes(x=data$TotalWorkingYears,y=data$MonthlyIncome))+geom_point()+geom_smooth(method = lm, se=FALSE)+
  labs(
  title = "Total working years Vs.Monthly income",
  x = "Total Working Years",
  y = "Monthly Income"
)

hist(data$TotalWorkingYears, 
     main = "Total working years Vs. Number of employees",
     xlab = "Total Working Years" ,
     ylab = "NO. of employees" ,
     col = "green")

hist(data$MonthlyIncome, 
     main = "Monthly income Vs. Number of employees",
     xlab = "Monthly Income" ,
     ylab = "NO. Of Employees" ,
     col = "red")

TotalWorkingYears_count <- table(data$TotalWorkingYears)

barplot(TotalWorkingYears_count, main = "Histogram - Total working years Vs. Number of employees ",
        xlab = "Total Working Years",
        ylab = "NO. Of Employees",
        col = "skyblue")

MonthlyIncome_count <- table(data$MonthlyIncome)

#this bar-plot is ugly
barplot(MonthlyIncome_count, main = "Histogram - Monthly income Vs. Number of employees ",
        xlab = "Monthly Income",
        ylab = "NO. Of Employees",
        col = "green")

pie(TotalWorkingYears_count)

#this pie chart is ugly
pie(MonthlyIncome_count)

