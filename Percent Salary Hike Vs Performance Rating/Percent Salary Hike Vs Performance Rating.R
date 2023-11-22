library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
str(data)
view(data)

ggplot(data = data,aes(x=data$PercentSalaryHike,y=data$PerformanceRating))+geom_point()+geom_smooth(method = lm, se=FALSE)+
  labs(
    title = "Percent Salary Hike Vs.Performance Rating",
    x = "Percent Salary Hike",
    y = "Performance Rating"
  )

hist(data$PercentSalaryHike, 
     main = "Percent Salary Hike Vs. Number of employees",
     xlab = "Percent Salary Hike" ,
     ylab = "NO. of employees" ,
     col = "maroon")

hist(data$PerformanceRating, 
     main = "Performance Rating Vs. Number of employees",
     xlab = "Performance Rating" ,
     ylab = "NO. Of Employees" ,
     col = "orange")

PercentSalaryHike_count <- table(data$PercentSalaryHike)

barplot(PercentSalaryHike_count, main = "Histogram - Percent Salary Hike Vs. Number of employees ",
        xlab = "Percent Salary Hike",
        ylab = "NO. Of Employees",
        col = "lightgreen")

PerformanceRating_count <- table(data$PerformanceRating)

barplot(PerformanceRating_count, main = "Histogram - Performance Rating Vs. Number of employees ",
        xlab = "Performance Rating",
        ylab = "NO. Of Employees",
        col = "lightblue")

pie(PercentSalaryHike_count)

pie(PerformanceRating_count)