library(ggplot2)
library(dplyr)
data <- read.csv("D:\\N learn\\2nd year\\projects\\R project\\HR_Analytics.csv")
#View(data)

#overall summery visualization of total working years / experience
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

  
  



