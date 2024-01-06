library(ggplot2)
library(dplyr)
library(tidyverse)

data <- read.csv("D:\\NSBM\\2nd Year\\Semester 1\\Data Programming in R\\Assignments\\Group Project\\HR_Analytics.csv")

View(data)
str(data)

#ENCODING CATEGORICAL VARIABLE

#data$MaritalStatus = factor(data$MaritalStatus, level = c("Single","Divorced","Married"),
 #                           labels = c("S","D","M"))

#MStatus_mapping <- c("S"=0,"D"=1,"M"=2)

#CONVERT THE FACTOR VARIABLE TO NUMERIC USING THE MAPPING

#data$NumericMStatus <- MStatus_mapping[data$MaritalStatus]

#str(data)


data %>%
  drop_na(MaritalStatus) %>%  #getting rid of missing data
  ggplot(aes(fct_infreq(MaritalStatus), fill = MaritalStatus))+
  geom_bar(position= "dodge", alpha = 0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Marital Status", y = "No. of Employees")+
  guides(fill = FALSE)

percentage_table <- prop.table(table(data$MaritalStatus)) * 100
view(percentage_table)

#####

data %>%
  ggplot(aes(x = factor(MaritalStatus), y = Age, fill = factor(AgeGroup))) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot comparison of Marital Status and Age",
       x = "Marital Status",
       y = "Age") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
    )

#####

data %>%
ggplot(aes(x = MaritalStatus, fill = Gender)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Marital Status based on Gender",
       x = "Marital Status",
       y = "No. of Employees")+
    theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5)
    )+
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

#####

ggplot(data, aes(x=DistanceFromHome, fill = factor(MaritalStatus))) +
  geom_density() +
  facet_grid(MaritalStatus ~ .)

#####

data %>%
ggplot(aes(x = MaritalStatus, y = RelationshipSatisfaction, fill = Gender)) +
  geom_violin(draw_quantiles = TRUE, alpha = 0.8) +
  labs(title = "Relationship Satisfaction based on Marital Status and Gender",
       x = "Marital Staus",
       y = "Relationship Satisfaction Level")+
  scale_fill_manual(values = c("pink","skyblue")) +
  theme_bw()+
theme(
  plot.title = element_text(hjust = 0.5)
)

#####

# Assuming 'data' contains the necessary columns

data %>%
  ggplot(aes(x = RelationshipSatisfaction, fill= Gender)) +
  geom_bar(position = "dodge", alpha = 0.6) +
  facet_wrap(~ AgeGroup, scales = "free_y", ncol = 2) +  # Customize the number of columns and scales
  labs(title = "Relationship Satisfaction with different Age Groups",
       x = "Relationship Satisfaction Level",
       y = "Employee Count") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 9),  # Customize facet label appearance
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )+
  scale_fill_brewer(palette = "Dark2")
