# Load the library
library(ggplot2)
library(dplyr)
library(tidyverse)

#loading the dataset
data <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\ProjectAge\\dataAssignment.csv")

#Bar plot

AgeGroup <- data$AgeGroup


ggplot(data, aes(x = AgeGroup, y = EmployeeCount , fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Employees by Age Group", x = "Age Groups", y = "Employee Count") +
  theme_bw()+ theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Set title to bold and centered
    axis.text.x = element_text(face = "bold"), # Set x-axis labels to bold
    axis.text.y = element_text(face = "bold")
  )


data$BusinessTravel[data$BusinessTravel == "TravelRarely"] <- "Travel_Rarely"

ggplot(data, aes(x = AgeGroup, y = MonthlyIncome, fill = BusinessTravel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Income by Age Group and Business Travel Type",
       x = "Age Group", y = "Monthly Income") +
  theme_bw() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Set title to bold and centered
    axis.text.x = element_text(face = "bold"), # Set x-axis labels to bold
    axis.text.y = element_text(face = "bold")
  )

#Mosaic Plot

Department <- data$Department

ggplot(data, aes(x = data$AgeGroup, fill = Department)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Departments in Age Groups", x = "Age Group", y = "Proportion") +
  theme_bw()+ theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Set title to bold and centered
    axis.text.x = element_text(face = "bold"), # Set x-axis labels to bold
    axis.text.y = element_text(face = "bold")
  )

#Violin Plot

ggplot(data, aes(x = data$AgeGroup, y = data$JobLevel, fill = data$AgeGroup)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Job Levels Across Age Groups", x = "Age group", y = "Job Level") +
  scale_fill_discrete(guide = FALSE) + # Removing the legend
  theme_bw() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Set title to bold and centered
    axis.text.x = element_text(face = "bold"), # Set x-axis labels to bold
    axis.text.y = element_text(face = "bold")
  )

# swarm-like plot

ggplot(data, aes(x = data$AgeGroup, y = data$DistanceFromHome , color = data$AgeGroup)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  labs(title = "Distance from Home Across Age Groups", x = "Age Group", y = "Distance From Home") +
  scale_color_discrete(guide = FALSE) + # Removing the legend
  theme_bw()+ theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Set title to bold and centered
    axis.text.x = element_text(face = "bold"), # Set x-axis labels to bold
    axis.text.y = element_text(face = "bold")
  )
















