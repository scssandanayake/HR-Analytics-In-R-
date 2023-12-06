data <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\Project\\dataAssignment.csv")

library(ggplot2)


Age_group <- table(data$AgeGroup)
Age_group

bt <- table(data$BusinessTravel)
bt

bt

ggplot(aes(x=Age_group))

barplot(Age_group,
        main = "Histrogram - Employee Age Distribution",
        xlab = "Age Group",
        ylab = "Number of Employees",
        col = "brown")




data$BusinessTravel[data$BusinessTravel == "TravelRarely"] <- "Travel_Rarely"

ggplot(data, aes(x = AgeGroup, y = MonthlyIncome, fill = BusinessTravel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Income by Age Group and Business Travel Type",
       x = "Age Group", y = "Monthly Income") +
  theme_minimal()


boxplot(data$Age, 
        main="Age of the Employees",
        xlab="Age",
        ylab="Employees",
        col = "red",
        border = "black",
        notch = TRUE,
        horizontal= TRUE
)


ggplot(data, aes(x = AgeGroup, y = NumCompaniesWorked)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(title = "Number of Companies worked by Age Group",
       x = "Age Group",
       y = "Number of Companies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        legend.position = "none")






summary(data)




ggplot(data, aes(x = AgeGroup, y = TrainingTimesLastYear)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(title = "Training Time by Age Group",
       x = "Age Group",
       y = "Training Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        legend.position = "none")

############################################################################################################



ggplot(data, aes(x = AgeGroup, y = data$YearsSinceLastPromotion)) +
  geom_bar(stat = "identity", fill = "Red", alpha = 0.7) +
  labs(title = "Years Since Last Promotion by Age Group",
       x = "Age Group",
       y = "Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        legend.position = "none")







ggplot(data, aes(x = BusinessTravel)) +
  geom_bar() +
  facet_wrap(~ AgeGroup) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  theme(strip.background = element_blank())  # Remove strip background for better visibility of margins





