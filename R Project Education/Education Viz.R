data <- read.csv("C:\\Users\\Gosisa Jinasena\\Desktop\\R Project Education\\HR_Analytics.csv")

library(tidyverse)
library(ggplot2)

# Number of Employees by Education Field

Edu_Field <- table(data$EducationField)
Edu_Field

barplot(Edu_Field,
        main = "Number of Employees by Education Field",
        xlab = "Education Field",
        ylab = "Number of Employees",
        col = "skyblue")

# Number of Employees by Gender & Education Field

Gender <- table(data$Gender)
Gender

Edu_Field <- table(data$EducationField)
Edu_Field

ggplot(data, aes(x = EducationField, fill = Gender)) +
  geom_bar(position = "dodge",
           alpha = 0.5) +
  labs(title = "Number of Employees by Gender & EducationField",
       x = "Education Field",
       y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"))

# Education Field & Job Role

data %>% 
  select(JobRole, EducationField) %>%
  mutate(JobRole = recode(JobRole,"Human Resources" = "Human Resources Manager")) %>% 
  arrange(JobRole) -> Job_Edu

Job_Edu %>%
  ggplot(aes(x = "", fill = JobRole)) +
  theme_bw() +
  geom_bar(width = 1, color = "white") +
  facet_wrap(~EducationField) +
  coord_polar("y", start = 0) +
  ggtitle("Education Field & Job Role") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Number of Employees by Department & Education Field

ggplot(data, aes(x = EducationField, fill = EducationField)) +
  geom_bar() +
  facet_wrap(~ Department) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  theme(strip.background = element_blank()) +
  labs(title = "Number of Employees by Department & Education Field",
         x = "Education Field",
         y = "Number of Employees")

# Number of Employees by Age Group & Education Level

data %>% 
  select(Education, AgeGroup) %>%
  mutate(Education = recode(Education,
                           "1" = "Level 1",
                           "2" = "Level 2",
                           "3" = "Level 3",
                           "4" = "Level 4",
                           "5" = "Level 5")) %>% 
  arrange(Education) -> education_level 

ggplot(education_level, aes(x = Education, fill = AgeGroup)) +
  geom_bar(position = "dodge",
           alpha = 0.5) +
  theme_bw() +
  labs(title = "Number of Employees by Age Group & Education Level",
       x = "Education Level",
       y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"))

# Number of Employees by Education Level & Marital Status

data %>% 
  select(Education, MaritalStatus) %>%
  mutate(Education = recode(Education,
                            "1" = "Level 1",
                            "2" = "Level 2",
                            "3" = "Level 3",
                            "4" = "Level 4",
                            "5" = "Level 5")) %>% 
  arrange(Education) -> Ms_Edu 

ggplot(Ms_Edu, aes(x = MaritalStatus, fill = Education)) +
  geom_bar(position = "dodge",
           alpha = 0.5) +
  theme_bw() +
  labs(title = "Number of Employees by Education Level & Marital Status",
       x = "Marital Status",
       y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"))

# Monthly Income by Education Field

ggplot(data, aes(x = EducationField, y = MonthlyIncome, color = EducationField)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  labs(title = "Monthly Income by Education Field",
       x = "Education Field",
       y = "Monthly Income") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"))

# Monthly Income by Gender & Education Level

data %>% 
  select(Education, MonthlyIncome, Gender) %>%
  mutate(Education = recode(Education,
                            "1" = "Level 1",
                            "2" = "Level 2",
                            "3" = "Level 3",
                            "4" = "Level 4",
                            "5" = "Level 5")) %>% 
  arrange(Education) -> Monthly_Edu

ggplot(Monthly_Edu, aes(x = Education, y = MonthlyIncome, fill = Gender)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.6) +
  labs(title = "Monthly Income by Gender & Education Level",
       x = "Education Level",
       y = "Monthly Income")+
  scale_fill_manual(values = c("pink","skyblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"))

# Number of Companies Worked by Education *To be corrected

data %>% 
  select(Education, NumCompaniesWorked) %>%
  mutate(Education = recode(Education,
                            "1" = "Level 1",
                            "2" = "Level 2",
                            "3" = "Level 3",
                            "4" = "Level 4",
                            "5" = "Level 5")) %>% 
  arrange(Education) -> NoComp_Edu

NoComp_Edu %>% 
  
ggplot(aes(x = Education, y = NumCompaniesWorked)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(title = "Number of Companies Worked by Education",
       x = "Education Level",
       y = "Number of Companies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        axis.line = element_line(color = "black"),
        legend.position = "none")




     











  
  

  



