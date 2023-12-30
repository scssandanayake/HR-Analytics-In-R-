data <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\dataAssignment.csv")
str(data)

# Linear Regression: This model can be used to understand how age relates to various other factors in the dataset. 
# For example, you could predict salary based on age, years of experience, etc.

lm_model <- lm(data$MonthlyIncome ~ data$Age + data$TotalWorkingYears, data = data)
lm_model


install.packages("randomForest")
library(randomForest)


# Decision Trees and Random Forests: 
# These models can help identify significant variables that influence age or predict certain outcomes based on 
# age in a more interpretable manner.


sum(is.na(data))
data2 <- na.omit(data)
sum(is.na(data2))
rf_model <- randomForest(data$Age ~ ., data = data2)   # To implement this model we need to do better data preprocessing.


#Logistic Regression: If you're looking to understand binary outcomes
#(e.g., likelihood of leaving the company based on age), logistic regression can be useful.



# Example code for logistic regression
glm_model <- glm(data$Attrition ~ data$Age + data$TotalWorkingYears, data = data)


# Example code for k-means clustering
# must preprocess the data before implemnting this.

set.seed(1500)
cluster_results <- kmeans(data[,1:11], centers = 5, nstart = 100)
cluster_results

table(cluster_results$cluster , data$Age)



clusplot(data,cluster_results$cluster, color = T, shade = T, labels = 0 , lines= 0)
