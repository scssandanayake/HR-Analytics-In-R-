data1 <- read.csv("C:\\Users\\User\\Desktop\\R lec\\Assignment\\Project\\dataAssignment.csv")

str(data1)

data1$AgeGroup <-as.integer(factor(data1$AgeGroup))
data1$Attrition <-as.integer(factor(data1$Attrition))
data1$BusinessTravel <-as.integer(factor(data1$BusinessTravel))
data1$Department <-as.integer(factor(data1$Department))
data1$EducationField <-as.integer(factor(data1$EducationField))
data1$Gender <-as.integer(factor(data1$Gender))
data1$JobRole <-as.integer(factor(data1$JobRole))
data1$MaritalStatus <-as.integer(factor(data1$MaritalStatus))
data1$SalarySlab <-as.integer(factor(data1$SalarySlab))
data1$Over18 <-as.integer(factor(data1$Over18))
data1$OverTime <-as.integer(factor(data1$OverTime))

str(data1)


data2 <- data1[, !names(data1) %in% c("Age")]
data2 <- data2[, !names(data2) %in% c("AgeGroup")]
data2 <- data2[, !names(data2) %in% c("EmpID")]

sum(is.na(data2))
data2 <- na.omit(data2)
sum(is.na(data1))
data1 <- na.omit(data1)
sum(is.na(data1))

str(data1)

constant_vars <- sapply(data2, function(x) length(unique(x))) == 1
constant_columns <- names(data2)[constant_vars]
constant_columns
data2 <- data2[, !constant_vars]

set.seed(100)
cluster_results <- kmeans(data2, centers = 5, nstart = 25)
cluster_results

table(cluster_results$cluster , data1$AgeGroup)

length(cluster_results$cluster)
length(data1$AgeGroup)

Age_group <- table(data1$AgeGroup)
Age_group

library(cluster)

clusplot(data2,cluster_results$cluster, color = T, shade = T, labels = 0 , lines= 0)

