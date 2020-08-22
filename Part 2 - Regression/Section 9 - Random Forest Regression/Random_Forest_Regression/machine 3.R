# CLASSIFICATION

#Logistic Regression

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting Logistic Regression to the dataset
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

#Predicting the Test set results
prob_pred = predict(classifier, type = "response", newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
prob_set = predict(classifier, type = "response", newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "Logistic Regression (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
prob_set = predict(classifier, type = "response", newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "Logistic Regression (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#--------------------- K-Nearest Neighbours (K-NN)

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5)

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = knn(train = training_set[, -3],
             test = grid_set,
             cl = training_set[, 3],
             k = 5)
plot(set[, -3],
     main = "K-NN (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = y_grid = knn(train = training_set[, -3],
                      test = grid_set,
                      cl = training_set[, 3],
                      k = 5)
plot(set[, -3],
     main = "K-NN (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#--------------------------- SVM

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting SVM to the Training set
library(e1071)
classifier = svm(formula =  Purchased ~ .,
                 data = training_set,
                 type = "C-classification",
                 kernel = "linear")

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "SVM (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, type = "response", newdata = grid_set)
plot(set[, -3],
     main = "SVM (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#--------------------- Kernel SVM

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#Encoding the Target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = "C-classification",
                 kernel = "radial")

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Kernel SVM (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, type = "response", newdata = grid_set)
plot(set[, -3],
     main = "Kernel SVM (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#----------------------- Naive Bayes

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#Encoding the Target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting Naive Bayes to the Training set
library(e1071)
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Naive Bayes (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Naive Bayes (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#----------------------- Decision Tree

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#Encoding the Target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting Decision Tree Classifier to the Training set
library(rpart)
classifier = rpart(formula = Purchased ~ .,
                   data = training_set)

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3], type = "class")

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = "Decision Tree (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = "Decision Tree (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Plotting the Decision Tree
plot(classifier)
text(classifier)

#----------------------- Random Forest

#Importing the Dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[,3:5]

#Encoding the Target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Fitting Random Forest Classifier to the Training set
library(randomForest)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 10)

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

#Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the Training set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Random Forest (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#Visualising the Test set results
#install_github("cran/ElemStatLearn")
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Random Forest (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#------------------------- 