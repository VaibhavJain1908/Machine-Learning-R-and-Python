#Preprocessing Template

#Importing the Dataset
dataset=read.csv("Data.csv")
#dataset=dataset[,2:3]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$DependantVariable,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

#---------------------------------

#Regression Template

#Importing the Dataset
dataset=read.csv("Position_Salaries.csv")
dataset=dataset[,2:3]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
# library(caTools)
# set.seed(123)
# split=sample.split(dataset$DependentVariable,SplitRatio = 0.8)
# training_set=subset(dataset,split==TRUE)
# test_set=subset(dataset,split==FALSE)

#Feature Scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

#Fitting Regression Model to the dataset
#Create your regressor here

#Predicting the New Result
y_pred = predict(regressor, data.frame(Level=6.5))

#Visualising the Regression Model Results
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=dataset$Level,y=predict(regressor,newdata = dataset)),
            color="Blue") +
  ggtitle("Truth or Bluff (Regression Model)") +
  xlab("Level") +
  ylab("Salary")

#Visualising the Regression Model Results(for higher resolution and smoother curve)
#install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level),0.1)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=x_grid,y=predict(regressor,newdata = data.frame(Level = x_grid))),
            color="Blue") +
  ggtitle("Truth or Bluff (Regression Model)") +
  xlab("Level") +
  ylab("Salary")

#---------------------------- Classification Template

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

#Fitting Classifier to the Training set
#Create your classifier here

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
     main = "Classifier (Training set)",
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
     main = "Classifier (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4","red3"))

#--------------------------