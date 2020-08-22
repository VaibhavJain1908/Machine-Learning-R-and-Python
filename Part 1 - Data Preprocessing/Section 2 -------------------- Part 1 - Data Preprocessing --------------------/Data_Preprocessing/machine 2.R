# Simple linear Regression

#Importing the Dataset
dataset=read.csv("Salary_Data.csv")
#dataset=dataset[,2:3]

#sPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

#Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

#Predicting the Test set Results
y_pred = predict(regressor, newdata = test_set)

#Visualising the Training set Results
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             color = "Red") +
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = "Blue") +
  ggtitle("Salary Vs Experience (Training set)") +
  xlab("Years of experience") +
  ylab("Salary")

#Visualising the Test set Results
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),
             color = "Red") +
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = "Blue") +
  ggtitle("Salary Vs Experience (Training set)") +
  xlab("Years of experience") +
  ylab("Salary")

#---------------------------- Multiple Linear Regression


#Importing the Dataset
dataset=read.csv("50_Startups.csv")
#dataset=dataset[,2:3]

#Encoding Categorical Data
dataset$State=factor(dataset$State,
                       levels=c('New York','California','Florida'),
                       labels=c(1,2,3))


#sPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Profit,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

#Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

#Predicting the Test set Results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend + State,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend ,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)

#------------------------------ Polynomial Regression

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

#Fitting Simple Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
  
#Fitting Simple Linear Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#Visualising the Linear Regression
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata = dataset)),
            color="Blue") +
  ggtitle("Truth or Bluff (Linear Regression)") +
  xlab("Level") +
  ylab("Salary")

#Visualising the Polynomial Regression
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata = dataset)),
            color="Blue") +
  ggtitle("Truth or Bluff (Polynomial Regression)") +
  xlab("Level") +
  ylab("Salary")

#Predicting The new result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level=6.5))

#Predicting the New Result with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level=6.5,
                                      Level2=6.5^2,
                                      Level3=6.5^3,
                                      Level4=6.5^4))

#---------------------------- Support Vector Regression

#Importing the Dataset
dataset=read.csv("Position_Salaries.csv")
dataset=dataset[,2:3]

#SPLITTING The Dataset Into the Taining set and Test set
#install.packages("caTools ")
# library(caTools)
# set.seed(123)
# split=sample.split(dataset$DependentVariable,SplitRatio = 0.8)
# training_set=subset(dataset,split==TRUE)
# test_set=subset(dataset,split==FALSE)

#Feature Scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

#Fitting SVR Model to the dataset
#install.packages("e1071")
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = "eps-regression")

#Predicting the New Result
y_pred = predict(regressor, data.frame(Level=6.5))

#Visualising the SVR Model Results
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=dataset$Level,y=predict(regressor,newdata = dataset)),
            color="Blue") +
  ggtitle("Truth or Bluff (SVR Model)") +
  xlab("Level") +
  ylab("Salary")

#-------------------------- Decision Tree Regression

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

#Fitting Decision Tree Regression Model to the dataset
#install.packages("rpart")
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

#Predicting the New Result
y_pred = predict(regressor, data.frame(Level=6.5))

#Visualising the Decision Tree Regression Model Results(for higher resolution and smoother curve)
#install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=x_grid,y=predict(regressor,newdata = data.frame(Level = x_grid))),
            color="Blue") +
  ggtitle("Truth or Bluff (Decision Tree Regression Model)") +
  xlab("Level") +
  ylab("Salary")

#------------------------- Random Forest Regression

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

#Fitting Random Forest Regression Model to the dataset
#install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500)

#Predicting the New Result
y_pred = predict(regressor, data.frame(Level=6.5))

#Visualising the Random Forest Regression Model Results(for higher resolution and smoother curve)
#install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             color="Red") +
  geom_line(aes(x=x_grid,y=predict(regressor,newdata = data.frame(Level = x_grid))),
            color="Blue") +
  ggtitle("Truth or Bluff (Random Forest Regression Model)") +
  xlab("Level") +
  ylab("Salary")


#---------------------------------------------------------------------------
#Note-Assumptions of a Linear Regression(always check before making one)   |

#                                                                          |
#1.Linearity                                                               |                                                                           |
#2.Homoscedasticity                                                        |
#3.Multivariate normality                                                  |
#4.Independence of Errors                                                  |        
#5.Lack of Collinearity                                                    |
#---------------------------------------------------------------------------




