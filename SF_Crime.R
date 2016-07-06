#CSE 563 Fall 2015
#Japa Swadia

#San Francisco crime records analysis
#Applying data mining techniques to gain useful information regarding crimes in SF

#load the csv file of training data
sf_train <- read.csv("C:/Users/japas_000/Documents/CSE 563/Data sets/kaggle/SF crime/train.csv", header = TRUE, nrows = 10000)


#load the csv file of test data
sf_test <- read.csv("C:/Users/japas_000/Documents/CSE 563/Data sets/kaggle/SF crime/test.csv/test.csv", header = TRUE, nrows = 10000)


#prepare data frame for training data
t_data <- as.data.frame(sf_train)

#prepare data frame for test_data
test_data <- as.data.frame(sf_test)

#----------Classification--------------------

#install and load library for Support Vector Machine (SVM) classification
package.install("e1071")
library(e1071)

#remove unwanted columns from training set
train_data <- t_data[1:5000, -c(3,6)]


#train model using SVM with 10-fold cross-validation
model <- svm(Category~., data = train_data, method = "C-classification", kernel = "radial", cost = 10, gamma = 0.1)

#show summary of classification
summary(model)

#predict the category of crime
prediction <- predict(model, train_data)
#print the predicted data
print(prediction)

#plot graph based on prediction
plot(prediction)

#create confusion matrix
install.packages(caret)
library(caret)

#tab <- table(prediction, train_data$Category)
cm <- confusionMatrix(prediction, train_data$Category)
print(cm)

#applying trained model on test data
test2 <- t_data[5001:9999, c("Dates", "DayOfWeek", "PdDistrict", "Address", "X", "Y")]

#predict category of crime
pred_test <- predict(model, test2, type = 'class')


#print predicted values
print(pred_test)
write.csv(pred_test, "C:/Users/japas_000/Documents/CSE 563/output.csv")
#plot prediction
plot(pred_test)

tr_data <- train_data[ , -2]
pred_train <- predict(model, tr_data, type = 'class')

print(pred_train)
summary(pred_train)

write.csv(pred_train, "C:/Users/japas_000/Documents/CSE 563/trained_output.csv")
#-------------attempting neural network classifier----------
install.packages("nnet")
library(nnet)

#apply nnet function on training data
fitnn = nnet(Category ~ Address + Dates + DayOfWeek , train_data, size=1, MaxNWts = 10000)

#predict on test data
pred_nn <- predict(fitnn, test2, type = 'class')

#table(data.frame(predicted=predict(fitnn, test2)[,2] > 0.5,
                 actual=test$Surv[,2]>0.5))
cmnn <- confusionMatrix(pred_nn, train_data$Category)
train_nn <- train_data[ , -2]
pred_nn2 <- predict(fitnn, train_nn, type = 'class')


#-----------using Text Classification-----------------------
install.packages("RTextTools")
library(RTextTools)
# Create the document term matrix
dtMatrix1 <- create_matrix(sf_train[1:500, "Category"])

# Configure the training data
container <- create_container(dtMatrix1, sf_train[1:500, "Address"], trainSize=1:500, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)

# create a prediction document term matrix
sf_tester <- sf_test[1:500, ]
predMatrix <- create_matrix(sf_tester, originalMatrix=dtMatrix1)


 #-----------Regression----------------------------

#regressing category of crime against day of week
reg_test <- t_data[ , c(2,4)]

#plot graph
plot(reg_test, pch=16)

#linear regression - crimes by day of week
reg_model <- lm(DayOfWeek ~ Category, reg_test)

#print the model
print(reg_model)

#check the different levels of Category and Day of week
levels(train_data$Category)
levels(train_data$DayOfWeek)

#now applying regression function
lm(Category ~ DayOfWeek, train_data)

#regressing crimes by year
#install lubridate package to extract year from Dates column
install.packages("lubridate")
library(lubridate)

#get all years from data set
crime_year <- as.Date(train_data$Dates, format = "%m/%d/%Y")
yr <- year(crime_year)
print(yr)

#now applying regression
reg_annual <- lm(yr ~ train_data$Category)
print(reg_annual)

#print summary
summary(reg_annual)

#attempting Random Forest classifier

#install and load library for applying Random Forest classification
install.packages("randomForest")
library(randomForest)
crime_fit <- randomForest(Category~., data=train_data, importance = TRUE, proximity = TRUE, ntree = 500)
#random forest cannot handle categorical predictors with more than 53 categories


#-----------Association rule discovery----------------

#applying association mining to discover interesting crime-related relations in the data set

#install arules package and load the library
install.packages("arules")
library(arules)

#inspect structure of data frame
str(train_data)
#coerce X and Y columns to factor
train_data$X <- as.factor(train_data$X)
train_data$Y <- as.factor(train_data$Y)
#apply apriori function
rules <- apriori(train_data, parameter = list(supp = 0.001, conf = 0.5))

#inspect the association rule output
inspect(rules)

