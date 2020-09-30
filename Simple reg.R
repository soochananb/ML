##--- Step 1: Clear environment variables ------------------------------------------##
# if u want to clean the environment use this code = rm(list=ls(all=TRUE))
##--- Step 2: Set working Directory ------------------------------------------------##

getwd()

##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data = read.csv(file="Toyota_SimpleReg.csv",header = T)
names(cars_data)
str(cars_data)
summary(cars_data)

##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):
#drop the ID , model attributes
cars_data=cars_data[,-c(1,2)]

# Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)

#no missing values(NA)
##The covariance of the age of the car and price is -59136.11
##It indicates a negative linear relationship between the two variables
##This relation could be observed from the scatter plot also.
#cov is covariance
cov(cars_data)
#its -59136 so it indicates a negative relationship and this could be seen in scatter plot
plot(cars_data$Age_06_15,cars_data$Price)
#pch = plot shape
plot(cars_data$Age_06_15,cars_data$Price,xlab = "age of the car",ylab = "price in ($)",pch=1,col="blue")
#corelation is to find out how strongly variables are related
cor(cars_data)
cor(cars_data$Age_06_15,cars_data$Price)


#The correlation coeffiecient of the Age of the car and price is -0.8765905.
#Since the value is close to 1 and has a -ve sign we can conclude that the variables are strongly -ve


#Describe how the covarainace and correlation coefficients 



#Do the attributes have a good enough correlation coefficient to support linear regres  sion model building?

##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (70:20) ratio
rows=seq(1, nrow(cars_data),1)
set.seed(123)
trainrows=sample(rows,(70*nrow(cars_data))/100)
cars_train=cars_data[trainrows,]
cars_test =cars_data[-trainrows,]

trainRows1= sample(rows,(80*nrow(cars_data))/100)
cars_train1 = cars_data[trainRows1,]
cars_test1= cars_data[-trainRows1,]

trainRows2= sample(rows,(90*nrow(cars_data))/100)
cars_train2 = cars_data[trainRows2,]
cars_test2= cars_data[-trainRows2,]


###--- Step 6: Linear regression model building--------------------------------------##

LinReg =lm(Price~Age_06_15, data = cars_train)
coefficients(LinReg)

LinReg1= lm(Price~Age_06_15,data = cars_train1)
coefficients(LinReg1)

LinReg2=lm(Price~Age_06_15, data = cars_train2)
coefficients(LinReg2)

## Summary of model:
summary(LinReg)
plot(LinReg$residuals)
summary(LinReg)

#*** means its zero,thats how u read significant codes
#below 30 r square is noisy data
#if r square is noisy its underfit but present one is r square is 77 so its fit

#optional for info to extract cofficients
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
names(coefficients(LinReg))

#to extract residuals
LinReg$residuals
LinReg$rank
# to extract tarin predictions
LinReg$fitted.values
plot(LinReg$fitted.values)

#check validity of linear regression assumptions

##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments
par(mfrow=c(2,2))
plot(LinReg)
par(mfrow=c(1,1))

##--- Step 8: Predict on testdata --------------------------------------------------##

test_prediction =predict(LinReg,cars_test)
test_prediction
test_actual=cars_test$Price

##------step-9 error metrics--------------------##
library(DMwR)
#error verification on train data
regr.eval(cars_train$Price,LinReg$fitted.values)
plot(regr.eval(cars_train$Price,LinReg$fitted.values))

#error in test data
regr.eval(test_actual,test_prediction)

##--- Step 10: Confidence and Prediction Intervals----------------------------------##
# Confidence Intervals talk about the average values intervals
# Prediction Intervals talk about the all individual values intervals
Conf_Pred = data.frame(predict(LinReg, cars_test, interval="confidence",level=0.95))
Pred_Pred = data.frame(predict(LinReg, cars_test, interval="prediction",level=0.95))
plot(Conf_Pred)
names(Conf_Pred)

#Data Visualization
plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in ($)")

points(cars_test$Age_06_15,Conf_Pred$fit,type="l", col="green", lwd=2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$lwr,pch="-", col="blue", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$upr,pch="-", col="blue", lwd=4)


























