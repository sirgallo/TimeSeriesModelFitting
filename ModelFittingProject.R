library(TSA)
library(forecast)

########
#Nicholas Gallo
#Time Series Model Fitting Project
########

#Read in CSV file, however, there are libraries for JSON objects and Excel tables
Project <- read.csv(file.choose(), header = TRUE)

#Convert dataset to seasonal time series 
ProjData <- ts(Project$Data, frequency = 12, start = c(1962, 1))
summary(ProjData)
plot.ts(ProjData, main = "Milk Production per Cow in Pounds from 1962-1975", xlab = "Milk Production", ylab = "Month", type = "o")
#Decompose to check trend and seasonality of data, as well as randomness
plot(decompose(ProjData))

#Split dataset into training and testing set
#The training set is used to fit models and the test set is used for analysis of predicted values up to the end of the dataset
trainset <- subset(ProjData, end = floor(0.75*length(ProjData)))
testset <- subset(ProjData, start = floor(0.75*length(ProjData)) + 1)

#Fitting a linear regression model, using lagged data
ProjDataRegress <- lm(formula = ProjData ~ zlag(ProjData), data = ProjData)
summary(ProjDataRegress)
#Check residuals for normality. Histogram is especially useful
StandardResRegress <- rstandard(ProjDataRegress)
qqnorm(StandardResRegress)
qqline(StandardResRegress)
hist(StandardResRegress)

#Analyze the data set, differencing if needed
DiffProjData <- diff(ProjData)
plot(DiffProjData)
acf(DiffProjData)
pacf(DiffProjData)

#Test first order auto-regressive (AR(1)) model on the training set
ProjAR1 <- Arima(trainset, order = c(1, 0, 0))
ProjAR1

#Predict one value ahead of the last value in the training set
#Use last value in training set to predict the next value
#This is just a test using the AR(1) Forecast function Yt(1)
Yt1 <- 0.9243*(trainset[length(trainset)] - 709.2583) + 709.2583
Yt1
#Alternative method that also works, using predict function
ProjARforecast <- predict(ProjAR1, n.ahead = 1)
ProjARforecast
#Forecast error of the model
ARforecastError <- testset[1] - ProjARforecast$pred
ARforecastError

#Lower and Upper Confidence intervals for the predicted data points
LCI <- (ProjARforecast$pred - 1.96*ProjARforecast$se)
UCI <- (ProjARforecast$pred + 1.96*ProjARforecast$se)
LCI
UCI

#Build custom model for the training set, in this case I utilize the ARIMA model and the associated libraries
auto.arima(ProjData)
ProjModel <- Arima(ProjData, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 1), period = 12))
ProjModel
#Test the normality of the Residuals
acf(ProjModel$residuals)
Box.test(ProjModel$residuals)
#Forecast predicted values up to one year and plot the forecast with the original data
ProjForecast <- forecast(ProjData, model = ProjModel, 12)
plot(ProjForecast, main = "Forecasted Values with Original Data", ylab = "Milk Production per Cow", xlab = "Time in Months", type = "o")
legend("bottomright", legend = c("Forecasted values"), col = c("blue"), pch = c(15))

#Fit a custom training model with the training set and the Arima model specified for the entire dataset
TrainProjModel <- Arima(trainset, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 1), period = 12))
#Use the predict function to fit values up through the length of the test set 
TrainPrediction <- predict(TrainProjModel, n.ahead = length(testset))
#Plot the predicted data against the observed data
plot(TrainPrediction$pred, main = "predicted vs actual", ylab = "Value in Pounds", xlab = "Time in Months", col = "blue", type ="o")
points(testset, col = "red", type = "o")
legend("topright", legend = c("Predicted Data", "Test Data"), col = c("blue", "red"), pch = c(15, 15))

#Calculate mean squared error of the predicted values vs. the observed values
MSEcalc <- function(testset, TrainPrediction) {
  prediction <- TrainPrediction$pred
  sum1 = 0 
  for(i in length(testset)) {
    sum1 <- sum1 + ((testset[i] - prediction[i])^2)
  }
  mse1 <- sum1/length(testData1)
  return(mse1)
}  
MSEcalc(testset, TrainPrediction)
