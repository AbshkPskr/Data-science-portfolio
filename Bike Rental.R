#cleanup enviorment
rm(list = ls())

#installing required pacakages
install.packages("caret")
install.packages("Hmisc")
install.packages('corrplot')
install.packages('PerformanceAnalytics')
install.packages('caTools')
install.packages('randomForest')
install.packages('e1071')


#read dataset 
library(caret)
bikeRent = read.csv(file = "https://s3-ap-southeast-1.amazonaws.com/edwisor-india-bucket/projects/data/DataN0103/day.csv",row.names = 1)

#dimensions of dataset: 731 Rows, 16 columns
dim(bikeRent)

#getting datatypes and structure of columns
str(bikeRent)

#getting first five rows
head(bikeRent)

#getting statistical figures of columns of dataset
library(Hmisc)
describe(bikeRent)

#getting column names
names(bikeRent)

#DATA PREPARATION--------------------------------------------------------------------------------------------------------------------------------------------

#creating new dataset for EXPLORTORY DATA ANALYSIS with proper column names
data = bikeRent

data$Date = factor(data$dteday)
data$Season = factor(data$season ,levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
data$Year = factor(data$yr,levels = c(0,1),labels  =  c("2011","2012"))
data$Month = factor(data$mnth)
data$Holiday = factor(data$holiday,levels = c(0,1),labels  =  c("Working day","Holiday"))
data$Weekday = factor(data$weekday)
data$Working_Day = factor(data$workingday,levels = c(0,1),labels  =  c("Holiday","Working day"))
data$Weather_Condition = factor(data$weathersit,levels = c(1,2,3,4),labels  =  c("Clear, Few clouds, Partly cloudy, Partly cloudy",
                                                                                 "Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist",
                                                                                 "Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds",
                                                                                 "Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog"))
data$Temperature = as.numeric(data$temp*(39 + 8)) - 8
data$Feeling_Temperature = as.numeric(data$atemp*(50 + 16)) - 16
data$Humidity = as.numeric(data$hum * 100)
data$Wind_Speed = as.numeric(data$windspeed * 67)
data$Casual_Users = as.numeric(data$casual)
data$Registered_Users = as.numeric(data$registered)
data$Count = as.numeric(data$cnt)

sapply(data,class)

data = data[, !colnames(data) %in% c(names(bikeRent)), drop = FALSE]

#EXPLORTORY DATA ANALYSIS--------------------------------------------------------------------------------------------------------------------------------------

#Checking distribution of target variable
hist(data$Count)
#it seems target variable is nearly normally distributed

#plotting categorical variable vs target variable count
library(ggplot2)
ggplot(data, aes(x=Season,y = Count, fill=Weather_Condition)) +  geom_bar(stat = 'identity') + ggtitle('Season and Weather wise Bike Count')
ggplot(data, aes(x=Year,y = Count)) +  geom_bar(stat = 'identity') + ggtitle('Year wise Bike Count')
ggplot(data, aes(x=Month,y = Count)) +  geom_bar(stat = 'identity') + ggtitle('Month wise Bike Count')
ggplot(data, aes(x=Holiday,y = Count)) +  geom_bar(stat = 'identity') +ggtitle('Holiday wise Bike Count')
ggplot(data, aes(x=Weekday,y = Count)) +  geom_bar(stat = 'identity') + ggtitle('Weekday wise Bike Count')
ggplot(data, aes(x=Working_Day,y = Count)) +  geom_bar(stat = 'identity') +ggtitle('Bike Count By Working Day')

#plotting continuous variable vs target variable count
ggplot(data, aes(x=Temperature,y = Count)) +  geom_point(color = 'maroon')  +ggtitle('Bike Count vs Temperature Distribution')
ggplot(data, aes(x=Feeling_Temperature,y = Count)) +  geom_point()  +ggtitle('Bike Count vs Feeling Temperature')
ggplot(data, aes(x=Humidity,y = Count)) +  geom_point() +ggtitle('Bike Count vs Humidity')
ggplot(data, aes(x=Wind_Speed,y = Count)) +  geom_point() +ggtitle('Bike Count vs Wind Speed')
ggplot(data, aes(x=Casual_Users,y = Count)) +  geom_point()  +ggtitle('Bike Count vs Casual number of users')
ggplot(data, aes(x=Registered_Users,y = Count)) +  geom_point() +ggtitle('Bike Count vs Registered Users')

#plotting distribution of continuous variable
ggplot(data, aes(x=Temperature)) +  geom_histogram(bins = 50) +ggtitle('Temperature Distribution')
ggplot(data, aes(x=Feeling_Temperature)) +  geom_histogram(bins = 50) +ggtitle('Feeling Temperature Distribution')
ggplot(data, aes(x=Humidity)) +  geom_histogram(bins = 50)+ggtitle('Humidity Distribution')
ggplot(data, aes(x=Wind_Speed)) +  geom_histogram(bins = 50)+ggtitle('WindSpeed Distribution')
ggplot(data, aes(x=Casual_Users)) +  geom_histogram(bins = 50)+ggtitle('Casual users Distribution')
ggplot(data, aes(x=Registered_Users)) +  geom_histogram(bins = 50)+ggtitle('Registered users Distribution')

#MISSING VALUE ANALYSIS----------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(data))

#OUTLIER DETECTION---------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Count,y = Temperature)) +  geom_boxplot()
ggplot(data, aes(x=Count,y =Feeling_Temperature)) +  geom_boxplot()
ggplot(data, aes(x=Count,y =Humidity)) +  geom_boxplot()
ggplot(data, aes(x=Count,y =Wind_Speed)) +  geom_boxplot()
ggplot(data, aes(x=Count,y =Casual_Users)) +  geom_boxplot()
ggplot(data, aes(x=Count,y =Registered_Users)) +  geom_boxplot()
#variable humidity, wind speed and casual users have outliers


#OUTLIER REMOVAL-----------------------------------------------------------------------------------------------------------------------------------------------
OutlierRemoval = function(var){
  qnt = quantile(var, probs=c(.25, .75), na.rm = T)
  caps = quantile(var, probs=c(.05, .95), na.rm = T)
  H = 1.5 * IQR(var, na.rm = T)
  var[var < (qnt[1] - H)] <- caps[1]
  var[var > (qnt[2] + H)] <- caps[2]
  return (var)}

data$Humidity = OutlierRemoval(data$Humidity)
data$Wind_Speed = OutlierRemoval(data$Wind_Speed)
data$Casual_Users = OutlierRemoval(data$Casual_Users)


#FEATURE SELECTION---------------------------------------------------------------------------------------------------------------------------------------------
library("PerformanceAnalytics")
Continuous = c('Temperature','Feeling_Temperature','Humidity','Wind_Speed','Casual_Users','Registered_Users','Count')
chart.Correlation(data[Continuous], histogram=TRUE)

#dropping date variable as it is not useful in analysis
#droppin feeling temperature as it is highly correlated with temperature
#registered users is also highly correlated with target variable count
#after dropping registered users, casual users is of no use so dropping it also
data = data[, !colnames(data) %in% c('Date','Feeling_Temperature','Casual_Users','Registered_Users'), drop = FALSE]

data$Season = as.factor(bikeRent$season)
data$Year = as.factor(bikeRent$yr)
data$Month = as.factor(bikeRent$mnth)
data$Holiday = as.factor(bikeRent$holiday)
data$Weekday = as.factor(bikeRent$weekday)
data$Working_Day = as.factor(bikeRent$workingday)
data$Weather_Condition = as.factor(bikeRent$weathersit)

#FEATURE sCALING-----------------------------------------------------------------------------------------------------------------------------------------------
#Scale = c('Casual_Users')
#for(i in Scale){  data[i] = (data[i] - min(data[i]))/(max(data[i] - min(data[i])))}

#SAMPLING------------------------------------------------------------------------------------------------------------------------------------------------------
library(caTools)
#divided dataset into 80% training set and 20% test set
sample = sample.split(data,SplitRatio = 0.8) 
train =subset(data,sample ==TRUE)  
test=subset(data, sample==FALSE)

#MODELLING AND EVALUATION--------------------------------------------------------------------------------------------------------------------------------------

#evaluation (error calculation functions)
MAPE = function(actual,predicted){mean((abs(actual-predicted))/actual)*100}
MAE  = function(actual,predicted){mean((abs(actual-predicted)))}
RMSE = function(actual,predicted){sqrt(mean(((abs(actual-predicted)))^2))}


#Linear Regression
#MAPE = 16.61%
#MAE  = 547.63
#RMSE = 777.09
LR =  lm(Count ~.,data = train)
LRpredicted = predict(LR,test)
MAPE(test$Count,LRpredicted)
MAE (test$Count,LRpredicted)
RMSE(test$Count,LRpredicted)


#Decision Tree
#MAPE = 21.24%
#MAE  = 702.53
#RMSE = 975.02
library(rpart)
DT = rpart(Count ~ ., data = train)
DTpredicted = predict(DT,test)
MAPE(test$Count,DTpredicted)
MAE (test$Count,DTpredicted)
RMSE(test$Count,DTpredicted)


#Random Forest
#MAPE = 15.78%
#MAE  = 477.89
#RMSE = 682.39
library(randomForest)
RF =  randomForest(Count ~.,data = train)
RFpredicted = predict(RF,test)
MAPE(test$Count,RFpredicted)
MAE (test$Count,RFpredicted)
RMSE(test$Count,RFpredicted)

#From above calcualtions RandomForest is the best fit for the dataset
