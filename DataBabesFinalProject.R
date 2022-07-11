#Load Libraries

library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")
library("tidyverse")
library("lubridate")
library("forecast")
library("tseries")
library("tsbox")

#Import Datasets
#1st dataset

Data3RegularAllAreasAllFormulations <-read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/Data3AllAreasAllFormulations.xlsx")
View(Data3AllAreasAllFormulations)
Data6MidGradeAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data6MidGradeAllAreasAllFormulations.xlsx")
View(Data6MidGradeAllAreasAllFormulations)
Data9PremiumAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data9PremiumAllAreasAllFormulations.xlsx")
View(Data9PremiumAllAreasAllFormulations)
Data12AllGradesAreasAndFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data12AllGradesAreasAndFormulations.xlsx")
View(Data12AllGradesAreasAndFormulations)

#2nd data set

SR1 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/SR1.xlsx")
View(SR1)
SR2 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/SR2.xlsx")
View(SR2)
SR3 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/SR3.xlsx")
View(SR3)
SR4 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/SR4.xlsx")
View(SR4)
SR5 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/SR5.xlsx")
View(SR5)

#Asking the Questions
# What will future gas prices look like based on previous trends?
# How do gas prices differ among regions?

#Testing Assumptions for Regression

#1. There is a linear relationship between x and y.
#Create a Scatterplot and Look at the Shape
scatter.smooth(x = Data12AllGradesAreasAndFormulations$Date, y= Data12AllGradesAreasAndFormulations$USAllAll)

# The data is not linear
# The data is not exponential but potentially logistic

# Data Wrangling- picking out IVs  
# View NA Values

view(Data3AllAreasAllFormulations)
is.na(Data3AllAreasAllFormulations)

# Eliminate NA Values
Data3 = na.omit(Data3AllAreasAllFormulations)
Data3
Data6 = na.omit(Data6MidGradeAllAreasAllFormulations)
Data6
Data9 = na.omit(Data9PremiumAllAreasAllFormulations)
Data9
Data12 = na.omit(Data12AllGradesAreasAndFormulations)
Data12


LocationModel <- Data12[,2:10]
LocationModel

unique(LocationModel)



#----------------------------------------------------------
#Arima Forecasting

class(GradeModel3)

#It is listed as a data frame. Need to recode as time series data

#GradeModelTS <- ts(GradeModelT3$Date, start = min(GradeModel3$Date), end = max(GradeModel3$Date), frequency = 51)

#Need smaller dataset, subsetting further
#Try from rows 450-992
GradeModelTS2 <- GradeModel3[450:478,1:2]
GradeModelTS2

GradeModelTS <- ts(GradeModelTS2$Date, start = min(GradeModelTS2$Date), end = max(GradeModelTS2$Date), frequency = 26)

class(GradeModelTS)
#ts

plot(GradeModelTS)
#Data set is too large


#Running ARIMA on the 2nd dataset

class(SR1)
#Data Frame 
#Subsetting for timeseries recoding

SR1a <- SR1[,1:2]
SR1TS <- ts(SR1a$Year, start = min(SR1a$Year), end = max(SR1a$Year), frequency = 1)
SR1TS


class(SR1TS)
# ts
plot(SR1a)

SR1b <- SR1[,1,3]
SR1b

#Looking at the shape of the data

scatter.smooth(x = SR1$Year, y = SR1$Jan)
scatter.smooth(x = SR1$Year, y = SR1$Feb)
scatter.smooth(x = SR1$Year, y = SR1$Mar)
scatter.smooth(x = SR1$Year, y = SR1$Apr)
scatter.smooth(x = SR1$Year, y = SR1$May)
scatter.smooth(x = SR1$Year, y = SR1$Jun)
scatter.smooth(x = SR1$Year, y = SR1$Jul)
scatter.smooth(x = SR1$Year, y = SR1$Aug)
scatter.smooth(x = SR1$Year, y = SR1$Sep)
scatter.smooth(x = SR1$Year, y = SR1$Oct)
scatter.smooth(x = SR1$Year, y = SR1$Nov)
scatter.smooth(x = SR1$Year, y = SR1$Dec)
scatter.smooth(x = SR1$Year, y = SR1$HALF1)
scatter.smooth(x = SR1$Year, y = SR1$HALF2)

scatter.smooth(x = SR2$Year, y = SR2$Jan)
scatter.smooth(x = SR2$Year, y = SR2$Feb)
scatter.smooth(x = SR2$Year, y = SR2$Mar)
scatter.smooth(x = SR2$Year, y = SR2$Apr)
scatter.smooth(x = SR2$Year, y = SR2$May)
scatter.smooth(x = SR2$Year, y = SR2$Jun)
scatter.smooth(x = SR2$Year, y = SR2$Jul)
scatter.smooth(x = SR2$Year, y = SR2$Aug)
scatter.smooth(x = SR2$Year, y = SR2$Sep)
scatter.smooth(x = SR2$Year, y = SR2$Oct)
scatter.smooth(x = SR2$Year, y = SR2$Nov)
scatter.smooth(x = SR2$Year, y = SR2$Dec)
scatter.smooth(x = SR2$Year, y = SR2$HALF1)
scatter.smooth(x = SR2$Year, y = SR2$HALF2)
scatter.smooth(x = SR2$Year, y = SR2$Annual)

scatter.smooth(x = SR3$Year, y = SR3$Jan)
scatter.smooth(x = SR3$Year, y = SR3$Feb)
scatter.smooth(x = SR3$Year, y = SR3$Mar)
scatter.smooth(x = SR3$Year, y = SR3$Apr)
scatter.smooth(x = SR3$Year, y = SR3$May)
scatter.smooth(x = SR3$Year, y = SR3$Jun)
scatter.smooth(x = SR3$Year, y = SR3$Jul)
scatter.smooth(x = SR3$Year, y = SR3$Aug)
scatter.smooth(x = SR3$Year, y = SR3$Sep)
scatter.smooth(x = SR3$Year, y = SR3$Oct)
scatter.smooth(x = SR3$Year, y = SR3$Nov)
scatter.smooth(x = SR3$Year, y = SR3$Dec)
scatter.smooth(x = SR3$Year, y = SR3$HALF1)
scatter.smooth(x = SR3$Year, y = SR3$HALF2)
scatter.smooth(x = SR3$Year, y = SR3$Annual)

scatter.smooth(x = SR4$Year, y = SR4$Jan)
scatter.smooth(x = SR4$Year, y = SR4$Feb)
scatter.smooth(x = SR4$Year, y = SR4$Mar)
scatter.smooth(x = SR4$Year, y = SR4$Apr)
scatter.smooth(x = SR4$Year, y = SR4$May)
scatter.smooth(x = SR4$Year, y = SR4$Jun)
scatter.smooth(x = SR4$Year, y = SR4$Jul)
scatter.smooth(x = SR4$Year, y = SR4$Aug)
scatter.smooth(x = SR4$Year, y = SR4$Sep)
scatter.smooth(x = SR4$Year, y = SR4$Oct)
scatter.smooth(x = SR4$Year, y = SR4$Nov)
scatter.smooth(x = SR4$Year, y = SR4$Dec)
scatter.smooth(x = SR4$Year, y = SR4$HALF1)
scatter.smooth(x = SR4$Year, y = SR4$HALF2)
scatter.smooth(x = SR4$Year, y = SR4$Annual)

scatter.smooth(x = SR5$Year, y = SR5$Jan)
scatter.smooth(x = SR5$Year, y = SR5$Feb)
scatter.smooth(x = SR5$Year, y = SR5$Mar)
scatter.smooth(x = SR5$Year, y = SR5$Apr)
scatter.smooth(x = SR5$Year, y = SR5$May)
scatter.smooth(x = SR5$Year, y = SR5$Jun)
scatter.smooth(x = SR5$Year, y = SR5$Jul)
scatter.smooth(x = SR5$Year, y = SR5$Aug)
scatter.smooth(x = SR5$Year, y = SR5$Sep)
scatter.smooth(x = SR5$Year, y = SR5$Oct)
scatter.smooth(x = SR5$Year, y = SR5$Nov)
scatter.smooth(x = SR5$Year, y = SR5$Dec)
scatter.smooth(x = SR5$Year, y = SR5$HALF1)
scatter.smooth(x = SR5$Year, y = SR5$HALF2)
scatter.smooth(x = SR5$Year, y = SR5$Annual)

#Combining datasets

AModel <-merge(SR1, SR2, by = "Year")
AModel
BModel <-merge(AModel, SR3, by = "Year")
BModel
CModel <-merge(BModel, SR5, by = "Year")

#Dropping NA values

DModel <-na.omit(CModel)
#Making into time-series data
class(DModel)
#Data frame

FinModel <- ts(DModel$HALF1.x, start = min(DModel$Year), end = max(DModel$Year), frequency = 2)
FinModel
class(FinModel)

#Now it is the right class
FinModel1 = unique(FinModel)

plot(FinModel1)

#Testing Assumptions
#1. Data is Stationary
#Auto-corrolation test
acf(FinModel1)
#Partial auto-corrolation test
pacf(FinModel1)
#The data is not stationary
#Augmented Dickey-Fuller Test
adf.test(FinModel1)

#Augmented Dickey-Fuller Test

#data:  FinModel1
#Dickey-Fuller = -1.3573, Lag order = 2, p-value = 0.8172
#alternative hypothesis: stationary

#Convert data into stationary Data

NewModel = auto.arima(FinModel1, ic = "aic", trace = TRUE)
NewModel

#Best model: ARIMA(0,1,0)                    
#NewModel
#Series: FinModel1 
#ARIMA(0,1,0) 

#sigma^2 = 1619:  log likelihood = -46.02
#AIC=94.05   AICc=94.62   BIC=94.25

acf(ts(NewModel$residuals))
#partial auto correlation function

pacf(ts(NewModel$residuals))

MyForecast = forecast(NewModel, level = c(95), h = 20*1)
MyForecast

#Point Forecast      Lo 95    Hi 95
#11        243.583 164.716153 322.4498
#12        243.583 132.048436 355.1176
#13        243.583 106.981615 380.1844
#14        243.583  85.849307 401.3167
#15        243.583  67.231370 419.9346
#16        243.583  50.399468 436.7665
#17        243.583  34.920937 452.2451
#18        243.583  20.513872 466.6521
#19        243.583   6.982460 480.1835
#20        243.583  -5.815867 492.9819

plot(MyForecast)

#validate forecast
Box.test(MyForecast$residuals, lag = 15, type = "Ljung-Box")

___________________________________________________________________

#Running ARIMA forecasting on the 1'st dataset
#Importing the quarterly numbers

AverageQuarterly <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/AverageQuarterly.xlsx")
View(AverageQuarterly)

#Getting rid of NAs

YearUsAll20 = na.omit(AverageQuarterly)
YearUsAll20
class(YearUsAll20)

#data frame table
#Putting in time series format

TSMod <- ts_timeSeries(YearUsAll20$Year)

TSMod = ts(YearUsAll20$Year, start = 2002, end = 2022, frequency = 4)
TSMod

#double-checking that it worked
class(TSMod)

#Plotting to see the shape
scatter.smooth(x = YearUsAll20$Year, y = YearUsAll20$AvgQtrly)

TSMod[nrow(TSMod)+1] <- New_Row
YearUsAll20['TSMod'] <- TSMod

rlang::last_error()

#Testing Assumptions
#1. Data is Stationary
#Auto-correlation test
acf(TSMod)

#Partial auto-correlation test
pacf(TSMod)
#The data is stationary

#Augmented Dickey-Fuller Test
adf.test(TSMod)

#data:  TSMod
#Dickey-Fuller = -3.73e+12, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary
#The data is stationary

NewMod = auto.arima(TSMod, ic = "aic", trace = TRUE)
NewMod

#Models (0,0,0) and (0,1,0)

acf(ts(NewMod$residuals))
#partial auto correlation function

pacf(ts(NewMod$residuals))

MyForecast2 = forecast(NewMod, level = c(95), h = 10*1)
MyForecast2

#Point Forecast    Lo 95    Hi 95
#2022 Q2           2022 2021.106 2022.894
#2022 Q3           2022 2021.106 2022.894
#2022 Q4           2022 2021.106 2022.894
#2023 Q1           2023 2022.106 2023.894
#2023 Q2           2023 2021.736 2024.264
#2023 Q3           2023 2021.736 2024.264
#2023 Q4           2023 2021.736 2024.264
#2024 Q1           2024 2022.736 2025.264
#2024 Q2           2024 2022.451 2025.549
#2024 Q3           2024 2022.451 2025.549

#validate forecast
Box.test(MyForecast2$residuals, lag = 4, type = "Ljung-Box")

#data:  MyForecast2$residuals
#X-squared = 73.662, df = 4, p-value = 3.775e-15

-----------------------------------------------------------
#Read in quarterly dataset
AverageQrtrly <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/AverageQrtrly.xlsx")
View(AverageQrtrly)

TSMod = ts(AverageQrtrly$AvgQtrly, start = 2002, end = 2022, frequency = 4)
TSMod

#double-checking that it worked
class(TSMod)

#Plotting to see the shape
scatter.smooth(x = AverageQrtrly$Year, y = AverageQrtrly$AvgQtrly)

AverageQrtrly['TSMod'] <- TSMod

#Testing Assumptions
#1. Data is Stationary
#Auto-correlation test
acf(TSMod)

#Partial auto-correlation test
pacf(TSMod)
#The data is stationary

#Augmented Dickey-Fuller Test
adf.test(TSMod)

#Augmented Dickey-Fuller Test
#Dickey-Fuller = -2.2474, Lag order = 4, p-value = 0.4745
#alternative hypothesis: stationary


NewMod = auto.arima(TSMod, ic = "aic", trace = TRUE)
NewMod

#Models (0,1,0) and (1,0,0)

acf(ts(NewMod$residuals))
#partial auto correlation function

pacf(ts(NewMod$residuals))

MyForecast2 = forecast(NewMod, level = c(95), h = 40*1)
MyForecast2

#Point Forecast    Lo 95    Hi 95
#2022 Q2       3.869131 3.179275 4.558986
#2022 Q3       3.923571 2.947968 4.899174
#2022 Q4       3.689933 2.495069 4.884798
#2023 Q1       4.066132 2.686421 5.445843
#2023 Q2       4.099061 2.459234 5.738888
#2023 Q3       4.114559 2.250568 5.978551
#2023 Q4       4.048046 1.984095 6.111998
#2024 Q1       4.155144 1.908963 6.401325
#2024 Q2       4.164518 1.728747 6.600290
#2024 Q3       4.168931 1.557295 6.780566


#validate forecast
Box.test(MyForecast2$residuals, lag = 4, type = "Ljung-Box")

#Box-Ljung test

#data:  MyForecast2$residuals
#X-squared = 5.5346, df = 4, p-value = 0.2367

plot(MyForecast2)

