#Load Libraries

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")
library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")
library("tidyverse")
library("lubridate")


#Import Datasets

Data1RegularConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data1RegularConventional.xlsx")
View(Data1RegularConventional)
Data2RegularReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data2RegularReformulated.xlsx")
View(Data2RegularReformulated)
Data3RegularAllAreasAllFormulations <-read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/All DataBabes Data/Data3AllAreasAllFormulations.xlsx")
View(Data3AllAreasAllFormulations)
Data4MidGradeConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data4MidGradeConventional.xlsx")
View(Data4MidGradeConventional)
Data5MidGradeReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data5MidGradeReformulated.xlsx")
View(Data5MidGradeReformulated)
Data6MidGradeAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data6MidGradeAllAreasAllFormulations.xlsx")
View(Data6MidGradeAllAreasAllFormulations)
Data7PremiumConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data7PremiumConventional.xlsx")
View(Data7PremiumConventional)
Data8PremiumReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data8PremiumReformulated.xlsx")
View(Data8PremiumReformulated)
Data9PremiumAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data9PremiumAllAreasAllFormulations.xlsx")
View(Data9PremiumAllAreasAllFormulations)
Data10AllGradesConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data10AllGradesConventional.xlsx")
View(Data10AllGradesConventional)
Data11AllGradesReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data11AllGradesReformulated.xlsx")
View(Data11AllGradesReformulated)
Data12AllGradesAreasAndFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data12AllGradesAreasAndFormulations.xlsx")
View(Data12AllGradesAreasAndFormulations)
#SeriesReport_20220608110458_31191e <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110458_31191e.xlsx")
#View(SeriesReport_20220608110458_31191e)
#SeriesReport_20220608110510_4a84c3 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110510_4a84c3.xlsx")
#View(SeriesReport_20220608110510_4a84c3)
#SeriesReport_20220608110516_3befcc <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110516_3befcc.xlsx")
#View(SeriesReport_20220608110516_3befcc)
#SeriesReport_20220608110521_4d3d85 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110521_4d3d85.xlsx")
#View(SeriesReport_20220608110521_4d3d85)
#SeriesReport_20220608110525_4e78be <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110525_4e78be.xlsx")
#View(SeriesReport_20220608110525_4e78be)

#Asking the Questions
# What will future gas prices look like based on previous trends?
# How do gas prices differ among regions?

#Testing Assumptions for Regression

#1. There is a linear relationship between x and y.
#Create a Scatterplot and Look at the Shape
scatter.smooth(x = Data12AllGradesAreasAndFormulations$Date, y= Data12AllGradesAreasAndFormulations$USAllAll)

# The data is not linear
# Examine the Residuals for Patterns-- 
# The data is not exponential or logistic
# Deciding on Step-wise Regression
# Backward Model

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


#Backward Elimination

fitAll = lm(USAllAll ~ ., data = LocationModel)
summary(fitAll)
# All Coefficients are significant
# Which location is the best indicator of overall price?
step(fitAll, direction = "backward")

#Best Fit Model: lm(formula = USAllAll ~ EastCoastAllAll + NewEnglandAllAll + 
# CentralAtlanticAllAll + LowerAtlanticAllAll + MidwestAllAll + 
# GulfCoastAllAll + RockyMountainAllAll + WestCoastAllAll, 
# data = LocationModel)

#All locations are significant, and are all included in our best fit model

#How does the Grade effect the price?
#Creating another model based on Grade: Regular, MidGrade, or Premium

GradeModel <-merge(Data3, Data6, by = "Date")
GradeModel

GradeModel2 <- merge(GradeModel, Data9, by = "Date")
GradeModel2

GradeModel3 <- GradeModel2()
GradeModel3

#2. Test for Homoscedasticity: The error term is normally distributed.
lmMod <- lm(Date ~ USAllAll, data = Data12AllGradesAreasAndFormulations)
par(mfrow = c(2,2))
plot(lmMod)




#3. Homogenity of variance: the variance of the error terms is constant for all values of x.
lmtest::bptest(lmMod)
#NCV Test

car::ncvTest(lmMod)

#4. The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
# x's are dates, so yes they are fixed and constant


#5. Multicollinearity: the observations are independent.


#6. Lack of outliers

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

class(SR1)
#Data Frame 
#Subsetting for timeseries recoding

SR1a <- SR1[,1:2]
SR1TS <- ts(SR1a$Year, start = min(SR1a$Year), end = max(SR1a$Year), frequency = 1)
SR1TS

install.packages("forecast")
install.packages("tseries")


library("forecast")
library("tseries")
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

#Subsetting DModel
keeps2 <- c("Jan.x", "Jan.y", data - DModel)

FinModel <- ts(DModel$Year, start = min(DModel$Year), end = max(DModel$Year), frequency = 1)
FinModel
class(FinModel)

#Now it is the right class
unique(FinModel)

plot(FinModel)

#Testing Assumptions
#1. Data is Stationary
#Auto-corrolation test
acf(FinModel)
#Partial auto-corrolation test
pacf(FinModel)
#The data is not stationary
#Augmented Dickey-Fuller Test
adf.test(FinModel)

#data:  FinModel
#Dickey-Fuller = NaN, Lag order = 2, p-value = NA
#alternative hypothesis: stationary
#essentially perfect fit. Model may be unreliable.

#Convert data into stationary Data

NewModel = auto.arima(FinModel, ic = "aic", trace = TRUE)
NewModel

#Best Model: ARIMA(0,1,0) with drift 
#Coefficients:
#  drift  
#1  

#sigma^2 = 0.4493:  log likelihood = Inf
#AIC=-Inf   AICc=-Inf   BIC=-Inf

acf(ts(NewModel$residuals))
#partial auto correlation function

pacf(ts(NewModel$residuals))

MyForecast = forecast(NewModel, level = c(95), h = 10*1)
MyForecast

#Point Forecast    Lo 95    Hi 95
#2022           2022 2020.686 2023.314
#2023           2023 2021.142 2024.858
#2024           2024 2021.724 2026.276
#2025           2025 2022.372 2027.628
#2026           2026 2023.062 2028.938
#2027           2027 2023.782 2030.218
#2028           2028 2024.524 2031.476
#2029           2029 2025.284 2032.716
#2030           2030 2026.059 2033.941
#2031           2031 2026.845 2035.155

plot(MyForecast)

#validate forecast
Box.test(MyForecast$residuals, lag = 15, type = "Ljung-Box")
