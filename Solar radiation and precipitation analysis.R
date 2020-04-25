# Packages
library(TSA)
library(urca)
library(forecast)
library(x12)
library(dLagM)
library(Hmisc)
library(tseries)
library(car)

library(FitAR)
library(readxl)
library(tidyverse)
library(dplyr)


# loading Functions
source("GoFVals.R")
source("LEIC.R")
source("pVal.R")
source("MASEvalues.R")
source("DataProcessing.R")
source("MASE.forecast.R")

# Reading data

data_year = read_excel("M3C_reduced_2019.xlsx", sheet = 1)
class(data_year) 
head(data_year)

data_quart = read_excel("M3C_reduced_2019.xlsx", sheet = 2)
class(data_quater) 
head(data_quater)

data_month = read_excel("M3C_reduced_2019.xlsx", sheet = 3)
class(data_month) 
head(data_month)

# Yearly MICRO data
data_year$Category = as.factor(data_year$Category)
levels(data_year$Category)

#"DEMOGRAPHIC" "FINANCE"     "INDUSTRY"    "MACRO"       "MICRO"       "OTHER"     

data_year_MICRO =  data_year %>% filter(Category == "MICRO")

N1=nrow(data_year_MICRO)

data = list()

data = DataProcessing(N1,"Yearly",data_year_MICRO)

plot(data[[1]], ylab = "Data", main = "1st annual MICRO data")

acf(data[[1]], main = "Sample ACF of 1st annual MICRO data")

# Trend exist. ? seasonality

# Choose models with additive or multiplicative error and trend. No seasonality

models = c("ANN", "MNN", "AAN", "MAN","MMN")   

H = 6

GoFVals(data = data, H = H, models = models)

a=GoFVals(data = data, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output1.csv")

output1 <- read_csv("output1.csv")

Min_mase <- output1 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase$GoF.FittedModels)
#Out of 60 MICRO annual series, 19 fit MMN model and 18 fit MAN model based on lowest MASE.

# Apply the prediction validation procedure

pVal(data = data, H = H, models = models)

b = pVal(data = data, H = H, models = models)

b$best.model

table(b$best.model)
#Out of 60 MICRO annual series, 15 fit MMN model and 14 fit MAN model by prediction validation procedure.
# IC procedure and prediction validation procedure give similar model selection results.

MASEvalues(data = data, H = H, model = "MMN", MASEs = MASEs)

MASEvalues(data = data, H = H, model = "MAN", MASEs = MASEs)

# MAN has lower mean rank MASE, mean MASE and median MASE. 
# MAN model can be used to fit yearly MICRO data.

#To get the MASE over the training sample 
mase_train_MICRO = c()

for (i in 1:60) {  

training_y_MICRO_i <- as.vector(subset(data[[i]], end=length(data[[i]])-7))

test_y_MICRO_i <- as.vector(subset(data[[i]], start=length(data[[i]])-6))

y_MICRO <- ets(data[[i]], model = "MAN")

y_MICRO_fore <- forecast(y_MICRO, h = 6) %>% as.data.frame() 

y_MICRO_forecast <- as.vector(y_MICRO_fore[,1])

MASE_training <- MASE.forecast(training = training_y_MICRO_i, test = training_y_MICRO_i, forecasts = y_MICRO_forecast)

mase_train_MICRO[i]=MASE_training

print(paste(i, MASE_training))

}
mean(mase_train_MICRO)
# As per the training set results, the minimum MASE is the 25th series (MASE = 0.76107518858778)
#To get the MASE over the test sample 

y_test_micro <- c()

for (i in 1:60) {
  training_y_MICRO_i <- as.vector(subset(data[[i]], end=length(data[[i]])-7))
  
  test_y_MICRO_i <- as.vector(subset(data[[i]], start=length(data[[i]])-6))
  
  y_MICRO <- ets(data[[i]], model = "MAN")
  
  y_MICRO_fore <- forecast(y_MICRO, h = 6) %>% as.data.frame() 
  
  y_MICRO_forecast <- as.vector(y_MICRO_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_MICRO_i, test = test_y_MICRO_i, forecasts =y_MICRO_forecast)
  y_test_micro[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_micro)
# As per the testing set results, the minimum MASE is the 27th series (MASE = 0.40875056214836)

# Yearly DEMOGRAPHIC data

data_year_DEMOGRAPHIC =  data_year %>% filter(Category == "DEMOGRAPHIC")

N2=nrow(data_year_DEMOGRAPHIC)

data2 = list()

data2 = DataProcessing(N2,"Yearly",data_year_DEMOGRAPHIC)

plot(data2[[1]], ylab = "Data", main = "1st annual DEMOGRAPHIC data")

acf(data2[[1]], main = "Sample ACF of 1st annual DEMOGRAPHIC data")

# Very obvious trend. No seasonality.

# Choose models with additive or multiplicative error and trend. No seasonality.

models = c("ANN", "MNN", "AAN", "MAN", "MMN")   

H = 6

GoFVals(data = data2, H = H, models = models)

a=GoFVals(data = data2, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output2.csv")

output2 <- read_csv("output2.csv")

Min_mase_DEM<- output2 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase_DEM$GoF.FittedModels)
#Out of 105 demographic annual series, 31 fit MMN model based on the lowest MASE.

# Apply the prediction validation procedure

pVal(data = data2, H = H, models = models)

b = pVal(data = data2, H = H, models = models)

b$best.model

table(b$best.model)
#Out of 105 demographic annual series, 24 fit MMN model and 24 fit ANN respectively by prediction validation procedure.
# IC procedure and prediction validation procedure give similar model selection results.

MASEvalues(data = data2, H = H, model = "MMN", MASEs = MASEs)

MASEvalues(data = data2, H = H, model = "ANN", MASEs = MASEs)

# MMN has lower mean rank MASE, mean MASE and median MASE. 
# MMN model can be used to fit yearly demographic data.IC procedure gives more precise model selection results.

Yearly_DEMO = ets(data2[[3]], model = "MMN")
summary(Yearly_DEMO)
checkresiduals(Yearly_DEMO)

#To get the MASE over the training sample 
y_train_DEMO <- c()
for (i in 1:105) {  
  
  training_y_DEMO_i <- as.vector(subset(data2[[i]], end=length(data2[[i]])-7))
  
  test_y_DEMO_i <- as.vector(subset(data2[[i]], start=length(data2[[i]])-6))
  
  y_DEMO <- ets(data2[[i]], model = "MMN")
  
  y_DEMO_fore <- forecast(y_DEMO, h = 6) %>% as.data.frame() 
  
  y_DEMO_forecast <- as.vector(y_DEMO_fore[,1])
  
  MASE_training <- MASE.forecast(training = training_y_DEMO_i, test = training_y_DEMO_i, forecasts = y_DEMO_forecast )
  y_train_DEMO[i] <- MASE_training
  print(paste(i, MASE_training))
  
}
mean(y_train_DEMO)
#As per the training set results, the minimum MASE is the 28th series (MASE = 1.10264298170281)
#To get the MASE over the test sample 
y_test_DEMO <- c()
for (i in 1:105) {
  training_y_DEMO_i <- as.vector(subset(data2[[i]], end=length(data2[[i]])-7))
  
  test_y_DEMO_i <- as.vector(subset(data2[[i]], start=length(data2[[i]])-6))
  
  y_DEMO <- ets(data2[[i]], model = "MMN")
  
  y_DEMO_fore <- forecast(y_DEMO, h = 6) %>% as.data.frame() 
  
  y_DEMO_forecast <- as.vector(y_DEMO_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_DEMO_i, test = test_y_DEMO_i, forecasts =y_DEMO_forecast)
  y_test_DEMO[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_DEMO)
# As per the testing set results, the minimum MASE is the 104th series (MASE = 0.340241435470447)



# Yearly finance data
data_year_FINANCE = data_year %>% filter(Category == "FINANCE")

N3=nrow(data_year_FINANCE)

data3 = list()

data3 = DataProcessing(N3,"Yearly",data_year_FINANCE)

plot(data3[[1]], ylab = "Data", main = "1st annual FINANCE data")

acf(data3[[1]], main = "Sample ACF of 1st annual FINANCE data")

# Trend exist. NO seasonality

# Choose models with additive or multiplicative error and trend. No seasonality

models = c("ANN", "MNN", "AAN", "MAN","MMN")   

H = 6

GoFVals(data = data3, H = H, models = models)

a=GoFVals(data = data3, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output3.csv")

output3 <- read_csv("output3.csv")

Min_mase <- output3 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase$GoF.FittedModels)
#Out of 36 FINANCE annual series, 10 fit ANN model and 9 fit MMN model based on lowest MASE.

# Apply the prediction validation procedure

pVal(data = data3, H = H, models = models)

b = pVal(data = data3, H = H, models = models)

b$best.model

table(b$best.model)
#Out of 36 FINANCE annual series, 10 fit ANN model by prediction validation procedure.
# IC procedure and prediction validation procedure give similar model selection results.

MASEvalues(data = data3, H = H, model = "ANN", MASEs = MASEs)

MASEvalues(data = data3, H = H, model = "MMN", MASEs = MASEs)

# MMN has lower mean rank MASE, mean MASE and median MASE. 
# MMN model can be used to fit yearly FINANCE data.

#To get the MASE over the training sample 
y_train_finance <- c()
for (i in 1:36) {  
  
  training_y_FINANCE_i <- as.vector(subset(data3[[i]], end=length(data3[[i]])-7))
  
  test_y_FINANCE_i <- as.vector(subset(data3[[i]], start=length(data3[[i]])-6))
  
  y_FINANCE <- ets(data3[[i]], model = "MMN")
  
  y_FINANCE_fore <- forecast(y_FINANCE, h = 6) %>% as.data.frame() 
  
  y_FINANCE_forecast <- as.vector(y_FINANCE_fore[,1])
  
  MASE_training <- MASE.forecast(training = training_y_FINANCE_i, test = training_y_FINANCE_i, forecasts = y_FINANCE_forecast)
  y_train_finance[i] <- MASE_training
  print(paste(i, MASE_training))
  
}
mean(y_train_finance)
#As per the training set results, the minimum MASE is the 7th series (MASE = 1.43790440407558)
#To get the MASE over the test sample 
y_test_finance <- c()
for (i in 1:36) {
  training_y_FINANCE_i <- as.vector(subset(data3[[i]], end=length(data3[[i]])-7))
  
  test_y_FINANCE_i <- as.vector(subset(data3[[i]], start=length(data3[[i]])-6))
  
  y_FINANCE <- ets(data3[[i]], model = "MMN")
  
  y_FINANCE_fore <- forecast(y_FINANCE, h = 6) %>% as.data.frame() 
  
  y_FINANCE_forecast <- as.vector(y_FINANCE_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_FINANCE_i, test = test_y_FINANCE_i, forecasts =y_FINANCE_forecast)
  y_test_finance[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_finance)
# As per the testing set results, the minimum MASE is the 29th series (MASE = 0.448649456401651)


# Yearly industry data
data_year_INDUSTRY = data_year %>% filter(Category == "INDUSTRY")

N4=nrow(data_year_INDUSTRY)

data4 = list()

data4 = DataProcessing(N4,"Yearly",data_year_INDUSTRY)

plot(data4[[1]], ylab = "Data", main = "1st annual INDUSTRY data")

acf(data4[[1]], main = "Sample ACF of 1st annual FINANCE data")

# Trend exist. NO seasonality

# Choose models with additive or multiplicative error and trend. No seasonality

models = c("ANN", "MNN", "AAN", "MAN","MMN")   

H = 6

GoFVals(data = data4, H = H, models = models)

a=GoFVals(data = data4, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output4.csv")

output4 <- read_csv("output4.csv")

Min_mase <- output4 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase$GoF.FittedModels)
#Out of 38 INDUSTRY annual series, 15 fit AAN model based on lowest MASE.

# Apply the prediction validation procedure

pVal(data = data4, H = H, models = models)

d = pVal(data = data4, H = H, models = models)

d$best.model

table(d$best.model)
#Out of 38 INDUSTRY annual series, 10 fit MNN model AND 9 FIT MMN model by prediction validation procedure.

MASEvalues(data = data4, H = H, model = "AAN", MASEs = MASEs)

MASEvalues(data = data4, H = H, model = "MMN", MASEs = MASEs)

MASEvalues(data = data4, H = H, model = "MNN", MASEs = MASEs)

# AAN has lower mean rank MASE, mean MASE and median MASE. 
# AAN model can be used to fit yearly INDUSTRY data.

#To get the MASE over the training sample 
y_train_industry <- c()
for (i in 1:38) {  
  
  training_y_INDUSTRY_i <- as.vector(subset(data4[[i]], end=length(data4[[i]])-7))
  
  test_y_INDUSTRY_i <- as.vector(subset(data4[[i]], start=length(data4[[i]])-6))
  
  y_INDUSTRY <- ets(data4[[i]], model = "AAN")
  
  y_INDUSTRY_fore <- forecast(y_INDUSTRY, h = 6) %>% as.data.frame() 
  
  y_INDUSTRY_forecast <- as.vector(y_INDUSTRY_fore[,1])
  
  MASE_training <- MASE.forecast(training = training_y_INDUSTRY_i, test = training_y_INDUSTRY_i, forecasts = y_INDUSTRY_forecast)
  y_train_industry[i] <- MASE_training
  print(paste(i, MASE_training))
  
}
mean(y_train_industry)
#As per the training set results, the minimum MASE is the 33rd series (MASE = 0.846510340018795)
#To get the MASE over the test sample 
y_test_industry <- c()
for (i in 1:38) {
  training_y_INDUSTRY_i <- as.vector(subset(data4[[i]], end=length(data4[[i]])-7))
  
  test_y_INDUSTRY_i <- as.vector(subset(data4[[i]], start=length(data4[[i]])-6))
  
  y_INDUSTRY <- ets(data4[[i]], model = "AAN")
  
  y_INDUSTRY_fore <- forecast(y_INDUSTRY, h = 6) %>% as.data.frame() 
  
  y_INDUSTRY_forecast <- as.vector(y_INDUSTRY_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_INDUSTRY_i, test = test_y_INDUSTRY_i, forecasts =y_INDUSTRY_forecast)
  y_test_industry[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_industry)
# As per the testing set results, the minimum MASE is the 28th series (MASE = 0.226839426168679)

#Yearly MACRO DATA
data_year_MACRO =  data_year %>% filter(Category == "MACRO")

N5=nrow(data_year_MACRO)

data5 = list()

data5 = DataProcessing(N5,"Yearly",data_year_MACRO)

plot(data5[[1]], ylab = "Data", main = "1st annual MACRO data")

acf(data5[[1]], main = "Sample ACF of 1st annual MACRO data")

# Trend exist. ? seasonality

# Choose models with additive or multiplicative error and trend. No seasonality

models = c("ANN", "MNN", "AAN", "MAN","MMN")   

H = 6

GoFVals(data = data5, H = H, models = models)

a=GoFVals(data = data5, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output5.csv")

output5 <- read_csv("output5.csv")

Min_mase <- output5 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase$GoF.FittedModels)
#Out of 83 MACRO annual series, 32 fit AAN model and 30 fit MAN model based on lowest MASE.

# Apply the prediction validation procedure

pVal(data = data5, H = H, models = models)

E = pVal(data = data5, H = H, models = models)

E$best.model

table(E$best.model)
#Out of 83 MACRO annual series, 38 fit MMN model by prediction validation procedure.

MASEvalues(data = data5, H = H, model = "AAN", MASEs = MASEs)

MASEvalues(data = data5, H = H, model = "MMN", MASEs = MASEs)

#AAN has slightly lower mean rank MASE and mean MASE. Its median MASE is slightly higher 
#than MMN. 
# AAN model can be used to fit yearly MACRO data.

#To get the MASE over the training sample 
y_train_MACRO <- c()
for (i in 1:83) {  
  
  training_y_MACRO_i <- as.vector(subset(data5[[i]], end=length(data5[[i]])-7))
  
  test_y_MACRO_i <- as.vector(subset(data5[[i]], start=length(data5[[i]])-6))
  
  y_MACRO <- ets(data5[[i]], model = "AAN")
  
  y_MACRO_fore <- forecast(y_MACRO, h = 6) %>% as.data.frame() 
  
  y_MACRO_forecast <- as.vector(y_MACRO_fore[,1])
  
  MASE_training <- MASE.forecast(training = training_y_MACRO_i, test = training_y_MACRO_i, forecasts = y_MACRO_forecast)
  y_train_MACRO[i] <- MASE_training
  print(paste(i, MASE_training))
  
}
mean(y_train_MACRO)
# As per the training set results, the minimum MASE is the 52nd series (MASE = 1.320708843762)
#To get the MASE over the test sample 
y_test_MACRO <- c()
for (i in 1:83) {
  training_y_MACRO_i <- as.vector(subset(data5[[i]], end=length(data5[[i]])-7))
  
  test_y_MACRO_i <- as.vector(subset(data5[[i]], start=length(data5[[i]])-6))
  
  y_MACRO <- ets(data5[[i]], model = "AAN")
  
  y_MACRO_fore <- forecast(y_MACRO, h = 6) %>% as.data.frame() 
  
  y_MACRO_forecast <- as.vector(y_MACRO_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_MACRO_i, test = test_y_MACRO_i, forecasts =y_MACRO_forecast)
  y_test_MACRO[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_MACRO)
# As per the testing set results, the minimum MASE is the 56th series (MASE = 0.59448979801613)

#Yearly OTHER DATA

data_year_OTHER =  data_year %>% filter(Category == "OTHER")

N6=nrow(data_year_OTHER)

data6 = list()

data6 = DataProcessing(N6,"Yearly",data_year_OTHER)

plot(data6[[1]], ylab = "Data", main = "1st annual OTHER data")

acf(data6[[1]], main = "Sample ACF of 1st annual OTHER data")

# Trend exist? seasonality?

# Choose models with additive or multiplicative error. 

models = c("ANN", "MNN", "AAN", "MAN","MMN")   

H = 6

GoFVals(data = data6, H = H, models = models)

a=GoFVals(data = data6, H = H, models = models)

MASEs = a$GoF$MASE

write.csv(a,"output6.csv")

output6 <- read_csv("output6.csv")

Min_mase <- output6 %>% group_by(GoF.series) %>% filter(GoF.series, GoF.MASE == min(GoF.MASE))  
#Best model for each series by minimum MASE

table(Min_mase$GoF.FittedModels)
#Out of 11 OTHER annual series, 5 fit MAN model based on lowest MASE.

# Apply the prediction validation procedure

pVal(data = data6, H = H, models = models)

Result = pVal(data = data6, H = H, models = models)

Result$best.model

table(Result$best.model)
#Out of 11 OTHER annual series, 4 fit ANN model by prediction validation procedure.

MASEvalues(data = data6, H = H, model = "MAN", MASEs = MASEs)

MASEvalues(data = data6, H = H, model = "ANN", MASEs = MASEs)

#MAN AND ANN have very similar mean rank MASE, mean MASE and median MASE. (MAN's slightly lower)
# MAN model can be used to fit yearly OTHER data.

#To get the MASE over the training sample 
y_train_other <- c()
for (i in 1:11) {  
  
  training_y_OTHER_i <- as.vector(subset(data6[[i]], end=length(data6[[i]])-7))
  
  test_y_OTHER_i <- as.vector(subset(data6[[i]], start=length(data6[[i]])-6))
  
  y_OTHER <- ets(data6[[i]], model = "MAN")
  
  y_OTHER_fore <- forecast(y_OTHER, h = 6) %>% as.data.frame() 
  
  y_OTHER_forecast <- as.vector(y_OTHER_fore[,1])
  
  MASE_training <- MASE.forecast(training = training_y_OTHER_i, test = training_y_OTHER_i, forecasts = y_OTHER_forecast)
  y_train_other[i] <- MASE_training
  print(paste(i, MASE_training))
  
}
mean(y_train_other)
# As per the training set results, the minimum MASE is the 8th series (MASE = 0.646658259153328)
#To get the MASE over the test sample 
y_test_other <- c()
for (i in 1:11) {
  training_y_OTHER_i <- as.vector(subset(data6[[i]], end=length(data6[[i]])-7))
  
  test_y_OTHER_i <- as.vector(subset(data6[[i]], start=length(data6[[i]])-6))
  
  y_OTHER <- ets(data6[[i]], model = "MAN")
  
  y_OTHER_fore <- forecast(y_OTHER, h = 6) %>% as.data.frame() 
  
  y_OTHER_forecast <- as.vector(y_OTHER_fore[,1])
  
  MASE_test <- MASE.forecast(training = training_y_OTHER_i, test = test_y_OTHER_i, forecasts =y_OTHER_forecast)
  y_test_other[i] <- MASE_test
  print(paste(i, MASE_test))
  
}
mean(y_test_other)
# As per the testing set results, the minimum MASE is the 9th series (MASE = 0.53980584347656)

# for whole year data (MMN)
N = nrow(data_year)
N

data = list()

data = DataProcessing(N,"Yearly",data_year)

mase_train = c()
for (i in 1:N) {  
  training_y_i <- as.vector(subset(data[[i]], end=length(data[[i]]-7)))
  
  Model <- ets(data[[i]], model = "MMN")
  
  Model_forc <- forecast(Model, h = 6) %>% as.data.frame() 
  
  Model_forecast <- as.vector(Model_forc[,1])
  
  MASE_test <- MASE.forecast(training = training_y_i, test = training_y_i, forecasts = Model_forecast)
  
  mase_train[i]=MASE_test
  
  print(paste(i, MASE_test))
}
mean(mase_train)
# Mase over training sample

mase_test=c()
for (i in 1:N) {
  training_y_i = as.vector(subset(data[[i]], end=length(data[[i]]-7)))
  
  test_y_i = as.vector(subset(data[[i]], end=length(data[[i]])-6))
  
  Model <- ets(data[[i]], model = "MmN")
  
  Model_forc <- forecast(Model, h = 6) %>% as.data.frame() 
  
  Model_forecast <- as.vector(Model_forc[,1])
  
  MASE_test <- MASE.forecast(training = training_y_i, test = test_y_i, forecasts = Model_forecast)
  
  mase_test[i]=MASE_test
  print(paste(i, MASE_test))
  
}
mean(mase_test)
