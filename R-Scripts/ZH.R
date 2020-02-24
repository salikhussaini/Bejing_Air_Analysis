library(readr)
PRSA_Beijing_All <- read_csv("C:/Users/User/Desktop/DSC441/Data_Sets/PRSA_Beijing_All.csv")
View(PRSA_Beijing_All)
library(tidyverse)
all_no_na = PRSA_Beijing_All %>% filter((!(is.na(PM2.5))) & (!(is.na(SO2)))& (!(is.na(NO2)))& (!(is.na(CO)))& (!(is.na(O3))))
attach(all_no_na)

#Multiple Linear Regression Model - First Order Term 

#Model1 [PM10 VS SO2,NO2,CO,Temprature, Pressure, Day, Month, Year, DewPoint, Rain & WSPM]
model <- lm(PM10~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM)
summary(model)

#Model2 [PM2.5 VS SO2,NO2,CO,Temprature, Pressure, Day, Month, Year, DewPoint, Rain & WSPM]
mode2 <- lm(PM2.5~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM)
summary(model)

#Visualization 
library(ggplot2)
# sCATTERPLOTS FOR PREDICTORS WITH HIGHER T STAISTICS 

#PM10 VS NO2
ggplot(model,aes(x=NO2,y=PM10))+geom_point()+stat_smooth(method = "lm",se=FALSE)

#PM10 VS CO
ggplot(model,aes(x=NO2,y=PM10))+geom_point()+stat_smooth(method = "lm",se=FALSE)

#PM10 VS WSPM
ggplot(model,aes(x=WSPM,y=PM10))+geom_point()+stat_smooth(method = "lm",se=FALSE)


# PM2.5 VS SO2
ggplot(model,aes(x=SO2,y=PM2.5))+geom_point()+stat_smooth(method = "lm",se=FALSE)

#PM10 VS NO2
ggplot(model,aes(x=NO2,y=PM2.5))+geom_point()+stat_smooth(method = "lm",se=FALSE)

#PM10 VS CO
ggplot(model,aes(x=NO2,y=PM2.5))+geom_point()+stat_smooth(method = "lm",se=FALSE)