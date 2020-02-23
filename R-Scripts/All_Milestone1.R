#####               #####
##### Nick Edington #####
#####               #####

install.packages('tidyverse')
library(tidyverse)

###GET DATA###
all = read.csv("datasets/PRSA_Beijing_All.csv")
str(all_no_na)

###Clean Data### 

#Clean out all observations with NA for the PM2.5 and temp measurement
all_no_na = all %>% filter((!(is.na(PM2.5))) & (!(is.na(TEMP))))

#Check distribution
summary(all_no_na$PM2.5)
boxplot(all_no_na$PM2.5)
ggplot(all_no_na, mapping=aes(PM2.5)) + 
  geom_histogram(bins = 100, color='red', fill='yellow') + theme_bw()
#Considering we will be transforming this data with (>=35) to true false values we will no remove outliers

###Transform Data###

#Turn day, month and year into single date attribute
all_no_na$Date = format(as.Date(ISOdate(all_no_na$year, all_no_na$month, all_no_na$day)), "%Y/%m/%d")
str(all_no_na)

#Group by station and day and for each get the average daily amount of PM2.5 and average temp
avg_pm_day_station = all_no_na %>% group_by(Date, station) %>% 
  summarise(meanPM2.5 = mean(PM2.5), meanTemp = mean(TEMP))
head(avg_pm_day_station)
#Distributions of average daily PM2.5 by station 
ggplot(avg_pm_day_station, mapping=aes(meanPM2.5)) + 
  geom_histogram(bins = 100, color='red', fill='yellow') + facet_wrap(. ~ station)

#Transform average pm2.5 to true false value
avg_pm_day_station$safe = avg_pm_day_station$meanPM2.5 < 35
head(avg_pm_day_station)
#Distributions of safe days by station
ggplot(avg_pm_day_station, mapping=aes(station, fill=factor(safe))) + 
  geom_bar()

#Add day of year column
avg_pm_day_station$day_of_year = as.numeric(strftime(avg_pm_day_station$Date, "%j"))
ggplot(avg_pm_day_station, mapping=aes(day_of_year, fill=factor(safe))) + 
  geom_bar()

#Add month of year column
avg_pm_day_station$month_of_year = as.numeric(strftime(avg_pm_day_station$Date, "%m"))
ggplot(avg_pm_day_station, mapping=aes(month_of_year, fill=factor(safe))) + 
  geom_bar()

#Add year column
avg_pm_day_station$year = as.numeric(strftime(avg_pm_day_station$Date, "%Y"))
ggplot(avg_pm_day_station, mapping=aes(year, fill=factor(safe))) + 
  geom_bar()

###Create Models###

#Safety vs day of year
model1 = glm(safe~ day_of_year, avg_pm_day_station, family="binomial")
summary(model1)

#Safety vs month of year
model2 = glm(safe~ month_of_year, avg_pm_day_station, family="binomial")
summary(model2)

#Safety vs year
model3 = glm(safe~ year, avg_pm_day_station, family="binomial")
summary(model3)

#Safety vs day of year, month of year and year
model4 = glm(safe~ day_of_year + month_of_year + year, avg_pm_day_station, family="binomial")
summary(model4)

#Safety vs average temp
model5 = glm(safe~ meanTemp, avg_pm_day_station, family="binomial")
summary(model5)

###DRAW MODELS###

##Model 3##
#Create a data frame based around the prediction of safety 
predicted3 = data.frame(prob_safe=model3$fitted.values, safe=avg_pm_day_station$safe, year=avg_pm_day_station$year)
#Create a pot of the probability of it being safe based on the temperature
ggplot(predicted3, aes(year,prob_safe)) + geom_line()
#Order the predictions in increasing order of probibility
predicted5 = predicted3[order(predicted3$prob_safe, decreasing=FALSE),]
#Add a rank to the predictions
predicted3$rank = 1:nrow(predicted)
#Plot the distribution of predictions based on rank, not sure if this is useful
ggplot(predicted3, aes(rank,prob_safe)) + geom_point( alpha=1, shape=4, stroke=2)

##Model 5##
#Create a data frame based around the prediction of safety 
predicted5 = data.frame(prob_safe=model5$fitted.values, safe=avg_pm_day_station$safe, meanTemp=avg_pm_day_station$meanTemp)
#Create a pot of the probability of it being safe based on the temperature
ggplot(predicted5, aes(meanTemp,prob_safe)) + geom_line()
#Order the predictions in increasing order of probibility
predicted5 = predicted5[order(predicted5$prob_safe, decreasing=FALSE),]
#Add a rank to the predictions
predicted5$rank = 1:nrow(predicted)
#Plot the distribution of predictions based on rank, not sure if this is useful
ggplot(predicted5, aes(rank,prob_safe)) + geom_point( alpha=1, shape=4, stroke=2) 

# Possible to model PM2.5 against models:
# Day of year
# Month
# Year
# Station
# Average Temp
# Average daytime temp
# Average Nighttime temp
# Average Windspeed
# Total rainfall
# Recent rainfall
# Dew point
# 


#####                #####
##### Salik Hussaini #####
#####                #####

library(readr)
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(magrittr)

# Data loading and cleaning #### 

pr <- read_csv("GitHub/Boston-Crime_Linear-Regression_Analysis/Bejing_Air_Analysis/Data/PRSA_Beijing_All.csv", na = "NA")
head(pr)
str(pr)

#Format as Date Variable
pr$date <- as.Date(pr$date, "%m/%d/%Y")
str(pr)
head(pr)

#Convert to data frame 
pr <- as.data.frame(pr)

pr2 <- pr

pr2$date <- as.numeric(pr2$date)
head(pr2)

pr1 <- as.data.frame(pr2)

pr3 <- pr2 %>% select_if(is.numeric)
pr1 <- pr %>% select_if(is.numeric)

### Variable Analysis ####


#Principal Component Analysis of non numerical date
p1 <- principal(pr1, nfactors = 4, covar = TRUE)
print(p1)

#Principal Component Analysis of numerical date
p2 <- principal(pr3, nfactors = 4, covar = TRUE)
print(p2)

# Model Building #### 

#Regression
m1 <- lm(pr1$PM2.5 ~ ., data = pr1)
summary(m1)

#Regression of non numeric Date to predict pm2.5
m2 <- lm(pr$PM2.5 ~ pr$year + pr$month + pr$day + pr$hour
         + pr$date, data = pr)

summary(m2)
#Regression of non numeric Date to predict pm10
m3 <- lm(pr$PM10 ~ pr$year + pr$month + pr$day + pr$hour
         + pr$date, data = pr)
summary(m3)

#vif

pc <- cor(pr1, use = "complete.obs")
pc <- as.data.frame(pc)
sort(pc$date)

pr1 <- as.data.frame(pr)
pr1 <- pr1 %>% select_if(is.numeric)
str(pr1)


p1 <- principal(pr1, nfactors = 4, covar = TRUE)

print(p1)

pr %>%
  unite(dmy, pr$day, pr$month, pr$year) %>%
  transmute(dmy = dmy(dmy))

(time_plot <- ggplot(pr, aes(x = pr$date, y = pr$TEMP)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())
(time_plot2 <- ggplot(pr, aes(x = pr$date, y = pr$PM2.5)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())



library(ggplot2)

#####                #####
##### Pete Gillespie #####
#####                #####

#import dfs
d1 <- Aotizhongxin
d2 <- Changping
d3 <- Dingling
d4 <- Dongsi
d5 <- Guanyuan
d6 <- Gucheng
d7 <- Huairou
d8 <- Nongzhanguan
d9 <- Shunyi
d10 <- Tiantan
d11 <- Wanliu
d12 <- Wanshouxigong
df <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
df <- Beijing_All

#removes id columns
df <- df[,-c(1)]

### apply yearly averages, and merge by year back into dataset for each df###
library(tidyverse)
PM2.5_Yr_Avg <- tapply(d12$PM2.5,d12$year,mean,na.rm=TRUE)
PM10_Yr_Avg <- tapply(d12$PM10,d12$year,mean,na.rm=TRUE)

PM10DF <- as.data.frame(PM10_Yr_Avg)
year <- c(2013,2014,2015,2016,2017)
PM10DF$year <- year

d12 <- merge(d12, PM10DF, by = 'year')

PM25DF <- as.data.frame(PM2.5_Yr_Avg)
year <- c(2013,2014,2015,2016,2017)
PM25DF$year <- year

d12 <- merge(d12, PM25DF, by = 'year')

#plot 
plot(PM2.5_Average_Levels, type = "b", main = "Monthly Average PM2.5 Levels (ug/m^3)", xlab = "Month", col = "red", lwd = 5)
plot(PM10_Average_Levels, type = "b", main = "Monthly Average PM10 Levels (ug/m^3)", xlab = "Month", col = "green", lwd = 5)

### ggplot visualizations ###
library("ggplot2")

#Line plots
ggplot(data = df, aes(x=year, y=PM2.5_Yr_Avg, colour = station, group = station)) + geom_line() + ggtitle("Yearly Average PM2.5 Levels")
ggplot(data = df, aes(x=year, y=PM10_Yr_Avg, colour = station, group = station)) + geom_line() + ggtitle("Yearly Average PM10 Levels")


#blob plot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(df, aes(year, PM10))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="Particle Size: Year vs PM2.5 Levels", 
       y="PM2.5", 
       x="Year", 
       title="Counts Plot")

#density plot
ggplot(df, aes(PM10)) + geom_density(aes(fill=factor(station)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="PM10 Levels grouped by station",
       x="PM10 Level",
       fill="station")

#bubble plot
g <- ggplot(df, aes(year,PM2.5_Yr_Avg,PM10_Yr_Avg)) + 
  labs(subtitle="Year vs. PM10 Yearly Average",
       title="Bubble chart")

g + geom_jitter(aes(col=station)) 

### Split into train and test 80/20 split ###
s <- sample(420768, 336614)
Beijing_Train <- df[s,]
Beijing_Test <- df[-s,]
write_csv(Beijing_Train, 'C:/Users/P3DR0/Desktop/Classes/CSC/DSC441/Group Project/Individual Stuff/Beijing_Train.csv')
write_csv(Beijing_Test, 'C:/Users/P3DR0/Desktop/Classes/CSC/DSC441/Group Project/Individual Stuff/Beijing_Test.csv')

### Initial Classification Models to classify station based on PM2.5 and PM10 with year ### 
library(rpart)
library(rattle)

#PM2.5 Model
model1 = rpart(station ~ PM2.5_Yr_Avg + year, method = 'class', data=Beijing_Train)
summary(model1)
fancyRpartPlot(model1)  

#PM10 Model
model2 = rpart(station ~ PM10_Yr_Avg + year, method = 'class', data=Beijing_Train)
summary(model2)
fancyRpartPlot(model2)

#Test against predictions
p1 <- predict(model1, Beijing_Test, type = "class")
table(Beijing_Test[,17], p1)

p2 <- predict(model2, Beijing_Test, type = "class")
table(Beijing_Test[,17], p2)

#####             #####
##### Zoya Hafeez #####
#####             #####

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