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