install.packages('tidyverse')
install.packages('caTools')
install.packages('caret')
install.packages("rpart")
install.packages('rattle')
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)

##################GET DATA##################
all = read.csv("datasets/PRSA_Beijing_All.csv")

##################Clean Data##################

#Clean out all observations with NA for the PM2.5 and temp measurement
all_no_na = all %>% filter((!(is.na(PM2.5))) & (!(is.na(TEMP)))& (!(is.na(WSPM))) & (!(is.na(PRES))) & (!(is.na(DEWP))) & (!(is.na(RAIN))))
#Check distribution
summary(all_no_na$PM2.5)
ggplot(all_no)
ggplot(all_no_na, mapping=aes(y=PM2.5)) + 
  geom_boxplot(color='blue', fill='yellow') + theme(text = element_text(size=20))
ggplot(all_no_na, mapping=aes(PM2.5)) + 
  geom_histogram(bins = 50, color='blue', fill='yellow')  + theme(text = element_text(size=20))

#Considering we will be transforming this data with (>=35) to true false values we will not remove outliers

###############Transform Data###############

#Turn day, month and year into single date attribute
all_no_na$Date = format(as.Date(ISOdate(all_no_na$year, all_no_na$month, all_no_na$day)), "%Y/%m/%d")

#Group by station and day and for each get the average daily amount of PM2.5 and average temp
avg_pm_day_station = all_no_na %>% group_by(Date, station) %>% 
  summarise(meanPM2.5 = mean(PM2.5), meanTemp = mean(TEMP), avgWSPM = mean(WSPM),
            ttlRain = sum(RAIN), avgDew = mean(DEWP), meanPRES = mean(PRES))
#Distributions of average daily PM2.5 by station 
ggplot(avg_pm_day_station, mapping=aes(meanPM2.5)) + 
  geom_histogram(bins = 100, color='blue', fill='yellow') + facet_wrap(. ~ station)
#Scatter plots of average daily PM2.5 vs, mean temp, average windspeed, total rain
ggplot(avg_pm_day_station, mapping=aes(x=meanPM2.5, y=meanTemp)) + 
  geom_point(color='red')
ggplot(avg_pm_day_station, mapping=aes(x=meanPM2.5, y=avgWSPM)) + 
  geom_point(color='red')
ggplot(avg_pm_day_station, mapping=aes(x=meanPM2.5, y=ttlRain)) + 
  geom_point(color='red')

#Transform average pm2.5 to true false value
avg_pm_day_station$safe = avg_pm_day_station$meanPM2.5 < 35
#Distributions of safe days by station
ggplot(avg_pm_day_station, mapping=aes(station, fill=factor(safe))) + 
  geom_bar()  + theme(text = element_text(size=20)) + coord_flip()
#Distributions of safe days by average temp
ggplot(avg_pm_day_station, mapping=aes(avgWSPM)) + 
  geom_histogram()

#Add day of year column
avg_pm_day_station$day_of_year = as.numeric(strftime(avg_pm_day_station$Date, "%j"))
#Distrubution of safe days by day of year
ggplot(avg_pm_day_station, mapping=aes(day_of_year, fill=factor(safe))) + 
  geom_bar()

#Add month of year column
avg_pm_day_station$month_of_year = as.numeric(strftime(avg_pm_day_station$Date, "%m"))
#Distribution of safe days by month of year
ggplot(avg_pm_day_station, mapping=aes(month_of_year, fill=factor(safe))) + 
  geom_bar()

#Add year column
avg_pm_day_station$year = as.numeric(strftime(avg_pm_day_station$Date, "%Y"))
#Distributions of safe days by year
ggplot(avg_pm_day_station, mapping=aes(year, fill=factor(safe))) + 
  geom_bar()

#Have to remove meanPM2.5, Date, ungroup and add an ID to be able to split data
safe_day_station = avg_pm_day_station %>% ungroup %>% select(- Date) %>% select(-meanPM2.5) %>% mutate(id = row_number())
#view new structure
safe_day_station %>% str()

#Split data into training and test data
train <- safe_day_station %>% dplyr::sample_frac(.75)
test <- dplyr::anti_join(safe_day_station, train, by = 'id')


##################Train Models##################

#All values m1
m1 = rpart(safe ~ . - id , method = 'class', data=train)

#Without day of year m2
m2 = rpart(safe ~ . -day_of_year - id, method = 'class', data=train)

#Without dew m3
m3 = rpart(safe ~ . -avgDew -id, method = 'class', data=train)

#Day of year, dewpoint and year m4
m4 = rpart(safe ~ day_of_year + avgDew + year, method = 'class', data=train)

#Just windspeed m5
m5 = rpart(safe ~ avgWSPM -id, method = 'class', data=train)

#Just pressure m6
m6 = rpart(safe ~ meanPRES, method = 'class', data=train)

#Without temp m7
m7 = rpart(safe ~ . - id - meanTemp , method = 'class', data=train)

#Without temp or rainm8
m8 = rpart(safe ~  day_of_year + avgDew + year + meanPRES, method = 'class', data=train)

##################TEEST MODELS##################

#Create list of models
models = list(m1,m2, m3, m4, m5, m6, m7, m8)

#Create confusion matrices
lapply(models, function(x) {
  pred = predict(x, test, type="class");
  confusionMatrix(pred, as.factor(test$safe), positive="TRUE")
})

#Get values for cm
lapply(models, function(x) {
  pred = predict(x, test, type="class");
  cm = confusionMatrix(pred, as.factor(test$safe), positive="TRUE")
  cm
  cm$byClass
})

#Base Model Accuracy
sum(test$safe == FALSE) / length(pred)

##################PLOT MODELS##################
#Hearby named to as "Model 1"
rpart.plot(m8,cex=.9, under = TRUE, type=3)

#Hearby named to as "Model 2"
rpart.plot(m4,cex=.9, under = TRUE, type=3)

