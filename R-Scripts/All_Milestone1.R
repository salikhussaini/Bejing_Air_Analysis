#####               #####
##### Nick Edington #####
#####               #####
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