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
# Importing packages ####
library(tidyverse) # metapackage with lots of helpful functions
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(magrittr)
library(corrplot)
library(ggfortify)
library(MASS)
library(caret)
library(imputeTS)
# Data loading and cleaning #### 

pr <- read_csv("GitHub/Bejing_Air_Analysis/Data/PRSA_Beijing_All.csv", 
               na = "empty")
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

#Choose numerical data
pr <- as.data.frame(pr)
pr1 <- pr %>% select_if(is.numeric)

#Mising Data
Missing <- pr %>% summarise_all(~(sum(is.na(.))/n()))
Missing <- gather(Missing, key = "variables", value = "percent_missing")

pr$PM2.5 = na_mean(pr$PM2.5)
pr$PM10 = na_mean(pr$PM10)
Missing

## Variable Analysis ####
abc <- c('year','month','day','hour', 'PM2.5', 'PM10', 'TEMP', 'PRES', 'DEWP')
pr2 <- pr1[abc]
head(pr2)

#Correaltions
pc <- cor(pr1, use = "complete.obs")
pc <- as.data.frame(pc)

#PCA
p2 <- principal(pr2, nfactors = 3, covar = TRUE)
print(p2)

#PCA
p1 <- principal(pr1, nfactors = 3, covar = TRUE)
print(p1)

#Principal Component Analysis of non numerical date
p1 <- principal(pr1, nfactors = 4, covar = TRUE)
print(p1)

#Principal Component Analysis of numerical date
p2 <- principal(pr3, nfactors = 4, covar = TRUE)
print(p2)

### Model Building #### 

#Regression of non numeric Date to predict pm2.5
m3 <- lm(pr$PM2.5 ~ pr$year + pr$TEMP + pr$SO2 + pr$O3 + pr$WSPM)
summary(m3)

m4 <- lm(pr$PM10 ~ pr$year + pr$TEMP + pr$SO2 + pr$O3 + pr$WSPM)
summary(m4)

#Cross Validation
#PM2.5
data_ctrl <- trainControl(method = "cv", number = 5)
m5 <- train(PM2.5 ~ year + TEMP + SO2 + O3,   # model to fit
            data = pr,                        
            trControl = data_ctrl,              # folds
            method = "lm",                      # specifying regression model
            na.action = na.pass)                # pass missing data to model - some models will handle this

#PM10
m6 <- train(PM10 ~ year + TEMP + SO2 + O3,   # model to fit
            data = pr,                        
            trControl = data_ctrl,              # folds
            method = "lm",                      # specifying regression model
            na.action = na.pass)                # pass missing data to model - some models will handle this


#Model Summary PM2.5
m5
m5$finalModel
#Model Summary PM10
m6
m6$finalModel


####Visulizations####

#Histogram PM2.5
ggplot(pr, aes(PM2.5)) +
  geom_histogram(bins = 15, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of PM2.5")
theme(axis.title = element_text(), axis.title.x = element_text())

#Histogram Pm10
ggplot(pr, aes(PM10)) +
  geom_histogram(bins = 15, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of PM10")



#BoxPlot PM2.5 By station
ggplot(pr, aes(x = pr$station, y = pr$`PM2.5`)) +
  geom_boxplot(aes(fill = station)) + scale_y_log10() +
  xlab("station") + 
  ylab("PM2.5") +
  ggtitle("Boxplots of PM2.5 by station") 
#BoxPlot PM10 By station
ggplot(pr, aes(x = pr$station, y = pr$PM10)) +
  geom_boxplot(aes(fill = station)) + scale_y_log10() +
  xlab("station") + 
  ylab("PM10") +
  ggtitle("Boxplots of PM10 by station")


pr %>%
  unite(dmy, pr$day, pr$month, pr$year) %>%
  transmute(dmy = dmy(dmy))

#Temp vs Date
(time_plot <- ggplot(pr, aes(x = pr$date, y = pr$TEMP)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())
#PM2.5 vs Date
(time_plot2 <- ggplot(pr, aes(x = pr$date, y = pr$PM2.5)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())
theme(axis.title = element_text(), axis.title.x = element_text()) 

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
df <- Beijing_Train2
df <- Beijing_Test2

#removes id columns
df <- df[,-c(5,6)]
df <- na.omit(df)

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
PM25DF

d12 <- merge(d12, PM25DF, by = 'year')

###add monthly averages
d1 = aggregate(PM2.5 ~ station + year + month, df, mean,na.rm=TRUE)
colnames(d1)[4] <- "PM2.5_Mon_Avg"
d1
df <- merge(df, d1, by = c('station','year','month'))
df

#plot 
plot(PM2.5_Average_Levels, type = "b", main = "Monthly Average PM2.5 Levels (ug/m^3)", xlab = "Month", col = "red", lwd = 5)
plot(PM10_Average_Levels, type = "b", main = "Monthly Average PM10 Levels (ug/m^3)", xlab = "Month", col = "green", lwd = 5)

### ggplot visualizations ###
library("ggplot2")
library(gganimate)
theme_set(theme_bw())

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

#2017 Plot
p1 <- ggplot(
  df, 
  aes(x = df$station, y=df$PM2.5_Yr_Avg, fill = Level)
) +
  geom_bar(show.legend = TRUE, alpha = 0.7, stat = "identity") +
  scale_fill_brewer(palette="Set1")+
  labs(x = "Station", y = "PM2.5 Average Levels 2017")
p1

#Animation
p <- ggplot(
  df, 
  aes(x = df$station, y=df$PM2.5_Yr_Avg, fill = Level)
) +
  geom_bar(show.legend = TRUE, alpha = 0.7, stat = "identity") +
  scale_fill_brewer(palette="Set1")+
  labs(x = "Station", y = "PM2.5 Yearly Avg.") + 
  transition_time(year) +
  labs(title = "Year: {frame_time}")+
  ease_aes('linear')

animate(p, duration = 4, fps = 30, width = 800, height = 500, renderer = gifski_renderer())
anim_save("output.gif")

### Split into train and test 80/20 split ###
s <- sample(420768, 336614)
Beijing_Train <- df[s,]
Beijing_Test <- df[-s,]
write_csv(Beijing_Train, 'C:/Users/P3DR0/Desktop/Classes/CSC/DSC441/Group Project/Individual Stuff/Beijing_Train2.csv')
write_csv(Beijing_Test, 'C:/Users/P3DR0/Desktop/Classes/CSC/DSC441/Group Project/Individual Stuff/Beijing_Test2.csv')

### Initial Classification Models to classify station based on PM2.5 and PM10 with year ### 
library(rpart)
library(rattle)

#PM2.5 Base Model with all variables
model1 = rpart(station ~ ., method = 'class', data=Beijing_Train)
summary(model1)
fancyRpartPlot(model1)  

#PM2.5 Yearly Avg. Model
model2 = rpart(station ~ PM2.5_Yr_Avg + year, method = 'class', data=Beijing_Train)
summary(model2)
fancyRpartPlot(model2) 

#PM10 Model Yearly Avg. Model
model3 = rpart(station ~ PM10_Yr_Avg + year, method = 'class', data=Beijing_Train)
summary(model3)
fancyRpartPlot(model3)

#PM2.5 Monthly Avg. Model
model4 = rpart(station ~ PM2.5_Mon_Avg + year, method = 'class', data=Beijing_Train1)
summary(model4)
fancyRpartPlot(model4)

#PM2.5 Model based on so2, no2, co, o3
model5 = rpart(station ~ PM2.5_Yr_Avg + year + SO2 + NO2 + CO + O3, method = 'class', data=Beijing_Train)
summary(model5)
fancyRpartPlot(model5)

#Group level Model
model6 = rpart(Level ~ year + month + day + hour + SO2 + NO2 + O3 + CO + TEMP + PRES, method = 'class', data=Beijing_Train2)
summary(model6)
fancyRpartPlot(model6)

#Test against predictions. Confusion Matrices:
p1 <- predict(model1, Beijing_Test, type = "class")
table(Beijing_Test[,17], p1)

p2 <- predict(model2, Beijing_Test, type = "class")
table(Beijing_Test[,17], p2)

p3 <- predict(model3, Beijing_Test, type = "class")
table(Beijing_Test[,17], p3)

p4 <- predict(model4, Beijing_Test1, type = "class")
table(Beijing_Test1[,1], p4)

p5 <- predict(model5, Beijing_Test, type = "class")
table(Beijing_Test[,17], p5)

p6 <- predict(model6, Beijing_Test2, type = "class")
table(Beijing_Test2[,20], p6)

#Accuracy
A <- (6860 + 6750 + 7080 + 6702 + 6729 + 6747 + 6696 + 6815 + 6614 + 6660 + 7022 + 6672 ) / 84154
A

#Precision, Recall, F1 ():
P <- 836 / (836 + 30 + 271 + 0 + 0 + 0 + 0 + 0 + 0)
P
R <- 836 / (836 + 3268 + 4409 + 0)
R
F1 <- 2 * P * R / (P + R)
F1

#####             #####
##### Zoya Hafeez #####
#####             #####

#attach(PRSA_Beijing_All)
library(readr)
PRSA_Beijing_All <- read_csv("C:/Users/User/Desktop/DSC441/Data_Sets/PRSA_Beijing_All.csv")
View(PRSA_Beijing_All)
library(tidyverse)
all_no_na = PRSA_Beijing_All %>% filter((!(is.na(PM2.5))) & (!(is.na(SO2)))& (!(is.na(NO2)))& (!(is.na(CO)))& (!(is.na(O3))))
all_no_na2 = PRSA_Beijing_All %>% filter((!(is.na(RAIN))) &(!(is.na(WSPM))) &(!(is.na(day))) &(!(is.na(year))) &(!(is.na(month))) &(!(is.na(PRES))) &(!(is.na(TEMP))) &(!(is.na(DEWP))) &(!(is.na(PM2.5))) & (!(is.na(SO2)))& (!(is.na(NO2)))& (!(is.na(CO)))& (!(is.na(O3))))
all_no_na_all = PRSA_Beijing_All %>% filter((!(is.na(PM10))) &(!(is.na(RAIN))) &(!(is.na(WSPM))) &(!(is.na(day))) &(!(is.na(year))) &(!(is.na(month))) &(!(is.na(PRES))) &(!(is.na(TEMP))) &(!(is.na(DEWP))) &(!(is.na(PM2.5))) & (!(is.na(SO2)))& (!(is.na(NO2)))& (!(is.na(CO)))& (!(is.na(O3))))

attach(all_no_na)
summary(all_no_na)

#Multiple Linear Regression Model 

########Step 1######## 
#First Order Term 

#Model2 [PM2.5~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM]
model2 <- lm (PM2.5~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM)
summary(model2)

#Model1 [PM10 VS SO2,NO2,CO,Temprature, Pressure, Day, Month, Year, DewPoint, Rain & WSPM]
model <- lm(PM10~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM)
summary(model)



#######Step 2#########
#Second Order Term
#Model2 [PM2.5 VS SO2,NO2,CO,Temprature, Pressure, Day, Month, Year, DewPoint, Rain & WSPM]
modelfinal <- lm (PM2.5~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM+I(SO2^2)+I(NO2^2)+I(NO2^2)+I(CO^2)+I(TEMP^2)+I(PRES^2)+I(day^2)+I(month^2)+I(year^2)+(DEWP^2)+I(RAIN^2)+I(WSPM^2))
summary(modelfinal)

#Model3  [PM10 VS SO2,NO2,CO,Temprature, Pressure, Day, Month, Year, DewPoint, Rain & WSPM
modelfinal2.10 <- lm (PM10~SO2+NO2+CO+TEMP+PRES+day+month+year+DEWP+RAIN+WSPM+I(SO2^2)+I(NO2^2)+I(NO2^2)+I(CO^2)+I(TEMP^2)+I(PRES^2)+I(day^2)+I(month^2)+I(year^2)+(DEWP^2)+I(RAIN^2)+I(WSPM^2))
summary(modelfinal2.10)




#################Step4###############
# Feature Selection (Backward Selection)
library(MASS)

#For PM2.5

step <- stepAIC(modelfinal, direction="backward")


#For PM10
step <- stepAIC(modelfinal2.10, direction="backward")



#################Step 5###############

#Final Model

#For PM2.5
finalmodelPM2.5 <- lm(PM2.5 ~ SO2 + NO2 + CO + TEMP + PRES + day + month + year + DEWP + 
                        RAIN + WSPM + I(SO2^2) + I(NO2^2) + I(NO2^2) + I(CO^2) + 
                        I(TEMP^2) + I(PRES^2) + I(day^2) + I(month^2) + I(year^2) + 
                        (DEWP^2) + I(RAIN^2) + I(WSPM^2))
summary(finalmodelPM2.5)

#FOR PM10
#FinalmodelPM10 <- lm(PM10 ~ SO2 + NO2 + CO + TEMP + PRES + month + year + DEWP + RAIN + 
I(SO2^2) + I(NO2^2) + I(CO^2) + I(TEMP^2) + I(PRES^2) + I(day^2) + 
  I(month^2) + I(year^2) + I(RAIN^2) + I(WSPM^2))
#FinalmodelPM10 <- lm(PM10 ~ SO2 + NO2 + CO + TEMP + PRES + month + year + DEWP + RAIN + 
I(SO2^2) + I(NO2^2) + I(CO^2) + I(TEMP^2) + I(PRES^2) + I(day^2) + 
  I(month^2) + I(year^2) + I(RAIN^2) + I(WSPM^2), data=all_no_na_all)

FinalmodelPM10 <- lm (PM10 ~ SO2 + NO2 + CO + TEMP + PRES + month + year + DEWP + RAIN + 
                        I(SO2^2) + I(NO2^2) + I(CO^2) + I(TEMP^2) + I(PRES^2) +
                        I(month^2) + I(year^2) + I(RAIN^2) + , data=all_no_na_all)
summary(FinalmodelPM10)

######################FWD SELECTION###################

#Forward Selection
library(MASS)
#PM2.5
m6 <- lm(PM2.5 ~ SO2 + NO2 + CO + TEMP + PRES + day + month + year + DEWP + 
           RAIN + WSPM + I(SO2^2) + I(NO2^2) + I(NO2^2) + I(CO^2) + 
           I(TEMP^2) + I(PRES^2) + I(day^2) + I(month^2) + I(year^2) + 
           (DEWP^2) + I(RAIN^2) + I(WSPM^2), data=all_no_na2)
m7 <- lm(PM2.5~1, data=all_no_na2)
summary(m7)
step <- stepAIC(m7,direction="forward", scope=list(upper=m6,lower=m7))

#PM10
m8 <- lm(PM10 ~ SO2 + NO2 + CO + TEMP + PRES + day + month + year + DEWP + 
           RAIN + WSPM + I(SO2^2) + I(NO2^2) + I(NO2^2) + I(CO^2) + 
           I(TEMP^2) + I(PRES^2) + I(day^2) + I(month^2) + I(year^2) + 
           (DEWP^2) + I(RAIN^2) + I(WSPM^2), data=all_no_na2)
m9 <- lm(PM10~1, data=all_no_na2)
summary(m9)
step <- stepAIC(m9,direction="forward", scope=list(upper=m8,lower=m9))
#################Step6################
#Multicollinearity

library(car)
Vif(model)

#For PM2.5#
finalmodelPM2.5 <- lm(PM2.5 ~ SO2 + NO2 + CO + TEMP + PRES + day + month + year + DEWP + 
                        RAIN + WSPM + I(SO2^2) + I(NO2^2) + I(NO2^2) + I(CO^2) + 
                        I(TEMP^2) + I(PRES^2) + I(day^2) + I(month^2) + I(year^2) + 
                        (DEWP^2) + I(RAIN^2) + I(WSPM^2))
vif(finalmodelPM2.5)



#FOR PM10
FinalmodelPM10 <- lm(PM10 ~ SO2 + NO2 + CO + TEMP + PRES + month + year + DEWP + RAIN + 
                       I(SO2^2) + I(NO2^2) + I(CO^2) + I(TEMP^2) + I(PRES^2) + I(day^2) + 
                       I(month^2) + I(year^2) + I(RAIN^2) + I(WSPM^2))


vif(FinalmodelPM10)



###################Step 7#####################


#Residual Analysis on Final Model
#QQplot
set.seed(42)
x <- rnorm(100)
x
qqnorm(x)
qqline(x)

#For PM2.5
finalmodelPM2.5 <- lm(PM2.5 ~ SO2 + NO2 + CO + TEMP + PRES + day + month + year + DEWP + 
                        RAIN + WSPM + I(SO2^2) + I(NO2^2) + I(NO2^2) + I(CO^2) + 
                        I(TEMP^2) + I(PRES^2) + I(day^2) + I(month^2) + I(year^2) + 
                        (DEWP^2) + I(RAIN^2) + I(WSPM^2))
#hist(finalmodelPM2.5$residuals)
hist(finalmodelPM2.5$residuals, col="blue", nclass=100)


#QQplot
qqplot(finalmodelPM2.5$residuals, col="blue")
#qqnorm(finalmodelPM2.5)
qqline(finalmodelPM2.5$residuals)



# Rsidual with Explanatory variables:
#PM2.5 VS CO
plot(all_no_na2$CO, finalmodelPM2.5$residuals, ylab="Residuals", xlab="CO", main="Residuals (PM2.5)", col="purple") 
abline(0, 0)                  # the horizon 

#PM2.5 VS I(NO2^2)
#plot(all_no_na2$NO2^2, finalmodelPM2.5$residuals, ylab="Residuals", xlab="I(NO2^2)", main="Residuals") 
# abline(0, 0)                  # the horizon 

plot(all_no_na2$NO2^2, finalmodelPM2.5$residuals, ylab="Residuals", xlab="I(NO2^2)",  main="Residuals (PM2.5)", col="purple") 
abline(0, 0)                  # the horizon 


#PM10 VS CO
plot(all_no_na_all$CO, FinalmodelPM10$residuals, ylab="Residuals (PM10)", xlab="CO",  main="Residuals (PM10)", col="blue") 
abline(0, 0)                  # the horizon 

#PM10 VS I(NO2^2)
plot(all_no_na_all$NO2^2, FinalmodelPM10$residuals, ylab="Residuals (PM10)", xlab="I(NO2^2)",main="Residuals (PM10)", col="blue") 
abline(0, 0)                  # the horizon 


cor(PM2.5,PM10)

#FOR PM10
FinalmodelPM10 <- lm(PM10 ~ SO2 + NO2 + CO + TEMP + PRES + month + year + DEWP + RAIN + 
                       I(SO2^2) + I(NO2^2) + I(CO^2) + I(TEMP^2) + I(PRES^2) + I(day^2) + 
                       I(month^2) + I(year^2) + I(RAIN^2) + I(WSPM^2))
hist(finalmodelPM10)

#QQplot
qqnorm(modelhygiene$residuals)
qqline(modelhygiene$residuals)