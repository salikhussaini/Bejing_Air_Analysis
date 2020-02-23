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

