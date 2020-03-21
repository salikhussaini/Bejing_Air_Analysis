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

