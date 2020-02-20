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
