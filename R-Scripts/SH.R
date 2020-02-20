library(readr)
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(magrittr)

pr <- read_csv("F:/Graduate/Depaul/Classes/DSC 441/Project/PRSA_Beijing_All.csv", na = "empty")
head(pr)
str(pr)
pr$date <- as.Date(pr$date, "%m/%d/%Y")
str(pr)

plot(pr$date, pr$TEMP)
head(pr)

pr <- as.data.frame(pr)

pr2 <- pr
pr2$date <- as.numeric(pr2$date)
head(pr2)

pr1 <- as.data.frame(pr2)
pr1 <- pr2 %>% select_if(is.numeric)

#Principal Component Analysis
p1 <- principal(pr1, nfactors = 4, covar = TRUE)
print(p1)

#Regression
m1 <- lm(pr1$PM2.5 ~ ., data = pr1)
summary(m1)

m2 <- lm(pr$PM2.5 ~ pr$year + pr$month + pr$day + pr$hour
         + pr$date, data = pr)
summary(m2)

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
