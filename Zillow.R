library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(lubridate)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)

properties <- fread('properties_2016.csv', showProgress = FALSE)
train <- fread('train_2016.csv', showProgress = FALSE)
sample_submission <- fread('sample_submission.csv')

train$transactiondate <- as.Date(train$transactiondate)
# join both tables
train <- merge(train,properties)

#
tbl_by_zip <- train %>% group_by(regionidzip=regionidzip) %>% 
  summarize(mean_logerror=mean(logerror), count_logerror=length(logerror)) %>% as.data.table()
tbl_by_zip <- tbl_by_zip[order(tbl_by_zip$mean_logerror)]

# Couple of charts
prop_miss_pct <- map_dbl(properties, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
prop_miss_pct <- prop_miss_pct[prop_miss_pct > 0]

data.frame(miss=prop_miss_pct, var=names(prop_miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# logerror by transaction date
train %>% group_by(transaction_month=month(transactiondate)) %>% 
  summarize(mean_logerror=mean(logerror)) %>%
  ggplot(aes(x=transaction_month, y=mean_logerror)) +
  geom_point(color='red') + 
  #geom_smooth(color='darkgrey') +
  ggtitle('Mean price by year of build')

