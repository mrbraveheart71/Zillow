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
library(xgboost)
library(caret)

source("Zillow_Functions.R")

properties <- fread('properties_2016.csv', showProgress = TRUE)
train <- fread('train_2016.csv', showProgress = TRUE)
sample_submission <- fread('sample_submission.csv')

train$transactiondate <- as.Date(train$transactiondate)
train$transactionmonth <- month(train$transactiondate)

properties[,location_count:=length(parcelid),by=list(latitude,longitude)] 
properties[,location_count:=ifelse(is.na(latitude) | is.na(longitude),1,location_count)]
properties[,location_id:=max(parcelid),by=list(latitude,longitude)]

# join both tables
train <- merge(train,properties)

# prop_zoning_mean <- train[,j=list(prob_zoning_mean=mean(logerror),prop_zoning_count=length(logerror),
#                                   prop_zoning_sd = sd(logerror)),
#                                     by=list(propertyzoningdesc)]
# prop_zoning_mean[prop_zoning_mean$prop_zoning_count>20][order(prob_zoning_mean)]
# #train[,prop_zoning_mean=mean(logerror),by=list(propertyzoningdesc)]
# 
# prop_year_mean <- train[,j=list(prob_year_mean=mean(logerror),prop_year_count=length(logerror),
#                                   prop_year_sd = sd(logerror)),
#                           by=yearbuilt]
# prop_year_mean[order(prop_year_mean$yearbuilt)][100:131]

# from here create a model
characterVars <- names(which(sapply(train, class)=='character'))
noCharacter <- length(names(which(sapply(train, class)=='character')))
integerVars <- names(which(sapply(train, class)=='integer'))
noInteger <- length(names(which(sapply(train, class)=='integer')))
numericVars <- names(which(sapply(train, class)=='numeric'))
noNumeric <- length(numericVars)
noAllCol <- noCharacter + noInteger + noNumeric

# convert all character cols to numeric (integer encoding)
cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in characterVars) {
  #colValues <- c(train[[f]],test[[f]])
  colValues <- c(train[[f]])
  uniquecolValues <- table(colValues)
  levels <- names(uniquecolValues[order(uniquecolValues)])
  train[[f]] <- as.numeric(factor(train[[f]], levels=levels))
  #test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
}

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in integerVars) {
  train[[f]] <- as.numeric(train[[f]])
}

n_folds <- 50
train <- getCategoryNWayInteraction(train,"logerror","location_id", eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror","regionidzip", eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror","propertyzoningdesc", eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror","regionidneighborhood",eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror","transactionmonth",eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror","yearbuilt",eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror",c("yearbuilt","regionidzip"),eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror",c("yearbuilt","regionidneighborhood"),eliminateOrigColumns = FALSE, folds=n_folds)
train <- getCategoryNWayInteraction(train,"logerror",c("yearbuilt","propertyzoningdesc"),eliminateOrigColumns = FALSE, folds=n_folds)

# Create the folds
zillow.cv.folds <- createFolds(1:nrow(train),5)

save("zillow.cv.folds","train",file="Zillow Train Save")

# Now load file
load("Zillow Train Save")
#train <- merge(train,taxamount_pred)
idx <- sample(1:nrow(train),1000)
idx <- 1:100
closestDistArray <- sapply(train$parcelid[idx],function(x) as.numeric(getClosestDistLogError(train,x,500)))
closestDistArray <- as.data.frame(t(closestDistArray))
colnames(closestDistArray) <- c("logerrorMean","logerrorCount","logerrorMedian")

train$logerrorMean <- 0
train$logerrorCount <- 0
train$logerrorMedian <- 0
train$logerrorMean[idx] <- closestDistArray$logerrorMean
train$logerrorMedian[idx] <- closestDistArray$logerrorMedian
train$logerrorCount[idx] <- closestDistArray$logerrorCount
idx1 <- idx[!is.na(train$logerrorMean[idx])]
cor(train$logerrorCount[idx1], train$logerror[idx1])

# xgb_params = list(eta= 0.02,max_depth= 5,subsample= 0.8,colsample=0.9,
#                   min_child_weight = 200, base_score = 0,objective="reg:linear")
# 
# feature.names <- setdiff(colnames(train),c("parcelid","logerror","transactiondate","taxamount","taxvaluedollarcnt",
#                                            "structuretaxvaluedollarcnt","landtaxvaluedollarcnt"))
# feature.names <- setdiff(feature.names,c("preds.oob.1","preds.oob.2","preds.oob.3"))
# xall <- xgb.DMatrix(data.matrix(train[,feature.names,with=FALSE]), label=train$logerror, missing=NA)
# xgboost.cv.fit <- xgb.cv (data=xall,params=xgb_params,nround=50, metrics=list('mae'),
#                           early_stopping_rounds = 20, print_every_n =5,folds=zillow.cv.folds, prediction=TRUE)
# train$preds.oob.1 <- xgboost.cv.fit$pred

#idxYear <- which(train$yearbuilt<1920)
#idx <- 1:nrow(train)
print(length(idx))
xgb_params = list(eta= 0.01,max_depth= 4,subsample= 0.8,colsample=0.9,
                  min_child_weight = 80, base_score = 0,objective="reg:linear")
feature.names <- setdiff(colnames(train),c("parcelid","logerror","transactiondate"))
feature.names <- setdiff(feature.names,c("preds.oob.1","preds.oob.2","preds.oob.3"))
#feature.names <- c("logerrorMean","logerrorMedian","logerrorCount")
xall <- xgb.DMatrix(data.matrix(train[idx,feature.names,with=FALSE]), label=train$logerror[idx], missing=NA)
xgboost.cv.fit <- xgb.cv (data=xall,params=xgb_params,nround=500, metrics=list('mae'),
                          early_stopping_rounds = 20, print_every_n =5,
                          nfold=5,
                          #folds=zillow.cv.folds, 
                          prediction=TRUE)

ModelMetrics::mae(train$logerror[idx],rep(0,length(idx)))
ModelMetrics::mae(train$logerror[idx],xgboost.cv.fit$pred)
#ModelMetrics::mae(train$logerror[idxYear],xgboost.cv.fit$pred[idxYear])


train$preds.oob.2 <- xgboost.cv.fit$pred
cor(train$preds.oob.1,train$preds.oob.2)

feature.names <- sample(colnames(train),80)
feature.names <- setdiff(feature.names,c("preds.oob.1","preds.oob.2","preds.oob.3"))
feature.names <- setdiff(feature.names,c("parcelid","logerror","transactiondate","preds.oob.1","preds.oob.2","preds.oob.3"))
xall <- xgb.DMatrix(data.matrix(train[,feature.names,with=FALSE]), label=train$logerror, missing=NA)
xgboost.cv.fit <- xgb.cv (data=xall,params=xgb_params,nround=50, metrics=list('mae'),
                          early_stopping_rounds = 20, print_every_n =5,folds=zillow.cv.folds, prediction=TRUE)
train$preds.oob.3 <- xgboost.cv.fit$pred
cor(train$preds.oob.1,train$preds.oob.3)

# 2nd Layer Model
feature.names <- setdiff(colnames(train),c("parcelid","logerror","transactiondate"))
feature.names <- c("preds.oob.1","preds.oob.2","preds.oob.3")
xall <- xgb.DMatrix(data.matrix(train[,feature.names,with=FALSE]), label=train$logerror, missing=NA)
xgb_params = list(eta= 0.01,max_depth=3,subsample= 0.9,colsample=0.9,
                  min_child_weight = 20, objective="reg:linear", base_score=0)
xgboost.cv.fit <- xgb.cv (data=xall,params=xgb_params,nround=500, metrics=list('mae'),
                          early_stopping_rounds = 20, print_every_n =5,folds=zillow.cv.folds, prediction=TRUE)

ModelMetrics::mae(train$logerror,rep(0,nrow(train)))
ModelMetrics::mae(train$logerror,train$preds.oob.1)
ModelMetrics::mae(train$logerror[train$transactionmonth >9],train$preds.oob.1[train$transactionmonth>9]*1.00)

ModelMetrics::mae(train$logerror,train$preds.oob.3)
ModelMetrics::mae(train$logerror,xgboost.cv.fit$pred)
ModelMetrics::mae(train$logerror,train$preds.oob.1*0.5+train$preds.oob.2*0.5)
cor(train$preds.oob.1,train$preds.oob.2)


# GAM
t <- sample(1:nrow(train),0.9*nrow(train))
s <- setdiff(1:nrow(train),t)
gam.fit <- gam(logerror~ns(finishedsquarefeet12,df=5), data=train)
ModelMetrics::mae(train$logerror,gam.fit$fitted.values)
ModelMetrics::mae(train$logerror,rep(0,nrow(train)))

# KKNN
load("Zillow Train Save")
library(kknn)
train <- train[!is.na(train$latitude) & !is.na(longitude) ]
t <- sample(1:nrow(train),0.8*nrow(train))
s <- setdiff(1:nrow(train),t)
kknn.fit <- kknn(logerror~longitude+latitude,train=train[t],test=train[s])

train_idx <- 1:10000
train_idx <- sample(1:nrow(train),0.8*nrow(train))
test_idx <- c(10001:20000,30000:90000)
test_idx <- setdiff(1:nrow(train),train_idx)
kknn.fit <- kknn(logerror~longitude+latitude,train=train[train_idx],test=train[test_idx],k=10)
ModelMetrics::mae(train$logerror[test_idx],kknn.fit$fitted.values)

idx <- which(train$yearbuilt>1910)
ModelMetrics::mae(train$logerror[idx],rep(0,length(idx)))

# end of KKNN

# ModelMetrics::mae(train$logerror[s],kknn.fit$fitted.values[s])
# ModelMetrics::mae(train$logerror,rep(0,nrow(train)))

train <- train[idx]
t <- sample(1:nrow(train),0.8*nrow(train))
s <- setdiff(1:nrow(train),t)
xgb_params = list(eta= 0.01,max_depth=5,subsample= 0.9,colsample=0.9,
                  min_child_weight = 100, objective="reg:linear", base_score=0)
xtrain <- xgb.DMatrix(data.matrix(train[t,feature.names,with=FALSE]), label=train$logerror[t], missing=NA)
xvalid <- xgb.DMatrix(data.matrix(train[s,feature.names,with=FALSE]), label=train$logerror[s], missing=NA)
watchlist=list(xtrain=xtrain, xvalid=xvalid)
xgboost.fit <- xgb.train (data=xtrain,params=xgb_params,nround=200,print_every_n =10,
                          watchlist=watchlist,early_stopping_rounds = 20,
                          #objective = MAEobj,
                          feval=mae,
                          maximize=FALSE)

importance_matrix <- xgb.importance(model = xgboost.fit, feature.names)
head(importance_matrix,50)

# # check how good the predictions are
# train$pred <- xgboost.cv.fit$pred
# train$pred_diff <- train$pred-train$logerror
# hist(train$pred_diff,nclass=200)
# train[train$pred > 0.1]
# Metrics::mae(train$pred,train$logerror)
# Metrics::mae(0,train$logerror)


## Some visualization
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
train %>% group_by(transaction_month=make_date(year=year(transactiondate),month=month(transactiondate))) %>% 
  summarize(mean_logerror=mean(logerror)) %>%
  ggplot(aes(x=transaction_month, y=mean_logerror)) +
  geom_point(color='red') + 
  #geom_smooth(color='darkgrey') +
  ggtitle('Mean log-error by month')
