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
library(geosphere)
library(alr3)

mae <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- sum(abs(labels - preds))/length(labels)
  return(list(metric = "mae", value=err))
}

MAEobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  grad <- exp(preds)-exp(labels)
  hess <- exp(preds)
  return(list(grad = grad, hess = hess))
}


# Calculates n-way interactions for categorical variables
# If train is true we use folds to get out of sample results
getCategoryNWayInteraction <- function(train, y_name, category_vector, test=NULL,
                                       trainOrApply = 'T', folds=5, eliminateOrigColumns=FALSE,
                                       simpleName=FALSE) {
  no_rows <- nrow(train)
  #y_train <- as.data.frame(train[,y_name,with=FALSE])
    
  if (trainOrApply=='A') folds=1
  sample_folds <- sample(1:folds,no_rows,replace=TRUE)
  
  # Set names of columns
  if (simpleName==TRUE) {
    colMeanResponseName <- "NWay_Mean_Response"
    colMedianResponseName <- "NWay_Median_Response"
    colCountResponseName <- "NWay_Count_Response"
    colMaxResponseName <- "NWay_Max_Response"
    colMinResponseName <- "NWay_Min_Response"
    colSDResponseName <- "NWay_SD_Response"
    
  } else {  
    colMeanResponseName <- paste0(paste0(category_vector,collapse="_"),"_Mean_Response")
    colMedianResponseName <- paste0(paste0(category_vector,collapse="_"),"_Median_Response")
    colCountResponseName <- paste0(paste0(category_vector,collapse="_"),"_Count_Response")
    colMaxResponseName <- paste0(paste0(category_vector,collapse="_"),"_Max_Response")
    colMinResponseName <- paste0(paste0(category_vector,collapse="_"),"_Min_Response")
    colSDResponseName <- paste0(paste0(category_vector,collapse="_"),"_SD_Response")
  }
  
  for (f in 1:folds) {
    if (trainOrApply=='T')
      idx_train <- which(!sample_folds==f) else 
        idx_train <- which(sample_folds==f)        
      
      idx_out_of_sample <- which(sample_folds==f) 
      n_Way_Results <- train[idx_train,j=list(Mean.Response=mean(get(y_name)),Median.Response=median(get(y_name)),
                                              Max.Response=max(get(y_name)),Min.Response=min(get(y_name)),SD.Response=sd(get(y_name)),
                                              Count=length(get(y_name))),by=category_vector]
      setkeyv(n_Way_Results,category_vector)
      mean_y <- mean(train[[y_name]])
      
      if (trainOrApply=='T')  {
        train[idx_out_of_sample,colMeanResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Mean.Response)]
        train[idx_out_of_sample,colMedianResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Median.Response)]
        train[idx_out_of_sample,colCountResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Count)]
        #train[idx_out_of_sample,colMaxResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Max.Response)]
        #train[idx_out_of_sample,colMinResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Min.Response)]
        train[idx_out_of_sample,colSDResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(SD.Response)]
      } else {
        test[,colMeanResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Mean.Response)]
        test[,colMedianResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Median.Response)]
        test[,colCountResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Count)]
        #test[,colMaxResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Max.Response)]
        #test[,colMinResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Min.Response)]
        test[,colSDResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(SD.Response)]
        
      }
  } # end of For Loop with Folds
  
  
  returnCols <- c(colMeanResponseName,colCountResponseName, colSDResponseName)
  # returnCols <- c(colMeanResponseName, colCountResponseName)
  if (trainOrApply=='T') {
    #return <- train[,returnCols,with=FALSE]
    return <- train
    # This is apply
  } else {
    #return <- test[,returnCols,with=FALSE]
    return <- test
  }
  if (eliminateOrigColumns==FALSE) category_vector <- NULL
  returnCols <- setdiff(colnames(return),c(category_vector))
  return[[colMeanResponseName]] <- ifelse(is.na(return[[colMeanResponseName]]),mean_y, return[[colMeanResponseName]])
  return <- return[,returnCols,with=FALSE]
  return
}


cols <- c("parcelid","calculatedfinishedsquarefeet","latitude","longitude",
          "landtaxvaluedollarcnt","structuretaxvaluedollarcnt","taxvaluedollarcnt","lotsizesquarefeet")

#10711805
getClosestDistLogError <- function(train,parcelid,distance) {
  j <- min(which(train$parcelid==parcelid))
  trainSameZips <- which(train$regionidzip==train$regionidzip[j])
  trainSameZips <- setdiff(trainSameZips,j)
  logerrorArray <- as.vector(NULL)
  for (prop in trainSameZips) {
    if (distHaversine(c(train$longitude[j]/1e6,train$latitude[j]/1e6),
                      c(train$longitude[prop]/1e6,train$latitude[prop]/1e6)) < distance)
      #print(train[prop,cols,with=FALSE])
      logerrorArray <- c(logerrorArray,train$logerror[prop])
  }
  if (length(logerrorArray)==0) {
    list(logerrorMean = NA, logerrorCount = length(logerrorArray), logerrorMedian=NA)
  } else {
    list(logerrorMean = mean(logerrorArray), logerrorCount = length(logerrorArray), logerrorMedian=median(logerrorArray))
  }
}

#
#getClosestDistLogErrorLM <- function(train,parcelid,distance) {

idx <- 1:1000
idx_length <- length(idx)
df_rsquared <- data.frame(squarefeet_rsq=numeric(idx_length),yearbuilt_rsq=numeric(idx_length), count_lm=numeric(idx_length),
                          squarefeet_pred = numeric(idx_length), yearbuilt_pred = numeric(idx_length))

for (i in idx) {
  parcelid <- train$parcelid[i]
  distance <- 1000
  j <- min(which(train$parcelid==parcelid))
  trainSameZips <- which(train$regionidzip==train$regionidzip[j])
  trainSameZips <- setdiff(trainSameZips,j)
  parcel_df <- as.data.frame(NULL)
  for (prop in trainSameZips) {
    if (distHaversine(c(train$longitude[j]/1e6,train$latitude[j]/1e6),
                      c(train$longitude[prop]/1e6,train$latitude[prop]/1e6)) < distance)
      parcel_df <- rbind(parcel_df,
                         data.frame(parcelid=train$parcelid[prop],calculatedfinishedsquarefeet=train$calculatedfinishedsquarefeet[prop],
                                    yearbuilt=train$yearbuilt[prop],logerror=train$logerror[prop]))
  }
  print(paste0("no of rows: ",nrow(parcel_df)))
  idx <- which(parcel_df$logerror <= (mean(parcel_df$logerror)+2*(sd(parcel_df$logerror))))
  idx2 <- which(parcel_df$logerror >= (mean(parcel_df$logerror)-2*(sd(parcel_df$logerror))))
  parcel_df <- parcel_df[intersect(idx,idx2),]
  print(paste0("no of rows: ",nrow(parcel_df)))
  # plot(parcel_df$finishedsquarefeet,parcel_df$logerror)
  # points(train$calculatedfinishedsquarefeet[j], train$logerror[j], col=4,pch=16)
  # plot(parcel_df$yearbuilt,parcel_df$logerror)
  # points(train$yearbuilt[j], train$logerror[j], col=4,pch=16)
  
  if(nrow(parcel_df)>0)  {
    lm.fit <- lm(logerror~calculatedfinishedsquarefeet,data=parcel_df)
    df_rsquared$squarefeet_pred[i] <- predict(lm.fit,newdata=train[i])
    parcel_df$yearbuilt <- ifelse(is.na(parcel_df$yearbuilt),0,parcel_df$yearbuilt)
    lm.fit <- lm(logerror~yearbuilt,data=parcel_df)
    df_rsquared$yearbuilt_pred[i] <- predict(lm.fit,newdata=train[i])
    # summary(lm(logerror~yearbuilt,data=parcel_df))
    df_rsquared$squarefeet_rsq[i] <- cor(parcel_df$logerror ,parcel_df$calculatedfinishedsquarefeet)^2
    df_rsquared$yearbuilt_rsq[i] <- cor(parcel_df$logerror ,parcel_df$yearbuilt)^2
    df_rsquared$count[i] <- nrow(parcel_df)
  }
  
}

mean(df_rsquared$squarefeet,na.rm=TRUE)
mean(df_rsquared$yearbuilt,na.rm=TRUE)

idx <- 1:1000
train$squarefeet_rsq <- 0
train$squarefeet_rsq[idx] <- df_rsquared$squarefeet_rsq
train$yearbuilt_rsq <- 0
train$yearbuilt_rsq[idx] <- df_rsquared$yearbuilt_rsq
train$count <- 0
train$count[idx] <- df_rsquared$count 
train$squarefeet_pred <- 0
train$squarefeet_pred[idx] <- df_rsquared$squarefeet_pred
train$yearbuilt_pred <- 0
train$yearbuilt_pred[idx] <- df_rsquared$yearbuilt_pred


#   if (length(logerrorArray)==0) {
#     list(logerrorMean = NA, logerrorCount = length(logerrorArray), logerrorMedian=NA)
#   } else {
#     list(logerrorMean = mean(logerrorArray), logerrorCount = length(logerrorArray), logerrorMedian=median(logerrorArray))
#   }
# }

#  return a list with median, mean, min,max,sd,count
# Californian on Wilshire
#train[train$logerror>3.0]
# parcelid = 11617445
# # 34059300, -118440000
# # Californian on Wilshire
# californian <- which(properties$latitude==34059300 & properties$longitude==-118440000) 
# properties[californian, cols, with=FALSE]
# 
# #cor(properties$landtaxvaluedollarcnt[californian], properties$structuretaxvaluedollarcnt[californian])
# #cor(properties$finishedsquarefeet12[californian], properties$landtaxvaluedollarcnt[californian])
# 

# Geo check
# threshold <- 0.2
# #train_sub <- train[which(abs(train$logerror)>threshold & !is.na(train$latitude))]
# train_sub <- train[which(train$logerror>threshold & !is.na(train$latitude))]
# print(paste0("Number of Rows : ",nrow(train_sub)))
# 
# # convert data to a SpatialPointsDataFrame object
# xy <- SpatialPointsDataFrame(
#   matrix(c(train_sub$longitude/1e6,train_sub$latitude/1e6), ncol=2), data.frame(ID=train_sub$parcelid),
#   proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# 
# # use the distm function to generate a geodesic distance matrix in meters
# mdist <- distm(xy)
# 
# # cluster all points using a hierarchical clustering approach
# hc <- hclust(as.dist(mdist), method="complete")
# 
# # define the distance threshold, in this case 40 m
# d=400
# 
# # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
# xy$clust <- cutree(hc, h=d)
# length(unique(xy$clust))
# xy
# which(table(xy$clust)>3)
# train[train$parcelid %in% xy$ID[xy$clust==2504]]
# 
