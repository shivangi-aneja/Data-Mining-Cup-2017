set.seed(42)
rm(list=ls())

#DMC-Attempt
getwd()
setwd("~/Desktop/M.S./Business Analytics/DMC/");
train_data <- read.csv("DMC_training_data.csv")
test_data <- read.csv("DMC_test_data.csv")
str(train_data)
#Size
nrow(train_data)
ncol(train_data)

ncol(test_data)
nrow(test_data)

#Peek at first and last rows
head(train_data)
tail(train_data)

head(test_data)
tail(test_data)
#Look at missing value columns
colSums(is.na(train_data))
colSums(is.na(test_data))
#Convert the format of data
timestamp_format <- "%Y-%m-%d %H:%M:%S"
date_format <- "%Y-%m-%d"
week_format <- "%W"
time_format = "%H"
bin_points <- c(-Inf,3,6,9,12,15,18,21,Inf)
train_data$TimeStamp <- strftime(train_data$TimeStamp,timestamp_format)
#Rounding off properly
install.packages("lubridate")
library("lubridate")
extracted_time <- ymd_hms(train_data$TimeStamp)
rounded_time <- round_date(extracted_time,unit = "hour")
Train_Time_of_Day <- hour(rounded_time)
table(Train_Time_of_Day,useNA = "always")
train_data$time_of_Day <-cut(Train_Time_of_Day,bin_points,labels=1:8)
train_data$week <- strftime(train_data$TimeStamp,week_format)
#binning caused some issues but resolved it using:-
#https://stackoverflow.com/questions/11963508/generate-bins-from-a-data-frame

#Try the K-means clustering
data_point_train <- cbind(train_data$ADDR_LATITUDE,train_data$ADDR_LONGITUDE)

#Deciding the number of clusters
#wss <- (nrow(data_point)-1)*sum(apply(data_point,2,var))
#for (i in 2:20) wss[i] <- sum(kmeans(data_point,
#                                     centers=i)$withinss)
#plot(1:20, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")

#Chosing the number 5
install.packages("flexclust")
library("flexclust")
cl1 = kcca(data_point_train,k=5,kccaFamily("kmeans"))
#This one is a bit ugly but I could not find a better way for including the data
train_data$region <- predict(cl1,data_point_train)

#table(train_data$region)
#table(train_data$Train_Time_of_Day)

train_data$LAST_MODIFIED <- as.Date(train_data$LAST_MODIFIED,date_format)
train_data$VALIDATION_LAST_MODIFIED <- as.Date(train_data$VALIDATION_LAST_MODIFIED,date_format)
test_data$TimeStamp <- strftime(test_data$TimeStamp,timestamp_format)

extracted_time <- ymd_hms(test_data$TimeStamp)
rounded_time <- round_date(extracted_time,unit = "hour")
Test_Time_of_Day <- hour(rounded_time)
table(Test_Time_of_Day,useNA = "always")

test_data$time_of_Day <- cut(Test_Time_of_Day,bin_points,labels=1:8)
#colSums(is.na(test_data))
test_data$week <- strftime(test_data$TimeStamp,week_format)
data_point_test <- cbind(test_data$ADDR_LATITUDE,test_data$ADDR_LONGITUDE)
#table(test_data$Test_Time_of_day)
test_data$region <- predict(cl1,newdata = data_point_test)
#table(test_data$region)
test_data$LAST_MODIFIED <- as.Date(test_data$LAST_MODIFIED,date_format)
test_data$VALIDATION_LAST_MODIFIED <- as.Date(test_data$VALIDATION_LAST_MODIFIED,date_format)


#table(Train_Time_of_Day)
#Since the houehold count field is NA all through, we drop it
train_data = subset(train_data,select = -(HOUSEHOLD_COUNT))
test_data = subset(test_data,select = -(HOUSEHOLD_COUNT))
#Let us touch the test_data later

#See the structure again
str(train_data)

#Explore Address_Street
table(train_data$ADDR_STREET)

#Look at preferred partner
table(train_data$PREFERRED_PARTNER)
table(test_data$PREFERRED_PARTNER)
#train_data$PREFERRED_PARTNER <- NULL #All values are Yes only, no use
train_data = subset(train_data,select = -(PREFERRED_PARTNER))
test_data = subset(test_data,select = -(PREFERRED_PARTNER))
#Unify the ADDR_REGION column
table(train_data$ADDR_REGION)
train_data$ADDR_REGION[train_data$ADDR_REGION == "California"] = "CA"
train_data$ADDR_REGION <- droplevels(train_data$ADDR_REGION)
#table(train_data$ADDR_MUNICIPALITY)
train_data$ADDR_MUNICIPALITY[train_data$ADDR_MUNICIPALITY == "San Francisco"] = "SF"
train_data$ADDR_MUNICIPALITY <- droplevels(train_data$ADDR_MUNICIPALITY)
#table(train_data$ADDR_MUNICIPALITY)
#table(train_data$FREECHARGE,useNA = "ifany")
levels(train_data$FREECHARGE) = c(levels(train_data$FREECHARGE),"NO")
train_data$FREECHARGE[train_data$FREECHARGE == ""] = "NO"
train_data$FREECHARGE <- droplevels(train_data$FREECHARGE)

#Same for Test_Data
levels(test_data$FREECHARGE) = c(levels(test_data$FREECHARGE),"NO")
test_data$FREECHARGE[test_data$FREECHARGE == ""] = "NO"
test_data$FREECHARGE <- droplevels(test_data$FREECHARGE)

#table(train_data$FREECHARGE,useNA = "ifany")

table(train_data$IS_LSC_VALIDATED)
range(train_data$TimeStamp)
#We can use the idea of separating the weekdays from the weekends during our processing

train_data$weekday = weekdays(as.Date(train_data$TimeStamp,date_format))
test_data$weekday = weekdays(as.Date(test_data$TimeStamp,date_format))

#Checking if some numeric variables should be converted to factors
#Status is our dependent variable

#-------Doing same for port number check---------
unique(train_data$portNumber)
train_data$portNumber <- factor(train_data$portNumber)
test_data$portNumber <- factor(test_data$portNumber)

#-------And now for the Type1 count
unique(train_data$TYP1_COUNT)
train_data$TYP1_COUNT <- factor(train_data$TYP1_COUNT)
test_data$TYP1_COUNT <- factor(test_data$TYP1_COUNT)

# Multicollinearity
install.packages('caret')
#caret installation did not include stringi and so installed it separately
install.packages("stringi")
library(caret)
numeric_columns = c("EI65_GEO_ID","region")
numeric_columns_correlation <- cor(train_data[, numeric_columns], use="pairwise.complete.obs")
numeric_columns_correlation

#Better to remove the ID attribute from the data
train_data <- subset(train_data,select = -(ID))
#We will not remove the id attribute for test_data
#test_data <- subset(test_data,select = -(ID))

#Status is an integer number
#class(train_data$status)
#We are trying to use classifier. It makes sense to do the conversion of factors.
#A status of zero indicates available and 1 is occupied. So,
train_data$status <- factor(train_data$status,labels = c("Yes","No"))

# Feature Selection
install.packages("FSelector")
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_122.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(FSelector)
# Calculate weights for the attributes using Info Gain and Gain Ratio
############
# It makes sense to remove the timestamp data now because we have used it in Time_of_Day and weekday
# Similarly the week information is for making the training faster and shoudl be ignored
#
weights_info_gain = information.gain(status ~ EI65_GEO_ID + portNumber + TYP1_COUNT + FREECHARGE + LAST_MODIFIED + VALIDATION_LAST_MODIFIED + IS_LSC_VALIDATED + time_of_Day + region + weekday, data=train_data)
weights_info_gain
weights_gain_ratio = gain.ratio(status ~ EI65_GEO_ID + portNumber + TYP1_COUNT + FREECHARGE + LAST_MODIFIED + VALIDATION_LAST_MODIFIED + IS_LSC_VALIDATED + time_of_Day + region + weekday, data=train_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 7)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "status")
formula_with_most_important_attributes


# 4. Training & Evaluation
# Balanced samples using the "ROSE" package
table(train_data$status)
install.packages("ROSE")
install.packages("e1071")
library(ROSE)
library(RWeka)
library(e1071)


train_data_check <- ovun.sample(status ~ ., data=train_data, method="over",  na.action="na.pass")$data
table(train_data_check$status)

# 2 x 5-fold cross validation
fitCtrl <- trainControl(method="repeatedcv", number=3, repeats=1,classProbs =  TRUE)

#Partitioning....Here we are taking 75% of total values contaiing status
#index <- createDataPartition(train_data$status, p=0.75, list=FALSE)


# training a decision tree model using thfe metric "Accuracy"
#We are going to work on a subset of the data here, only the two week
temp_data <- train_data#[(train_data$week %in% c('14','16')),] #I spent hours on missing the comma :P

model_dt <- train(formula_with_most_important_attributes, data=temp_data, method="J48", trControl=fitCtrl, metric="Accuracy",  na.action=na.omit)
# Show results and metrics
model_dt
model_dt$results

#Trying with Random Forest now
#require(randomForest)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_model <- train(formula_with_most_important_attributes, data=temp_data, method="rf", trControl=fitCtrl, metric="Accuracy",na.action=na.omit,tuneGrid=tunegrid)
# Show results and metrics
rf_model
rf_model$results
#Now K-nearest neighbour
#install.packages("ada")
knn_model <- train(formula_with_most_important_attributes, data=temp_data, method="knn", trControl=fitCtrl, metric="Accuracy",  na.action=na.omit)
# Show results and metrics
knn_model
knn_model$results
#Logistic Regression
logi_model <- train(formula_with_most_important_attributes,data = temp_data,method = "glm",trControl = fitCtrl,metric= "Accuracy",na.action = na.omit)

svm_model <- train(formula_with_most_important_attributes,data = temp_data,method = "svmRadial",trControl = fitCtrl,metric= "Accuracy",na.action = na.omit)
svm_model$results

install.packages("klaR")
library("klaR")
nb_model <- train(formula_with_most_important_attributes,data = temp_data,method = "nb",trControl = fitCtrl,metric= "Accuracy",na.action = na.omit)
nb_model$results

install.packages("adabag")
library("adabag")
#Trying the gradient boosting
fit_grid <- expand.grid(mfinal = (1:3)*3,maxdepth = c(1,3),coeflearn = c("Breiman"))
model_boost <- train(formula_with_most_important_attributes,data = temp_data,method = "AdaBoost.M1",trControl = fitCtrl,metric= "Accuracy",na.action = na.omit)
  # Show decision tree
#model$finalModel
#rf_model$finalModel
#ada_model$finalModel
# Show confusion matrix (in percent)
confusionMatrix(model_dt)
confusionMatrix(rf_model)
confusionMatrix(knn_model)
confusionMatrix(logi_model)
confusionMatrix(svm_model)
confusionMatrix(model_boost)
confusionMatrix(nb_model)


#Let us try the stacking
train_data$pred_knn <- predict(object = knn_model,train_data)
train_data$pred_rf <- predict(object = rf_model,train_data)
train_data$pred_logi <- predict(object = logi_model,train_data)
train_data$pred_dt <- predict(object = model_dt,train_data)
train_data$pred_svm <- predict(object = svm_model,train_data)
train_data$model_boost <- predict(object = model_boost,train_data)
train_data$pred_nb <- predict(object = nb_model,train_data)
#train_data$pred_svm
confusionMatrix(train_data$status,train_data$pred_knn)
confusionMatrix(train_data$status,train_data$pred_dt)
confusionMatrix(train_data$status,train_data$pred_logi)
confusionMatrix(train_data$status,train_data$pred_rf)
confusionMatrix(train_data$status,train_data$pred_svm)
confusionMatrix(train_data$status,train_data$model_boost)
confusionMatrix(train_data$status,train_data$pred_nb)
#Performing the same on test data

test_data$pred_knn <- predict(object = knn_model,test_data)
test_data$pred_rf <- predict(object = rf_model,test_data)
test_data$pred_logi <- predict(object = logi_model,test_data)
test_data$pred_dt <- predict(object = model_dt,test_data)
test_data$pred_svm <- predict(object = svm_model,test_data)
test_data$model_boost <- predict(object = model_boost,test_data)
test_data$model_nb <- predict(object = nb_model,test_data)

#Let us have the predicitons as probabilities for the terms
train_data$pred_knn_prob <- predict(object = knn_model,train_data,type='prob')$Yes
train_data$pred_rf_prob <- predict(object = rf_model,train_data,type='prob')$Yes
train_data$pred_logi_prob <- predict(object = logi_model,train_data,type='prob')$Yes
train_data$pred_dt_prob <- predict(object = model_dt,train_data,type='prob')$Yes
train_data$pred_svm_prob <- predict(object = svm_model,newdata = train_data,type='prob')$Yes
train_data$pred_boost_prob <- predict(object = model_boost,newdata = train_data,type='prob')$Yes
train_data$pred_nb_prob <- predict(object = nb_model,newdata = train_data,type='prob')$Yes
#colSums(is.na(train_data))
#svm_model

#Now getting the predictions on the data set directly using weighted average
#train_data$pred_status_avg <- (train_data$pred_knn_prob$Yes + train_data$pred_dt_prob$Yes + train_data$pred_logi_prob$Yes + train_data$pred_rf_prob$Yes)/4
#train_data$pred_status_avg <- as.factor(ifelse(train_data$pred_status_avg > 0.5,'Yes','No'))
#Let us check accuracy
#levels(train_data$pred_status_avg)
#levels(train_data$status)
#count_corr <- sum(train_data$status == train_data$pred_status_avg)
#count_corr
#count_total <- nrow(train_data)
#acc <- (count_corr/count_total)*100
#acc

#Predictors for top layer models 
install.packages("gbm")
library("gbm")
predictors_top<-c('pred_knn_prob','pred_rf_prob','pred_dt_prob','pred_svm_prob','pred_boost_prob','pred_nb_prob','pred_logi_prob')
model_glm<-train(train_data[,predictors_top],train_data[,'status'],method='dt',trControl=fitCtrl,tuneLength=3,na.action = na.omit)
train_data$pred_status_avg<-predict(model_glm,train_data)
count_corr <- sum(train_data$status == train_data$pred_status_avg)
count_corr
count_total <- nrow(train_data)
acc <- (count_corr/count_total)*100
acc


######################################################
# 5. Predict Classes in Test Data
test_data$pred_knn_prob <- predict(object = knn_model,newdata = test_data,type='prob')$Yes
test_data$pred_rf_prob <- predict(object = rf_model,test_data,type='prob')$Yes
test_data$pred_logi_prob <- predict(object = logi_model,test_data,type='prob')$Yes
test_data$pred_dt_prob <- predict(object = model_dt,test_data,type='prob')$Yes
test_data$pred_svm_prob <- predict(object = svm_model,test_data,type='prob')$Yes
test_data$pred_boost_prob <- predict(object = model_boost,test_data,type='prob')$Yes
test_data$pred_nb_prob <- predict(object = nb_model,test_data,type='prob')$Yes
prediction_classes = predict.train(object=model_glm, newdata=test_data, na.action=na.pass)
#Performing consistency transformation
levels(prediction_classes)[1] <- 0
levels(prediction_classes)[2] <- 1
predictions = data.frame(id=test_data$ID, prediction = as.numeric(levels(prediction_classes))[prediction_classes])
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_faultless_saradine.csv", row.names=FALSE)

