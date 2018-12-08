# Business Analytics
# Data Mining Cup Introduction
#
# Please note, that this script only has the nature of proposal. It provides useful functions for the steps of data mining but does not cover all possibilities.

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-3 students)


######################################################
# 2. Load & Explore the Training Data Set
training_data = read.csv("raw_data2.csv", sep=",", na.string="")

# Show the structure
str(training_data)

# Size
nrow(training_data)
ncol(training_data)

# Show the first and last rows
head(training_data)
tail(training_data)

# Show columns with missing values
colSums(is.na(training_data))

# Explore the class column
table(training_data$a9)

# Explore the price column
mean(training_data$price)
# mean without N/A values
mean(training_data$price, na.rm=TRUE)

aggregate(x=training_data$price, by=list(training_data$a9), FUN=mean, na.rm=TRUE)

hist(training_data$price)
boxplot(training_data$price ~ training_data$a9)


######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data
test_data = read.csv("test.csv", sep=",", na.string="")

# Rename columns
names(training_data)
names(training_data)[names(training_data) == "od"] = "order_date"
names(test_data)[names(test_data) == "od"] = "order_date"

names(training_data)[names(training_data) == "dd"] = "delivery_date"
names(test_data)[names(test_data) == "dd"] = "delivery_date"

names(training_data)[names(training_data) == "a6"] = "salutation"
names(test_data)[names(test_data) == "a6"] = "salutation"

names(training_data)[names(training_data) == "a7"] = "date_of_birth"
names(test_data)[names(test_data) == "a7"] = "date_of_birth"

names(training_data)[names(training_data) == "a8"] = "state"
names(test_data)[names(test_data) == "a8"] = "state"

names(training_data)[names(training_data) == "a9"] = "return_shipment"
names(test_data)[names(test_data) == "a9"] = "return_shipment"


str(training_data)


# Nominal attributes
table(training_data$salutation)
table(test_data$salutation)
SALUTATION_LEVELS = c("Company", "Mr.", "Mrs.")
training_data$salutation = factor(training_data$salutation, levels=2:4, labels=SALUTATION_LEVELS)
test_data$salutation = factor(test_data$salutation, levels=2:4, labels=SALUTATION_LEVELS)
table(training_data$salutation)
table(test_data$salutation)

# If a nominal or ordinal column in the test data set contains more levels than the corresponding column in the training data set, you can add levels to the column in the training data set manually using the following command:
#training_data$salutation = factor(training_data$salutation, levels=c(levels(training_data$salutation), "Family"))

table(training_data$state)
table(test_data$state)
STATE_LEVELS = c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")
training_data$state = factor(training_data$state, levels=1:16, labels=STATE_LEVELS)
test_data$state = factor(test_data$state, levels=1:16, labels=STATE_LEVELS)
table(training_data$state)
table(test_data$state)

RETURN_LEVELS = c("0", "1")
training_data$return_shipment = factor(training_data$return_shipment, labels=RETURN_LEVELS)
test_data$return_shipment = factor(test_data$return_shipment, labels=RETURN_LEVELS)

# Unify "size" column
table(training_data$size)
training_data$size = toupper(training_data$size)
test_data$size = toupper(test_data$size)
table(training_data$size)

# Convert "size" to ordinal
SIZE_LEVELS = c("S", "M", "L", "XL", "XXL", "XXXL")
training_data$size = ordered(training_data$size, levels=SIZE_LEVELS)
test_data$size = ordered(test_data$size, levels=SIZE_LEVELS)

# Date attributes
# Unify date format
date_format = "%Y-%m-%d"
training_data$order_date = as.Date(training_data$order_date, date_format)
test_data$order_date = as.Date(test_data$order_date, date_format)

training_data$delivery_date = as.Date(training_data$delivery_date, date_format)
test_data$delivery_date = as.Date(test_data$delivery_date, date_format)

training_data$date_of_birth = as.Date(training_data$date_of_birth, date_format)
test_data$date_of_birth = as.Date(test_data$date_of_birth, date_format)

# Parsing new data
training_data$order_date_weekday = weekdays(training_data$order_date)
test_data$order_date_weekday = weekdays(test_data$order_date)

training_data$order_date_year = as.numeric(format(training_data$order_date, "%Y"))
test_data$order_date_year = as.numeric(format(test_data$order_date, "%Y"))

training_data$order_date_month = as.numeric(format(training_data$order_date, "%m"))
test_data$order_date_month = as.numeric(format(test_data$order_date, "%m"))

training_data$order_date_day = as.numeric(format(training_data$order_date, "%d"))
test_data$order_date_day = as.numeric(format(test_data$order_date, "%d"))

training_data$order_date_quarter = ceiling(as.numeric(format(training_data$order_date, "%m")) / 3)
test_data$order_date_quarter = ceiling(as.numeric(format(test_data$order_date, "%m")) / 3)

# as an alternative regarding date values you could also use the "lubridate" package

# Calculate new column "delivery time" as difference of order and delivery dates in days
training_data$delivery_time = as.numeric(training_data$delivery_date - training_data$order_date)
test_data$delivery_time = as.numeric(test_data$delivery_date - test_data$order_date)

hist(training_data$delivery_time)
table(training_data$delivery_time, useNA="ifany")

# Negative delivery time is impossible
training_data$order_date[training_data$delivery_time < 0] = NA
test_data$order_date[test_data$delivery_time < 0] = NA

training_data$delivery_date[training_data$delivery_time < 0] = NA
test_data$delivery_date[test_data$delivery_time < 0] = NA

training_data$delivery_time[training_data$delivery_time < 0] = NA
test_data$delivery_time[test_data$delivery_time < 0] = NA

hist(training_data$delivery_time)
table(training_data$delivery_time, useNA="ifany")
boxplot(training_data$delivery_time ~ training_data$return_shipment)

# Manual discretization of "delivery time"
training_data$delivery_time_discret = factor(rep("NA", nrow(training_data)), levels=c("NA", "<= 5d", "> 5d"))
test_data$delivery_time_discret = factor(rep("NA", nrow(test_data)), levels=c("NA", "<= 5d", "> 5d"))

training_data$delivery_time_discret[training_data$delivery_time <= 5] = "<= 5d"
test_data$delivery_time_discret[test_data$delivery_time <= 5] = "<= 5d"

training_data$delivery_time_discret[training_data$delivery_time > 5] = "> 5d"
test_data$delivery_time_discret[test_data$delivery_time > 5] = "> 5d"
table(training_data$delivery_time_discret, useNA="ifany")


# Binning/Discretization
#install.packages("arules")
library(arules)
# equal frequency binning
equal_frequency_cuts_delivery_time = discretize(training_data$delivery_time, categories=5, method="frequency", onlycuts=TRUE)
training_data$delivery_time_discret_ef = cut(training_data$delivery_time, breaks=equal_frequency_cuts_delivery_time, ordered_result=TRUE, right=FALSE)
test_data$delivery_time_discret_ef = cut(test_data$delivery_time, breaks=equal_frequency_cuts_delivery_time, ordered_result=TRUE, right=FALSE)
table(training_data$delivery_time_discret_ef, useNA="ifany")
str(training_data)
# equal width binning: with method "interval"

# another package for binning/discretization would be the "discretization" package
# some classifiers use built-in supervised binning, i.e. entropy-based binning


# Multicollinearity
library(caret)
numeric_columns = c("price", "tax", "order_date_day", "order_date_month", "delivery_time")
# these columns also contain N/A values --> the option "pairwise.complete.obs" should be used
numeric_columns_correlation = cor(training_data[, numeric_columns], use="pairwise.complete.obs")
numeric_columns_correlation
# works for non-N/A only (remove N/A rows or fill with mean, median, etc)
high_cor_columns = findCorrelation(numeric_columns_correlation)
high_cor_columns
# "price" and "tax" are perfectly correlated --> remove "tax" column
training_data$tax = NULL
test_data$tax = NULL



# Feature Selection
#install.packages("FSelector")
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_122.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(FSelector)
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(return_shipment ~ ., data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(return_shipment ~ ., data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 7)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "return_shipment")
formula_with_most_important_attributes


######################################################
# 4. Training & Evaluation
# Balanced samples using the "ROSE" package
#table(training_data$return_shipment)
#install.packages("ROSE")
#install.packages("e1071")
#library(ROSE)
library(RWeka)
#library(e1071)


#training_data = ovun.sample(return_shipment ~ ., data=training_data, method="over", N=500, na.action="na.pass")$data
#table(training_data$return_shipment)

# 2 x 5-fold cross validation
fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2)
#fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2, classProbs=TRUE, summaryFunction=twoClassSummary)

# training a decision tree model using the metric "Accuracy"
model = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="Accuracy",  na.action=na.omit)#tuneGrid=data.frame(C=c(0.1, 0.2, 0.3),M=c(1,1,1,2,2,2)),)
#model = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="ROC",  na.action=na.omit)

# Show results and metrics
model
model$results

# Show decision tree
model$finalModel

# Show confusion matrix (in percent)
confusionMatrix(model)


######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=model, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$ID, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_group_name_number.csv", row.names=FALSE)


  ######################################################
# 7. Upload the Predictions and the Corresponding R Script on DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Maxium number of submissions: 10
#
# Possible errors that could occur:
# - Wrong column names
# - Unknown IDs (if not in Test Data)
# - Missing IDs (if in Test Data but not in Predictions)
# - Wrong file format

