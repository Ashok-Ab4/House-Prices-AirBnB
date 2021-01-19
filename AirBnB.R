library(caret)
library(dplyr)
library(tidyverse)
library(MASS)
library(arm)
library(missForest) 
library(psych)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(caTools)
library(ggcorrplot)
#loading the dataset
abnb <- read.csv("AB_NYC_2019.csv", header=T, stringsAsFactors = F)

##PRE PROCESSING 
#analysing the basic structure 
str(abnb)
dim(abnb)
#let's check to see if there is any duplicated data in the database
cat("The number of duplicated rows are", nrow(abnb) - nrow(unique(abnb)))

#the dataset currently has 48895 rows and 16 columns. Lets check the missing data statistics
abnb2=read.csv("AB_NYC_2019.csv", header=T, na.strings=c("","NA"),stringsAsFactors = F)
sapply(abnb2, function(x) sum(is.na(x)))       

#from the data it is clearly observed that we have quite a bit of missing data in the last_review and reviews_per_month columns
#as well as a few for the name and host_name columns
#the large number of missing values for last_review and reviews_per_month can be explained by those units not having any reviews at all. 
#Therefore the values have been set to zero instead of removing them from the data completely.
abnb2$last_review[which(is.na(abnb2$last_review))] <- "NoReview"
abnb2$reviews_per_month[which(is.na(abnb2$reviews_per_month))] <- 0.0

#lets check the missing data again
sapply(abnb2, function(x) sum(is.na(x)))    
#the NA values have been set to zero and NoReview

#Upon exploration of the dataset it was found that the missing name and host_name values could not be explained by any phenomenon except them being test entries or mistakes in data entry so those values will be removed from the database.
abnb2 <- na.omit(abnb2)

#lets check the missing data a final time
sapply(abnb2, function(x) sum(is.na(x))) 
str(abnb2)

#EXPLORATORY DATA ANALYSIS AND DATA CLEANING
#first lets check the correlation matrix of the data
df <- abnb2[which(sapply(abnb2, is.numeric),)]
options(repr.plot.width=14, repr.plot.height=10)
cr <- round((cor(df, use = "complete.obs")), 2)
ggcorrplot(cr, lab = TRUE, show.legend = TRUE,
           tl.cex = 15, #hc.order = T,
           type = "upper", # remove to show upper and lower triangle
           lab_size = 5, sig.level = .2,
           color = c("deepskyblue", "white", "green"))

#Price is the dependent variable making it the most important. Let's run some statistics on it. 
summary(abnb2$price)
#there seem to be some very obvious outliers in this data with the minimum and maximum values having huge variations from the mean.
#first let's remove the zeros from the dataset because the price can never be zero. 
ggplot(data = abnb2, mapping = aes(x=abnb2$price))+geom_histogram(fill='cyan')+
xlab("price")

#as we can see the data is extremely skewed to the right. Removing outliers seems like the way to go as long as the number of outliers doesnt significantly reduce sample size. let's check that.
outlier_price <- abnb2[which(abnb2$price>300),] 
outlier_price

#since there are 2120 rows where price is greater than 500, we can omit those rows from the dataset completely to reduce the skewness. Let's do that.
abnb2 <- abnb2[which(abnb2$price<300),]

#let's check the distribution again
ggplot(data = abnb2, mapping = aes(x=abnb2$price))+geom_histogram(fill='blue')+
xlab("price")
#The distribution is now much more normalized 

#minimum nights is also a feature that appears to have some skewed values. Let's explore that a bit in detail
summary(abnb2$minimum_nights)
#1250 as a value seems very odd for the dataset. lets visualize this and figure out if this is an outlier or not.
ggplot(data = abnb2, mapping = aes(x=abnb2$minimum_nights), y=(..count..)) + geom_bar( fill ='blue')
xyz <- abnb2[which(abnb2$minimum_nights > 100),]
glimpse(xyz)
#since it is only 150 rows, we will remove those values from the database too. 
abnb2 <- abnb2[which(abnb2$minimum_nights<100),]

#lets move on to the neighbourhood groups. There are 5 distinct neighborhoods in New York and their distributions are as follows
ggplot(data = abnb2, mapping = aes(x=as.factor(abnb2$neighbourhood_group),y=(..count..))) +geom_bar (fill = 'black')+
  xlab("Neighborhood groups")+
  ylab("count")
#Manhattan and Brooklyn clearly have the most number of airbnb bookings compared to the other three. 

#price varies depending on the kind of rooms available as well. Let's check that out next
ggplot(abnb2, aes(x=as.factor(room_type), y=(..count..))) +geom_bar( fill="light blue" )
ggplot(abnb2[which(abnb2$room_type=='Entire home/apt'),],aes(x=neighbourhood_group,y=price, fill=neighbourhood_group)) +
geom_boxplot() 
ggplot(abnb2[which(abnb2$room_type=='Private room'),],aes(x=neighbourhood_group,y=price, fill=neighbourhood_group)) +
geom_boxplot()
ggplot(abnb2[which(abnb2$room_type=='Shared room'),],aes(x=neighbourhood_group,y=price, fill=neighbourhood_group)) +
geom_boxplot()
# A bit of an obvious conclusion but entire homes and private rooms are much more popular than shared rooms 
# And manhattan is the neighborhood with the highest prices for all 3 types of rooms.

#The data is significantly cleaner without compromising its integrity or potentially overfitting. Now it is time to select the features we will initially use for the regression models.

#lets initially drop name and host_name because they do not contribute to price in any forseeable way. The name of the listing might contribute to the number of reviews but that isnt something that can be quantified in a model. Lets remove them

abnb2$name <- NULL 
abnb2$host_name <- NULL
abnb2$host_id <- NULL
abnb2$id <- NULL 
#neighborhood has too many potential factors and some neighborhoods dont have a lot of data which could create conflicting models. let's drop that too.
unique(abnb2$neighbourhood)
abnb2$neighbourhood <- NULL
#Last_review by itself cannot provide any information that is useful. But we will convert it into a new variable called Latest_review taking only the year value and assigning a 5 factor value to it.
year_last_review <- format(as.Date(abnb2$last_review),"%Y")
latest_review <- year_last_review
latest_review <- as.numeric(latest_review)
latest_review[which(is.na(latest_review))]=0
latest_review <- as.factor(latest_review)
latest.review <- c()
for (i in 1:length(latest_review)) {
  if(is.na(latest_review[i])){
    latest.review[i]="No reviews"
  }else if(latest_review[i]=="2019"){
    latest.review[i]="6 months"
  }else if(latest_review[i]=="2018"){
    latest.review[i]="1 yr ago"
  }else if(latest_review[i]=="2016"){
    latest.review[i]="3 yrs back"
  }else if(latest_review[i]=="2015"){
    latest.review[i]="4 yrs back"
  }else if(latest_review[i]=="2014"){
    latest.review[i]="5 yrs back"
  }else {
    latest.review[i]="6 or more yrs back"
  }
}

table(latest.review)
abnb2 <-  cbind(abnb2,latest.review)
#Now that we have done this, we can remove last_review from the dataset
abnb2$last_review <- NULL 
#Lets convert neighbourhood_group and room_type to factors as well
abnb2$neighbourhood_group <- as.factor(abnb2$neighbourhood_group)
abnb2$room_type <- as.factor(abnb2$room_type)
glimpse(abnb2)

#TRAINING TEST SPLIT
#Since we do not have the computational ability to train and tune models for a dataset with 45k rows, we will create a random sample of 15k that will be split into training and test datasets.
set.seed(123)
abnbf <- abnb2[sample(nrow(abnb2),15000),]
sample <- sample.split(abnbf,SplitRatio = 0.75)
train1 <- subset(abnbf,sample==TRUE)
test1 <- subset(abnbf,sample==FALSE)

#Lets remove price from the test dataset, but keep it in a separate variable to compare model accuracy in the future.
test1.price <- test1$price
test1$price <- NA 

#RMSE 
rmse <- function(ActualVal,predictedVal)
{sqrt(mean((ActualVal-predictedVal)^2))}

#Linear Model
start.time <- proc.time()
lm1 <- lm(price ~ . , data = train1)
stop.time <- proc.time()
run.time <- stop.time-start.time
print(run.time)
summary(lm1)
varImp(lm1)
head(sort(abs(lm1$coefficients),decreasing = TRUE),n=20)

#adjusted R squared is only at 50%. Let's try and add some interaction terms.
lm2 <- lm(price~.+number_of_reviews:reviews_per_month+latitude:longitude+availability_365:minimum_nights,data = train1)
summary(lm2)
#barely any change to Error rates or r squared values. Interaction terms dont seem to be making any difference.
plot(lm2)
#Since the interaction terms seem to be contributing very little towards Adjusted R squared let's try adding some non linear components to the model and see if those make a difference.
#This may not yield favorable results because the residual plots do not suggest any staggering non linearity in the variables.
lm3 <- lm(price~.+poly(minimum_nights,5)+poly(availability_365,5)+poly(calculated_host_listings_count,5),data=train1)
summary(lm3)
#Even though some of the non linear terms are showing up as significant in the summary, they are doing little to change the R squared or the error rates at all.
#Lets move forward with the assumption that there is insufficient data to make accurate predictions on the response variable and accept the 50% r squared value of the simplest model. 

#MODEL FITTING
#Let us try to minimize the RMSE values by choosing a bunch of different cross validated models to fit the training data over. 

ctrl <- trainControl(method = "cv", number=10)
ctrl1 <- trainControl(method = "cv",number = 10)
set.seed(123)

#1 Linear model- 
set.seed(123)
lm.fit <- train(price ~., data = train1, method = "lm", trControl = ctrl)
lm.fit

RMSE_linear <- rmse(train1$price, predict(lm.fit,newdata = train1))
RMSE_linear

#2 Ridge model- 
set.seed(123)
ridge.fit <- train(price ~.,data = train1, preProcess = c("center", "scale"),method = "ridge",trControl=ctrl)

ridge.fit

RMSE_ridge <- rmse(train1$price, predict(ridge.fit,newdata = train1))
RMSE_ridge

#3 Lasso model -
set.seed(123)
lasso.fit <- train(price ~.,
                     data = train1, preProcess = c("center", "scale"),
                     method = "lasso", trControl=ctrl)

lasso.fit

RMSE_lasso <- rmse(train1$price, predict(lasso.fit,newdata = train1))
RMSE_lasso

#4 KNN model - 
set.seed(123)

knn.fit <- train(price ~.,
                   data = train1, preProcess = c("center", "scale"),
                   method = "knn", trControl=ctrl)

knn.fit

RMSE_knn <- rmse(train1$price, predict(knn.fit,newdata = train1))
RMSE_knn


#5 Random Forest
set.seed(123)
rf.fit <- train(price~ ., data=train1, 
                  method="rf")
rf.fit
RMSE_rf <- rmse(train1$price, predict(rf.fit,newdata = train1))
RMSE_rf

#6 boosting tree model
set.seed(123)
boost.fit <- train(price ~., data=train1, 
                     method="gbm",trControl = ctrl )
boost.fit
RMSE_boost <- rmse(train1$price, predict(boost.fit,newdata = train1))
RMSE_boost

#7 Bagging tree model
set.seed(123)
bag.fit <- train(price ~ ., data=train1, 
                   method="treebag",tuneLength=4,
                   trControl=ctrl)
bag.fit
RMSE_bag <- rmse(train1$price,predict(bag.fit,newdata = train1))
RMSE_bag

#Model Comparison

#This plot compares the RMSEs for all the models and shows the minimum and maximum RMSE of the models. 
#This helped us  finalize the best model.


models<- list("lm"=lm.fit, "gam" = gam.fit,
              "lasso" = lasso.fit, "ridge"=ridge.fit,
              "BaggingTree"=bag.fit, "RF"=rf.fit,
              "BoostingTree" = boost.fit)

hitter.resamples<- resamples(models)
summary(hitter.resamples)

#plot performances
bwplot(hitter.resamples, metric="RMSE")
bwplot(hitter.resamples, metric="Rsquared")


test.rmse <- data.frame( model = c("lm", "ridge", "lasso", "knn", "rf", "boost", "bag"),
                         rmse = c(RMSE_linear,RMSE_ridge, RMSE_lasso, RMSE_knn, RMSE_rf, RMSE_boost,RMSE_bag))


test.rmse <- test.rmse[order(test.rmse$rmse, decreasing = TRUE),]

test.rmse$model <- factor(test.rmse$model, levels = test.rmse$model)

plot(test.rmse, main = "Model Comparison")

predict_rf_test <- predict(rf.fit, test1)

prediction <- data.frame( predict_rf_test = predict_rf_test)
prediction$actual_rf_test <- test1.price
prediction$predictiondiff <- abs(prediction$predict_rf_test-prediction$actual_rf_test)
summary(prediction$predictiondiff)
