###Telecom customer churning

###reading data sets
train <- read.csv("C:\\Users\\prakh\\Desktop\\pocs\\Telecom_Train.csv")
test <- read.csv("C:\\Users\\prakh\\Desktop\\pocs\\Telecom_Test.csv")


#### dropping the first columns from both the datsets and making it workable
train_df <- train[-1]
test_df <- test[,-1]

head(train)
head(test)
edit(train)
#######EDA univariate analysis for numerical data by using boxplot and density plots.for this we have 
#separate the categorical variables from the numerical data.
summary(train_df)
str(train_df)
dim(train_df)  

num <- c(2,6:19) ###numerical columns
###creating boxplots for numerical data ..creating new data of only numerical features

train_bxplt <- train_df[,num]

str(train_bxplt)
dim(train_bxplt)

####creating test data sets
test_bxplt <- test_df[,num]

##boxplot

par(mfrow=c(2,3))
 for (i in 1:ncol(train_bxplt)){
   boxplot(train_bxplt[,i],main= names(train_bxplt[i]))
 }
 ## barplot for categorical data

train_df_cat <- train_df[,-(num)]
str(train_df_cat)
head(train_df_cat)

#####
test_df_cat <- test_df[,-(num)]

par(mfrow=c(2,1))
barplot(table(train_df_cat$state))
barplot(table(train_df_cat$area_code),color="red")
barplot(table(train_df_cat$international_plan),color="blue")
barplot(table(train_df_cat$voice_mail_plan))
barplot(table(train_df_cat$churn),color="blue")

###no of states
table(traindf_cat$state)

###creation of dummy variables
install.packages("dummy")
library(dummy)
?dummy
cat <- dummy(train_df_cat[,c(1,2)],p="all")
View(cat)

######   dummy variables for test data set

cat_test <- dummy(test_df_cat[,c(1,2)],p="all")
head(cat_test)
final_test <- data.frame(test_bxplt,cat_test,test_df[20])


###creating a final data set consisting of numericl df and cat df
final_train <- data.frame(train_bxplt,cat,train_df[20])
dim(final_train)
dim(cat)
str(final_train)




## does the outliers have any impact in the model so we ll run diff model with the outliers
#we can then remove or impute them and then we can rerun the model and compare the accuracy and
#then we can validate if outliers have any impact

###prepare test data set





##install libraries  caret, mlr and
library(caret)
install.packages("mlr")
library(mlr)


###building basic logistic based classifier

fit_glm <- glm(churn~.,data = final_train,family = binomial(link = 'logit') )
summary(fit_glm)

##predicting the values using the predict functions

pred <- ifelse(predict(fit_glm,
                       final_train,
                       type ='response')>0.50,'yes','no')
##these values can be put into final train dtaset
final_train$pred <- as.factor(pred)
head(pred)

###creating the confusion matrix
confusionMatrix(final_train$churn,final_train$pred)



###### creating models on the test data set
glm_fit_test <- glm(churn~.,data=final_test,family = binomial(link = 'logit'))
final_test$pred <- ifelse(predict(glm_fit_test,final_test,type = 'response')>0.50,'yes',
                          'no')
head(final_test$pred)
str(final_test$pred) ##its a character type
str(final_test$churn) ##its a factor type therefore these 2 should be of same type fo the 
#confusion matrix

###conversion into factor type
final_test$pred <- as.factor(final_test$pred)

head(final_test)
confusionMatrix(final_test$churn,final_test$pred)


###here kappa statistic has increased to 0.24 from 0.19 which is good but it has to be more

####changing  probability thresholds

####--------------------RUN1----------------------- prob =0.60

pred <- ifelse(predict(fit_glm,
                       final_train,
                       type ='response')>0.60,'yes','no')

final_train$pred <- as.factor(pred)

confusionMatrix(final_train$churn,final_train$pred)



final_test$pred <- ifelse(predict(glm_fit_test,final_test,type = 'response')>0.60,'yes',
                          'no')
final_test$pred <- as.factor(final_test$pred)
confusionMatrix(final_test$churn,final_test$pred)

prop.table(table(final_train$churn))
prop.table(table(final_train$pred))
prop.table(table(final_test$churn))
prop.table(table(final_test$pred))

------------------------------------------------------------------
##RUN2 prob=0.4
  pred <- ifelse(predict(fit_glm,
                         final_train,
                         type ='response')>0.40,'yes','no')

final_train$pred <- as.factor(pred)

confusionMatrix(final_train$churn,final_train$pred)



final_test$pred <- ifelse(predict(glm_fit_test,final_test,type = 'response')>0.40,'yes',
                          'no')
final_test$pred <- as.factor(final_test$pred)
confusionMatrix(final_test$churn,final_test$pred)

prop.table(table(final_train$churn))
prop.table(table(final_train$pred))
prop.table(table(final_test$churn))
prop.table(table(final_test$pred))


------------------------------------RUN3-----------------------------------------#prob=0.3
  pred <- ifelse(predict(fit_glm,
                         final_train,
                         type ='response')>0.30,'yes','no')

final_train$pred <- as.factor(pred)

confusionMatrix(final_train$churn,final_train$pred)



final_test$pred <- ifelse(predict(glm_fit_test,final_test,type = 'response')>0.30,'yes',
                          'no')
final_test$pred <- as.factor(final_test$pred)
confusionMatrix(final_test$churn,final_test$pred)

prop.table(table(final_train$churn))
prop.table(table(final_train$pred))
prop.table(table(final_test$churn))
prop.table(table(final_test$pred)) 
###kappa increased to 0.4 from 0.37 which is a good sign and acuuracy to 86% from 85%


###cross validation - how to stabilize the results,removing overfitting from the models
#creating a train control object using the caret library

train_control <- trainControl(method = 'cv',number = 10)
#small number runs faster otherwise for robust models use no =5 or 10
metric <- 'accuracy'

##decision tree based methods
install.packages("rpart")
library(rpart)
library(caret)
fit_dt <- caret::train(churn~.,data=train_df,trControl=train_control,method='rpart',metric=metric)

predictions <- predict(fit_dt,test_df)
pred <- cbind(test_df,predictions)

confusionMatrix(pred$churn,pred$predictions)
##kappa 0.343 and accuracy 0.88

names(getModelInfo())

############-----------------------------c5.0 method------------------#########################
library(C50)
fit_dtc50 <- caret::train(churn~.,data=train_df,trControl=train_control,method='C5.0',metric=metric)
fit_dtc50
predictions <- predict(fit_dtc50,test_df)
predc50 <- cbind(test_df,predictions)
confusionMatrix(predc50$churn,predc50$predictions)
varImp(fit_dtc50)
#Kappa : 0.8146 Accuracy : 0.961
#best model trials = 20, model = rules and winnow = FALSE.

#who is likely to churn?
#ans-           Reference
#Prediction   noyes   yes
#             no  1436    7
#                  58  16


#####boosting decision tree based methods   ----package "gbm" required
fit_bstTree <- caret::train(churn~.,data=train_df,trControl=train_control,method='bstTree',metric=metric)
fit_bstTree
predictions <- predict(fit_bstTree,test_df)
predbstTree <- cbind(test_df,predictions)
confusionMatrix(predbstTree$churn,predbstTree$predictions)
varImp(fit_bstTree)

#The final values used for the model were mstop = 150, maxdepth = 3 and nu = 0.1.
#Kappa : 0.7182 Accuracy : 0.9442   

##_______________c5.0cost-----------------------------------------------
fit_c50cost <- caret::train(churn~.,data=train_df,trControl=train_control,method='C5.0Cost',metric=metric)
fit_c50cost
predictions <- predict(fit_c50cost,test_df)
predc50cost <- cbind(test_df,predictions)
confusionMatrix(predc50cost$churn,predc50cost$predictions)
varImp(fit_c50cost)

#Kappa : 0.8007  Accuracy : 0.958

#####---------c50Rules----------------------------
fit_c50Rules <- caret::train(churn~.,data=train_df,trControl=train_control,method='C5.0Rules',metric=metric)
fit_c50Rules
predictions <- predict(fit_c50Rules,test_df)
pred_c50rules <- cbind(test_df,predictions)
confusionMatrix(pred_c50rules$churn,pred_c50rules$predictions)
varImp(fit_c50Rules)
# Accuracy : 0.9472 Kappa : 0.7441

########-------------------treebag---------------------

fit_treebag <- caret::train(churn~.,data=train_df,trControl=train_control,method='treebag',metric=metric)
fit_treebag
predictions <- predict(fit_treebag,test_df)
pred_treebag <- cbind(test_df,predictions)
confusionMatrix(pred_treebag$churn,pred_treebag$predictions)
varImp(fit_treebag)

#Kappa : 0.7665  Accuracy : 0.9508

###################----------------xgbtree-xtreme gradient boosting tree--------------lib-"xgboost"
fit_xgbtree <- caret::train(churn~.,data=train_df,trControl=train_control,method='xgbTree',metric=metric)
fit_xgbtree
predictions <- predict(fit_xgbtree,test_df)
pred_xgbTree <- cbind(test_df,predictions)
confusionMatrix(pred_xgbTree$churn,pred_xgbTree$predictions)
varImp(fit_xgbtree)
# Accuracy : 0.9544  Kappa : 0.78  

#####-------------------------------------bagging--------------------------------####
#here we ll take 500 obs to train the grid search model for ensemble learning coz it takes time to finish
#one run
##bagging is boot strap aggregation

control <- trainControl(method = 'repeatedcv',number = 10,repeats=3)
seed <- 1234
set.seed(seed)
mtry <- sqrt(ncol(train_df))
tunegrid <- expand.grid(.mtry=mtry)

fit_rf_default <- caret::train(churn~.,data=train_df[1:500,],method='rf',trControl= control,metric=metric)
fit_rf_default
predictions <- predict(fit_rf_default,test_df)
pred_rf_deafault <- cbind(test_df,predictions)
confusionMatrix(pred_rf_deafault$churn,pred_rf_deafault$predictions)

varImp(fit_rf_default)

#Accuracy : 0.9304  Kappa : 0.6539 

##########---------------SEARCH PARAMETER = random-----------------------------##########################
##   grid search and greedy search are also there  


control <- trainControl(method = 'repeatedcv',number = 10,repeats=3,search = 'random')

seed <- 1234
set.seed(seed)
mtry <- sqrt(ncol(train_df))

#tunegrid <- expand.grid(.mtry=mtry) isnt used here instead another parameter tune length is used.

fit_rf_random <- caret::train(churn~.,data=train_df[1:1000,],method='rf',trControl= control,
                              metric=metric,tuneLength=15)
fit_rf_random

predictions <- predict(fit_rf_random,test_df)
pred_rf_random <- cbind(test_df,predictions)
confusionMatrix(pred_rf_random$churn,pred_rf_random$predictions)

varImp(fit_rf_default)
#Kappa : 0.699 Accuracy : 0.94
plot(fit_rf_random)

###################-------------GRID SEARCH------------------------------------########################

control <- trainControl(method = 'repeatedcv',number = 10,repeats=3,search = 'grid')
seed <- 1234
set.seed(seed)
#      mtry <- sqrt(ncol(train_df))
tunegrid <- expand.grid(.mtry=c(1:15)) ###here we can change increase the predictors from 15 to 25
#coz there are 70 predictor var but ut will only increase the running time

fit_rf_tunegrid <- caret::train(churn~.,data=train_df[1:1000,],method='rf',trControl= control,
                               metric=metric,tunegrid=tunegrid)
fit_rf_tunegrid
predictions <- predict(fit_rf_tunegrid,test_df)
pred_rf_tunegrid <- cbind(test_df,predictions)
confusionMatrix(pred_rf_tunegrid$churn,pred_rf_tunegrid$predictions)

varImp(fit_rf_tunegrid)
plot(fit_rf_tunegrid)

##Kappa : 0.7123 Accuracy : 0.9424

--------------------------------------------------------------




