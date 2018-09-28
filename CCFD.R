##################--------------credit card fraud detection--------------############

data <- read.csv("C:\\Users\\prakh\\Desktop\\pocs\\creditcard.csv")
head(data)

dim(data)

names(data)  ###to get names of the columns in the data

princomp(data) # to get the principal components in the data
pc <- princomp(data)

pc$loadings #
pc$scores

## 99 percent says no fraud ...highly imbalanced data
prop.table(table(data$Class)) *100

table(data$Class,data$Time)

##max amount transacted is 25,691.16
max(data$Amount)


str(data) #all variables are of class numeric except for class int

### for EDA library caret 
library(caret)
#univariate - distributions,bivariate-correlations,multivariate-correlations

#pairwise plots for all the features by the class

featurePlot(x=data$V2, y = data$Class,plot = 'pairs') #this can be done for all the var from v1 to 28

featurePlot(x=data$V4,y=data$Class,plot = 'density') #throwing null value

install.packages("ellipse")
library(ellipse)

install.packages("corrplot")
library(corrplot)

corrplot(cor(data[,2:29]))




###to handle imbalance data library(unbalanced) or library ROSE is used to rebalance
install.packages("unbalanced")
library(unbalanced)

data1 <- data[,-1] #removing the time column
dim(data1)


library(ROSE)
#creating balanced data
data_bal <- ROSE(Class~.,data=data1,p=0.5,seed = 1)$data

table(data_bal$Class)            
str(data_bal)
data_bal$Class <- as.factor(data_bal$Class)


###creating data partition into test and train
library(caret)
set.seed(1234)
train_index <- createDataPartition(data_bal$Class,times = 1,p=0.5) ##its returning a list which cant be
#subscripted directly for partitioning hence use the unlist funcn

head(train_index)

train <- data_bal[unlist(train_index),]
dim(train)
prop.table(table(train$Class))

head(train)

## test data
test <- data_bal[-unlist(train_index),]
dim(test)
str(test)

###---------------cross validation setup----------------------####
control <- trainControl(method = 'repeatedcv',number = 3,repeats = 3)
metric <- 'Accuracy'
preProcess <- c('center','scale')




######------------------------performing Lda-----------------------#######
library(MASS)
set.seed(1234)

fit.lda <- caret::train(Class~.,data=train,method='lda',metric=metric,preprocess=preProcess,
                 trControl=control)
fit.lda


fit.lda$results
#     parameter  Accuracy     Kappa      AccuracySD        KappaSD
#1      none     0.8874119  0.7749093    0.0009326294    0.001863958

predictions_lda <- predict(fit.lda,data=test)
confusionMatrix(train$Class,predictions_lda)
varImp(fit.lda)
# Accuracy : 0.8876 Kappa : 0.7752  


############---------------logistic regression-----------------------###############
set.seed(1234)

fit.glm <- caret::train(Class~.,data=train,method='glm',metric=metric,
                        trControl=control) #no preprocess is required
fit.glm
fit.glm$results  ## these are training accuracy
#     parameter  Accuracy    Kappa      AccuracySD        KappaSD
#1      none     0.9350112  0.870044    0.0008765361     0.001752496
##the kappa sd should be low as it tells the standard deviation and accuracy should be high


fit_glm <- glm(Class~.,data = train,family = binomial(link = 'logit'))
summary(fit_glm)
pred_glm <- as.factor(ifelse(predict(fit_glm,data=test,type='response')>0.5,1,0))
length(pred_glm)==length(test$Class)
length(test$Class)
str(pred_glm)
confusionMatrix(test$Class,pred_glm[1:142403]) #Kappa : 0.8703 Accuracy : 0.9351 

###----------------------glm RUN2-----------------------------------
pred_glm <- as.factor(ifelse(predict(fit_glm,data=test,type='response')>0.6,1,0))
length(pred_glm)==length(test$Class)
length(test$Class)
str(pred_glm)
confusionMatrix(test$Class,pred_glm[1:142403])
#Accuracy : 0.931  Kappa : 0.862

####-------------------glm RUN3------------------------
pred_glm <- as.factor(ifelse(predict(fit_glm,data=test,type='response')>0.3,1,0))
confusionMatrix(test$Class,pred_glm[1:142403])
##kappa 84.8 accuracy 92.4

####glmnet is used for imbaanced data



############-----------------------Support vector Machines -------------------------------
library(kernlab)
set.seed(1234)

 # fit.svm <- caret::train(Class~.,data=train,method='svmRadial',metric=metric,preprocess=preProcess,
                       # trControl=control,fit=F)
#fit.svm

  
names(getModelInfo())



#########--------------KNN classification---------------------------#########
set.seed(1234)

fit.knn <- caret::train(Class~.,data=train,method='knn',metric=metric,preprocess=preProcess,
              trControl=control)




##################-----------------------Naive Bayes--------------------###################
set.seed(1234)
library(klaR)

fit.nb <- caret::train(Class~.,data=train,method='nb',metric=metric,preprocess=preProcess,
                       trControl=control)
predictions_nb <- predict(fit.nb,data=test)
length(predictions_nb)==length(test$Class)
str(predictions_nb)
confusionMatrix(test$Class,predictions_nb[1:142403],dnn=c("actual","predicted"))
#Accuracy : 0.9916 Kappa : 0.9831 best model so far



###########---------------classification and regression tree model----------------######3
set.seed(1234)
library(rpart)

fit.cart <- caret::train(Class~.,data=train,method='rpart',metric=metric,
                       trControl=control)


###############----------------bagging cart----library(ipred)----------##########
library(ipred)
set.seed(1234)
fit.treebag <- caret::train(Class~.,data=train,method='treebag',metric=metric,
                         trControl=control)






###-----------------------comparing models--------------------#####

#comparison of models
results <- resamples(list(lda=fit.lda,logistic=fit.glm,naive_bayes=fit.nb)) 
#to run the comparisons
summary(results)
#consistency of the resampling results
bwplot(results)
##############----------------------------------------------------------------#########


###binary classification using keras as deep learning library









