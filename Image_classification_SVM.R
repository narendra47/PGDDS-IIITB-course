##################################################################################################################
###----------------------------------     Support Vector Machine Assignment   --------------------------------###
##################################################################################################################

## Business Understanding
## Handwritten image recognition, the goal is to develop SVM model that can correctly identify the digit between
## 0 to 9 written in the image.

## Installing and loading required libraries
install.packages("dplyr")
install.packages("stringr")
install.packages("kernlab")
install.packages("readr")
install.packages("caret")
install.packages("ggplot2")
install.packages("e1071")
install.packages("doParallel")
library(dplyr)
library(stringr)
library(kernlab)
library(readr)
library(caret)
library(ggplot2)
library(e1071)
library(doParallel)

#Loading training and test mnist data

mnist_train <- read.csv("mnist_train.csv", sep = ",", stringsAsFactors = F)
mnist_test <- read.csv("mnist_test.csv", sep = ",", stringsAsFactors = F)

#Understanding the dimensions and structure of df
dim(mnist_train)

str(mnist_train)

##################################################################################################################
###----------------------------------     Data cleaning and prepearation    ----------------------------------###
##################################################################################################################

## Creating column name for the dataframe, considering first column as number value which indicates number ranging from 0 to 9
## and rest 784 columns signifies as pixels.

columnNames <- c("number")

for (i in 1:784) {
  columnNames <- union(columnNames, paste("pixel",i))
}

columnNames <- str_replace_all(columnNames, fixed(" "), "")

colnames(mnist_train) <- columnNames
colnames(mnist_test) <- columnNames

## Checking if na is present in the dataframe
which(is.na(mnist_train) == TRUE)

## Checking which all column has only 0 values
zeroColumnSum <- which(colSums(mnist_train) == 0)

## Removing the columns which have all value 0 from training df
mnist_train <- mnist_train[,-zeroColumnSum]

## Checking again if any columns left with 0
which(colSums(mnist_train) == 0)

## Making the first column to factor
mnist_train$number<-factor(mnist_train$number)
mnist_test$number <- factor(mnist_test$number)

## Since the training data is very large and it may take too much time to execute the command, 
## we are considering 15% of data for training purpose. 


######################################################################################
#----------------------------   Model building    -------------------------------##
######################################################################################
## I am taking two sample of 15% each and see if there is any significant difference in the sample data.
set.seed(100)

index_1 <- sample(1:nrow(mnist_train), 9000, replace = T)

index_2 <- sample(1:nrow(mnist_train), 9000, replace = T)

sample_data_1 <- mnist_train[index_1, ]
sample_data_2 <- mnist_train[index_2, ]

ggplot(sample_data_1, aes(x=as.factor(sample_data_1$number))) + geom_bar()

ggplot(sample_data_2, aes(x=as.factor(sample_data_2$number))) + geom_bar()

## After looking at both sample data distribution for 0-9 its almost look same, so I am assuming the data is properly sampled.
## 
## taking the best sampled data between them for further analysis
sample_train <- sample_data_2

## Splitting data in training and validation in ratio of 70:30

train.indices <- sample(1:nrow(sample_train), 0.7*nrow(sample_train))

train <- sample_train[train.indices, ]
train_validate <- sample_train[-train.indices, ]

##  Creating clusters for parallel processing, it makes processing fast for model building

clusters <- makeCluster(detectCores())
registerDoParallel(clusters)

SVM_model_linear <- ksvm(number~ ., data = train, scale = F, kernel = "vanilladot")

predicted_linear<- predict(SVM_model_linear, mnist_test)

#Confustion matrix - linear kernal
confusionMatrix(predicted_linear,mnist_test$number)

##Below is the Overall statistics of the linear model
##  Accuracy : 0.9148          
##  95% CI : (0.9091, 0.9202)
##  No Information Rate : 0.1135          
##  P-Value [Acc > NIR] : < 0.00000000000000022 

## Let's build a SVM model with polynomial kernal and see the accuracy
SVM_model_poly <- ksvm(number~ ., data = train, scale = F, kernel = "polydot")
predicted_poly <- predict(SVM_model_poly, mnist_test)

#Confustion matrix - Polynomial kernal
confusionMatrix(predicted_poly, mnist_test$number)

##Below is the Overall statistics of the polynomial kernal
##  Accuracy : 0.9148          
##  95% CI : (0.9091, 0.9202)
##  No Information Rate : 0.1135          
##  P-Value [Acc > NIR] : < 0.00000000000000022 

## Both linear and polynomial kernel has same accuracy, now let's look for rbf and see if the model accuracy vary a lot

## Let's build a SVM model using RBF kernal with default gamma and C value
SVM_model_RBF <- ksvm(number~ ., data = train, scale = F, kernel = "rbfdot")
predicted_RBF<- predict(SVM_model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(predicted_RBF,mnist_test$number)

##Below is the Overall statistics of the RBF kernal
##  Accuracy : 0.953          
##  95% CI : (0.9503, 0.9586)
##  No Information Rate : 0.1135          
##  P-Value [Acc > NIR] : < 0.00000000000000022

## After looking at all three model, the rbf kernel model giving more accuracy. 
## We will proceed with RBF kernel and try to tune/CV the model.

#####################################################################################
#------------------   Hyperparameter tuning and Cross Validation -------------------#
#####################################################################################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e CV -   Cross Validation.
# Number -   implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)



## Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(9)
grid <- expand.grid(.sigma=c(0.0000001, 0.0000002, 0.0000005), .C=c(1,3,5,10))


## let's see the fit for whole sample data for the range of sigma and cost factor. 
## The below function may take around 20-30 mins to execute based on speed of system (I have used i7 processor)

svm_rbf_fit <- train(number~., data=sample_train, method="svmRadial", metric="Accuracy", 
                 tuneGrid=grid, trControl=trainControl)

print(svm_rbf_fit)

# Resampling results across tuning parameters:

#sigma      C   Accuracy   Kappa    
#0.0000001   1  0.9456663  0.9396040
#0.0000001   3  0.9561110  0.9512122
#0.0000001   5  0.9581109  0.9534363
#0.0000001  10  0.9591116  0.9545486
#0.0000002   1  0.9589998  0.9544260
#0.0000002   3  0.9653335  0.9614659
#0.0000002   5  0.9659999  0.9622068
#0.0000002  10  0.9671112  0.9634420
#0.0000005   1  0.9693331  0.9659133
#0.0000005   3  0.9714444  0.9682599
#0.0000005   5  0.9714444  0.9682599
#0.0000005  10  0.9714444  0.9682599

## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.0000005 and C = 3.

plot(svm_rbf_fit)

#######################################################################################
##-----------------------          Final sVM Model        --------------------------##
#######################################################################################

## Building final model based on the C and sigma value which we got after tuning the hyperparameter for range of values.
## We then look for if the model is overfit or not.

SVM_Model_Final <- ksvm(number~ ., data = train, scale = F, kernel = "rbfdot", C=3, kpar = list(sigma = 0.0000005))

predict_final <- predict(SVM_Model_Final, mnist_test)

stopCluster(clusters)

confusionMatrix(predict_final, mnist_test$number)

##  By looking at accuracy (96.6%) on mnist_test data, it's clear that model is not over fit.
##  It's giving good prediction for handwritten digits, and i feel the model is performing well.
##  The final hyperparameter C = 3 and sigma = 0.0000005.
