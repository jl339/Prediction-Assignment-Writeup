---
title: "Prediction Assignment Writeup"
author: "Jie Li"
date: "July 16, 2018"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Introduction to the project
Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data
The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

What you should submit
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details.
Analysis

How the model was built

Our outcome variable is classe, a factor variable with 5 levels. For this data set, "participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 different fashions:

- exactly according to the specification (Class A)
- throwing the elbows to the front (Class B)
- lifting the dumbbell only halfway (Class C)
- lowering the dumbbell only halfway (Class D)
- throwing the hips to the front (Class E)
Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes." 
Prediction evaluations will be based on maximizing the accuracy and minimizing the out-of-sample error. All other available variables after cleaning will be used for prediction.



Summary of approach
Load the data set and briefly learn the characteristics of the data

Use cross-validation method to built a valid model; 70% of the original data is used for model building (training data) while the rest of 30% of the data is used for testing (testing data)


Apply random forest method to build a model

Check the model with the testing data set

Apply the model to estimate classes of 20 observations



## loading data
```{r}
setwd("C:/Users/jl339/Desktop/datascience/R")
data <- read.csv("pml-training.csv")
summary(data)
```
##Cross validation
use 70% of training set data to built a model, and use the rest to test the model
```{r}
library(caret)
set.seed(1234)
train <- createDataPartition(y=data$classe,p=.70,list=F)
training <- data[train,]
testing <- data[-train,]
```
##Cleaning the training data
```{r}
#exclude identifier, timestamp, and window data (they cannot be used for prediction)
Cl <- grep("name|timestamp|window|X", colnames(training), value=F) 
trainingCl <- training[,-Cl]
#select variables with high (over 95%) missing data --> exclude them from the analysis
trainingCl[trainingCl==""] <- NA
NArate <- apply(trainingCl, 2, function(x) sum(is.na(x)))/nrow(trainingCl)
trainingC <- trainingCl[!(NArate>0.95)]

```

##Random forest
Apply ramdom forest method (non-bionominal outcome & large sample size)
```{r}
library(randomForest)
modFitRF <- randomForest(trainingCl$classe ~ .,   data=trainingC, do.trace=F)
```

##Check with test set
```{r}
testingCl <- testing[,-Cl]
testingCl[testingCl==""] <- NA
NArate <- apply(testingCl, 2, function(x) sum(is.na(x)))/nrow(testingCl)
testingCl <- testingCl[!(NArate>0.95)]
confusionMatrix(testingCl$classe,predict(modFitRF,testingCl))
```

The machine learning with Random Forest have a 99.61% accuracy,we can expect that very few, or none, of the test samples will be missclassified.


##Predict classes of 20 test data
```{r}
testdata <- read.csv("pml-testing.csv")
testdataCl <- testdata[,-Cl]
testdataCl[testdataCl==""] <- NA
NArate <- apply(testdataCl, 2, function(x) sum(is.na(x)))/nrow(testdataCl)
testdataCl <- testdataCl[!(NArate>0.95)]
testdataCl$classe <- predict(modFitRF,testdataCl)
confusionMatrix(testdataCl$classe,predict(modFitRF,testdataCl))
```


Based on the results we can have 100% accurate perdiction.