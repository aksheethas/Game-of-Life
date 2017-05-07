# Machine Learning - Final Project 
# Conway's Reverse Game of Life 
# Random forest implementation

install.packages("randomForest")
library(randomForest)
library(MASS)

setwd("~/Desktop/Game-of-Life")
set.seed(35633)

# read in data
train_data <- read.csv("train.csv")
str(train_data)


#prepare train data for 5 different models according to delta                         
train_data_delta1 <- train_data[train_data$delta == 1,]
train_data_delta2 <- train_data[train_data$delta == 2,]
train_data_delta3 <- train_data[train_data$delta == 3,]
train_data_delta4 <- train_data[train_data$delta == 4,]
train_data_delta5 <- train_data[train_data$delta == 5,]

#free memory 
rm(train_data)

# building training model function 
buildingTrainingModels <- function(dataset){
  models_delta <- NULL
  
  for (i in 3:402){
    dataset[,i] <- as.factor(dataset[,i])
    rf <- randomForest(dataset[,i]~.,data = dataset[,403:802], mtry = 20, ntree = 20)
    print(class(rf))
    models_delta[[i-2]] <- rf
    print(class(models_delta[[i-2]]))
    print(i)
  }
  
  return(models_delta)
}

#run models for each delta
models_delta1 <- buildingTrainingModels(train_data_delta1)
models_delta2 <- buildingTrainingModels(train_data_delta2)
models_delta3 <- buildingTrainingModels(train_data_delta3)
models_delta4 <- buildingTrainingModels(train_data_delta4)
models_delta5 <- buildingTrainingModels(train_data_delta5)

#read in test data
test_data <- read.csv("Louvain_Clustering/test.csv")
#read submission example
submission <- read.csv("Louvain_Clustering/submission.csv")
#get col names of submission example
submission_colnames <- colnames(submission)

#split testing data by delta                         
test_data_delta1 <- test_data[test_data$delta == 1,]
test_data_delta2 <- test_data[test_data$delta == 2,]
test_data_delta3 <- test_data[test_data$delta == 3,]
test_data_delta4 <- test_data[test_data$delta == 4,]
test_data_delta5 <- test_data[test_data$delta == 5,]

# prediction of each cell function
predictingCells <- function(dataset, model_list){
  submission_data <- dataset$id
  
  for (i in 2:401){
    cellprediction <- predict(model_list[[i-1]], dataset[,3:402], type = "class")
    print(i)
    cellprediction <- as.numeric(levels(cellprediction))[cellprediction]
    submission_data <- cbind(submission_data, cellprediction)
  }
  
  return(submission_data)
}

#predict for delta = 1
submission1 <- predictingCells(test_data_delta1, models_delta1)
colnames(submission1) <- colnames(submission)

#predict for delta = 2
submission2 <- predictingCells(test_data_delta2, models_delta2)
colnames(submission2) <- colnames(submission)

#predict for delta = 3
submission3 <- predictingCells(test_data_delta3, models_delta3)
colnames(submission3) <- colnames(submission)

#predict for delta = 4
submission4 <- predictingCells(test_data_delta4, models_delta4)
colnames(submission4) <- colnames(submission)

#predict for delta = 5
submission5 <- predictingCells(test_data_delta5, models_delta5)
colnames(submission5) <- colnames(submission)

#combine prediction tables
submission <- rbind(submission1, submission2, submission3, submission4, submission5)
#check dimension of final prediction table
dim(submission)

submission <- as.data.frame(submission)
class(submission)

# order id's
submission <- submission[order(submission$id),]

write.csv(submission, file = "submission_data.csv")