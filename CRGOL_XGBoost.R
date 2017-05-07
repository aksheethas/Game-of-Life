# Machine Learning - Final Project 
# Conway's Reverse Game of Life 
# XGBoost implementation

#install packages
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)

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
 
    xgb <- xgboost(data = dataset[,403:802], label = dataset[,i], max.depth = 20, 
                   nround = 20, objective = "binary:logistic")
    print(class(xgb))
    models_delta[[i-2]] <- xgb
    print(class(models_delta[[i-2]]))
    print(i)
  }
  
  return(models_delta)
}

#run models for each delta

train_data_delta1[] <- lapply(train_data_delta1, as.numeric)
data_matrix <- as.matrix(train_data_delta1)
models_delta1 <- buildingTrainingModels(data_matrix)

train_data_delta2[] <- lapply(train_data_delta2, as.numeric)
data_matrix <- as.matrix(train_data_delta2)
models_delta2 <- buildingTrainingModels(data_matrix)

train_data_delta3[] <- lapply(train_data_delta3, as.numeric)
data_matrix <- as.matrix(train_data_delta3)
models_delta3 <- buildingTrainingModels(data_matrix)

train_data_delta4[] <- lapply(train_data_delta4, as.numeric)
data_matrix <- as.matrix(train_data_delta4)
models_delta4 <- buildingTrainingModels(data_matrix)

train_data_delta5[] <- lapply(train_data_delta5, as.numeric)
data_matrix <- as.matrix(train_data_delta5)
models_delta5 <- buildingTrainingModels(data_matrix)

#read in test data
test_data <- read.csv("test.csv")
#read submission example
submission <- read.csv("submission.csv")
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
  submission_data <- dataset[,1]
  
  for (i in 2:401){
    cellprediction <- predict(model_list[[i-1]], dataset[,3:402])
    print(i)
    cellprediction <- as.numeric(cellprediction > 0.5)
    submission_data <- cbind(submission_data, cellprediction)
  }
  
  return(submission_data)
}

#predict for delta = 1
test_data_delta1[] <- lapply(test_data_delta1, as.numeric)
data_matrix <- as.matrix(test_data_delta1)
submission1 <- predictingCells(data_matrix, models_delta1)
colnames(submission1) <- colnames(submission)

#predict for delta = 2
test_data_delta2[] <- lapply(test_data_delta2, as.numeric)
data_matrix <- as.matrix(test_data_delta2)
submission2 <- predictingCells(data_matrix, models_delta2)
colnames(submission2) <- colnames(submission)

#predict for delta = 3
test_data_delta3[] <- lapply(test_data_delta3, as.numeric)
data_matrix <- as.matrix(test_data_delta3)
submission3 <- predictingCells(data_matrix, models_delta3)
colnames(submission3) <- colnames(submission)

#predict for delta = 4
test_data_delta4[] <- lapply(test_data_delta4, as.numeric)
data_matrix <- as.matrix(test_data_delta4)
submission4 <- predictingCells(data_matrix, models_delta4)
colnames(submission4) <- colnames(submission)

#predict for delta = 5
test_data_delta5[] <- lapply(test_data_delta5, as.numeric)
data_matrix <- as.matrix(test_data_delta5)
submission5 <- predictingCells(data_matrix, models_delta5)
colnames(submission5) <- colnames(submission)

#combine prediction tables
submission <- rbind(submission1, submission2, submission3, submission4, submission5)
#check dimension of final prediction table
dim(submission)

submission <- as.data.frame(submission)
class(submission)

# order id's
submission <- submission[order(submission$id),]

write.csv(submission, file = "submission_data_xgboost.csv")
