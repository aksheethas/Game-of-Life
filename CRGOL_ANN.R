# Machine Learning - Final Project 
# Conway's Reverse Game of Life 
# Neural Net Implementation 

install.packages("neuralnet")
library(neuralnet)

setwd("~/Desktop/Game-of-Life")
set.seed(35633)

# read in data
train_data <- read.csv("cultural-mapper/train.csv")
str(train_data)

#prepare train data for 5 different models according to delta                         
train_data_delta1 <- train_data[train_data$delta == 1,]
train_data_delta2 <- train_data[train_data$delta == 2,]
train_data_delta3 <- train_data[train_data$delta == 3,]
train_data_delta4 <- train_data[train_data$delta == 4,]
train_data_delta5 <- train_data[train_data$delta == 5,]

#free memory 
rm(train_data)

#create formula 
respVars <- colnames(train_data_delta1[,3:402])
predVars <- colnames(train_data_delta1[,403:802])
formula_predVars <- paste(predVars,collapse = "+")
formula_respVars <- paste(respVars,collapse = "+")

resp_form <- paste(formula_respVars,'~')
form = as.formula(paste(resp_form,formula_predVars, collapse = "+"))


#resp <- paste(respVars[1],'~')
#form = as.formula(paste(resp,formula_predVars, collapse = "+"))
#model_data <- cbind(train_data_delta1[,3:3],train_data_delta1[,403:802])
#colnames(model_data)[1] <- respVars[1]

# building training model function 
buildingTrainingModels <- function(dataset){
  models_delta <- NULL
  
  for (i in 3:402){
    #dataset[,i] <- as.factor(dataset[,i])
    
    resp <- paste(respVars[i-2],'~')
    form = as.formula(paste(resp,formula_predVars, collapse = "+"))
    model_data <- cbind(dataset[,i],dataset[,403:802])
    colnames(model_data)[1] <- respVars[i-2]
    ann <- neuralnet(form,data = model_data, hidden = c(20,5,2), linear.output = FALSE)
    print(class(ann))
    models_delta[[i-2]] <- ann
    print(class(models_delta[[i-2]]))
    print(i)
  }
  
  return(models_delta)
}

buildingTrainingModel <- function(dataset){
    model_data <- dataset[,-1]
    model_data <- model_data[,-1]
    
    ann <- neuralnet(form,data = model_data, hidden = c(20,20), linear.output = FALSE)
    print(class(ann))
    
    return(ann)

}

#run models for each cell
models_delta1 <- buildingTrainingModels(train_data_delta1)
models_delta2 <- buildingTrainingModels(train_data_delta2)
models_delta3 <- buildingTrainingModels(train_data_delta3)
models_delta4 <- buildingTrainingModels(train_data_delta4)
models_delta5 <- buildingTrainingModels(train_data_delta5)

#run models for each delta
model_delta1 <- buildingTrainingModel(train_data_delta1)
model_delta2 <- buildingTrainingModel(train_data_delta2)
model_delta3 <- buildingTrainingModel(train_data_delta3)
model_delta4 <- buildingTrainingModel(train_data_delta4)
model_delta5 <- buildingTrainingModel(train_data_delta5)

#read in test data
test_data <- read.csv("cultural-mapper/test.csv")
#read submission example
submission <- read.csv("cultural-mapper/submission.csv")
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
    cellprediction <- compute(model_list[[i-1]], dataset[,3:402])
    print(i)
    cellprediction <- as.numeric(cellprediction$net.result > 0.5)
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
