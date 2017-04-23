library(genalg)
train <- read.csv('train.csv')

evaluateBoard <- function(startBoard, endBoard, delta){
  #Takes in a row, returns the difference score after advancing the board delta rounds.
  size <- sqrt(length(startBoard))
  candidateBoard <- startBoard
  
  for (i in 1:delta){
    #advane the board one step
    nextCandidate <- candidateBoard #all cell changes must be applied simultaneously
    for (cell in 1:length(startBoard)){
      
      #Find the neighbor indices of each cell, checking all possible edges
      #more specific hypotheses (corners) get evaluated first
      if(cell == 1){ #top left
        neighbors <- c(cell + 1, cell + size, cell + size + 1)
      } else if(cell == size){ #top right
      neighbors <- c(cell - 1, cell + size - 1, cell + size)
      } else if (cell == (size^2 - size + 1)){ #bottom left
        neighbors <- c(cell - size, cell - size + 1, cell + 1)
      } else if (cell == size^2){ #bottom right
        neighbors <- c(cell - 1, cell - size, cell - size - 1)
      } else if( cell < size){ #top edge
        neighbors <- c(cell - 1, cell + 1, cell + size - 1, cell + size, cell + size + 1)
      } else if ((cell %% size) == 1){ #left edge
        neighbors <- c(cell - size, cell - size + 1, cell + 1, cell + size, cell + size + 1)
      } else if ((cell %% size) == 0){ #right edge
        neighbors <- c(cell - size - 1, cell - size, cell - 1, cell + size - 1, cell + size)
      } else if (cell > size^2 - size){ #bottom edge
        neighbors <- c(cell - size - 1, cell - size, cell - size + 1, cell - 1, cell + 1)
      } else{ #not near any edges
        neighbors <- c(cell - size - 1, cell - size, cell - size + 1, cell - 1, cell + 1, cell + size - 1, cell + size, cell + size + 1)
      }
      
      neighborCount <- sum(neighbors)
      
      #Evaluate the rules of life for single cell and make changes
      if(candidateBoard[cell] == 0){ 
         if(neighborCount == 3){nextCandidate[cell] <- 1} #rule 4
      }else{
        if (neighborCount < 2 | neighborCount > 3){nextCandidate[cell] <- 0} #rules 1 and 3
        #rule 3 is implied
      }
      
      
    }
    candidateBoard <- nextCandidate
  }
  
  #evaluate completed board, return number of cells different from target
  return(length(startBoard) - sum(candidateBoard == endBoard))
}  




acc <- vector(mode = 'numeric', length = nrow(train))

#TODO: bugfix
#TODO: refactor to compare to true start
for (i in nrow(train)){ #gonna parallelize this
  rounds <- train[i,'delta']
  target <- train[i,403:802]
  wrapper <- function(x){ #creates a clean evaluation function with the correct end state and delta
    #todo: double check that it's not passing the entire global environment by accident
    evaluateBoard(x,target,rounds)
  }
  gamod <- rbga.bin(size = 400, popSize = 50, iters = 50, evalFunc = wrapper) #rbga parameters can be increased for more accuracy
  acc[i] <- length(target) - sum(gamod$population[1,] == target)
}

##testing functions
#evaluate a correct one-step board
oneStepStart <- train[22, 3:402]
oneStepFinish <- train[22, 403:802]
testScore <- evaluateBoard(oneStepStart,oneStepFinish,1) #this should be 0

#evaluate a correct five-step board
fiveStepStart <- train[2,3:402]
fiveStepFinish <- train[2,403:802]
testScore <- evaluateBoard(fiveStepStart, fiveStepFinish, 5) #should be 0

#optimize a single board
rounds <- train[50,'delta']
target <- train[50,403:802]
wrapper <- function(x){ #creates a clean evaluation function with the correct end state and delta
  evaluateBoard(x,target,rounds)
}
gamod <- rbga.bin(size = 400, popSize = 50, iters = 50, evalFunc = wrapper)
cat(summary(gamod))
acc <- length(target) - sum(gamod$population[1,] == target) #3 cells off in a test run
