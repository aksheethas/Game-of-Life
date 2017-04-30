library(genalg)
library(foreach)
library(doParallel)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

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



sampleVec <- sample(1:nrow(train),10)

acc <- vector(mode = 'numeric', length = length(sampleVec))

#parallelized assessment
registerDoParallel(cores = 8)
t1 <- Sys.time()
acc <- foreach(i = 1:length(sampleVec), .combine  = rbind, 
                         .export   = c('train'),
                         .packages = c('genalg'))  %dopar% {
                           rounds <- train[i,'delta']
                           end <- train[i,403:802]
                           start <- train[i,3:402]
                           wrapper <- function(x){ #creates a clean evaluation function with the correct end state and delta
                             evaluateBoard(x,end,rounds)
                           }
                           gamod <- rbga.bin(size = 400, popSize = 3000, iters = 200, evalFunc = wrapper)
                           return(length(end) - sum(gamod$population[1,] == start))
                         }
t2 <- Sys.time()
# > t1
# [1] "2017-04-30 04:28:46 EDT"
# > t2
# [1] "2017-04-30 09:30:04 EDT"
# > acc
# [,1]
# result.1    41
# result.2    60
# result.3   110
# result.4    93
# result.5   106
# result.6   137
# result.7   100
# result.8   118
# result.9   123
# result.10  113


#single-board assessment with printouts
samplePoint <- sample(1:nrow(train),1)
rounds <- train[samplePoint,'delta']
end <- train[samplePoint,403:802]
start <- train[samplePoint,3:402]
wrapper <- function(x){ #creates a clean evaluation function with the correct end state and delta
  evaluateBoard(x,end,rounds)
}
t1 <- Sys.time()
gamod <- rbga.bin(size = 400, popSize = 4000, iters = 300, evalFunc = wrapper, verbose = TRUE)
acc <- length(end) - sum(gamod$population[1,] == start)
t2 <- Sys.time()
#t1: "2017-04-27 21:01:28 EDT"
#t2: "2017-04-28 00:25:58 EDT"
#acc: 99

#https://www.researchgate.net/post/What_is_the_optimal_recommended_population_size_for_differential_evolution2
#Even among academics, there's significant disagreement on 'good' GA parameters.
#pop size of 250-500 suggested for ~1000 dimensional data; we go on the high end
#because we want a broad search space.
gamod <- rbga.bin(size = 400, popSize = 500, iters = 75, evalFunc = wrapper)
return(length(end) - sum(gamod$population[1,] == start))


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
