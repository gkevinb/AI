#source("HandWritingRecognition.R")
SOM <- function(seed, bool){
  set.seed(seed)
  dataRow <- dim(dataMatrix)[1]
  dataCol <- dim(dataMatrix)[2]
  
  ############# SOM Architecture #####################
  
  somRow = 10;
  somCol = 10;
  
  som = array(0, dim = c(somRow, somCol, dataCol))
  
  ############# Parameter Settings ###################
  # Max number of iterations
  N = 1000;
  
  # Initial effective width
  sigmaInitial = 5;
  
  # Time constant for sigma
  t1 <- N/log(sigmaInitial)
  
  # Initialize a 10x10 matrix to store the distance of neurons
  distance <- matrix(0,10,10)
  
  # Initialize a 10x10 matrix to store neighbourhood functions of neurons
  neighbourhoodF <- matrix(0,10,10)
  
  # Initial Learning Rate
  etaInitial <- 0.1
  
  # Time constant for eta(learning rate)
  t2 <- N
  
  ######################## Initialization ############################
  #Assign random weights vectors to third dimension of som matrix
  for(i in 1:somRow){
    for(j in 1:somCol){
      if(bool == TRUE){
        # Random Weights
        som[i, j,] <- runif(dataCol) 
      } else{
        # Static weights
        som[i, j,] <- rep(0.5, times=dataCol)
      }
    }
  }
  
  #Initialize iteration count
  n  <- 1
  
  meanData <- c()
  ######################## Start of Iterative training ###############
  #Start of one iterave loop
  while(n <= N){
    sigma <- sigmaInitial * exp(-n/t1)
    variance <- sigma^2 
    eta = etaInitial * exp(-n/t2)
    # prevent eta from falling below 0.01
    if(eta < 0.01)
      eta <- 0.01
    
    #Pick random number from 1 - 100 which is the row index of a sample in train data
    i <- sample(dataRow, 1)
    
    ######################### Competition Phase ######################
    # Compute 1-correlation distance from the input vector to all neurons
    for(r in 1:10){
      for(c in 1:10){
        # v <- dataMatrix[i,] - som[r,c,]
        # distance[r,c] <- sqrt(v%*%t(t(v)))
        corr <- cor(dataMatrix[i,],som[r,c,])
        if(is.na(corr))
          corr <- 0
        distance[r,c] <- (1-corr)
      }
    }
    # Find the winner neuron that is the closest to the input vector. 
    # winnerRow and winnerCol is the index of the winner neuron on the map.
    winner <- which(distance == min(distance), arr.ind = TRUE)
    winner.row <- winner[1,1]
    winner.col <- winner[1,2]
    ######################### End of competition Phase ###############
    
    ######################## Cooperation Phase #######################
    # Compute the neighboorhood function of every neuron
    for(r in 1:10){
      for(c in 1:10){
        if(r == winner.row && c == winner.col){ #Winner neuron
          neighbourhoodF[r,c] <- 1
        } else{ # not the winner neuron
          d <- (winner.row - r)^2 + (winner.col -c)^2
          neighbourhoodF[r,c] <- exp(-d/(2*variance))
        }
      }
    }
    ######################### End of Cooperation Phase ###############
    
    # save som to see for convergence
    oldWeights <- som
    
    ######################### Adaptation Phase - Only the first sub-phase is considered #######
    for(r in 1:10){
      for(c in 1:10){
        oldWeightVector <- som[r,c,]
        #update weight vector of neuron
        som[r,c,] <- oldWeightVector + eta*neighbourhoodF[r,c]*(dataMatrix[i,] - oldWeightVector)
      }
    }
    ######################### End of Adaptation Phase ################
    n = n + 1
    
    # Check for convergence
    if(oldWeights == som){
      print("It converges at:")
      print(n)
      break
    }
    
  }
  ######################### Draw updated SOM Map ###################
  #plotNumbers(SOMMatrixToList(som))
  #plotNumbers2(SOMMatrixToList(som))
  ######################### End of Draw updated SOM Map ############
  list <- list()
  list[[1]] <- som
  list[2] <- n
  return(list)
}
