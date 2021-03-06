source("Assignment2/Self Organizing Map.R")

neighbourList <- function(){
  neighbourList <- list()
  #Add each distance measurement to a matrix and then save it to the list.
  #This for loop makes sure that each neuron gets assigned a distance measurement for each neighbour.
  for(i in 1:10){
    for(j in 1:10){
      tempMatrix <- matrix(0,6,2)
      # Common neighbours for both even and odd rows
      tempMatrix[3,1] <- i
      tempMatrix[3,2] <- j+1
      tempMatrix[6,1] <- i
      tempMatrix[6,2] <- j-1
      if(i %% 2 != 0){
        # Odd rows
        tempMatrix[1,1] <- i-1
        tempMatrix[1,2] <- j-1
        tempMatrix[2,1] <- i-1
        tempMatrix[2,2] <- j
        tempMatrix[4,1] <- i+1
        tempMatrix[4,2] <- j
        tempMatrix[5,1] <- i+1
        tempMatrix[5,2] <- j-1
      }else {
        # Even rows
        tempMatrix[1,1] <- i-1
        tempMatrix[1,2] <- j
        tempMatrix[2,1] <- i-1
        tempMatrix[2,2] <- j+1
        tempMatrix[4,1] <- i+1
        tempMatrix[4,2] <- j+1
        tempMatrix[5,1] <- i+1
        tempMatrix[5,2] <- j
      }
      
      #Loops through each entry and removes those below 1 and above 10
      r <- 1
      counter <- 0
      while(r < 7){
        for(c in 1:2){
          if((any(tempMatrix[r,c] < 1)) || (any(tempMatrix[r,c] > 10))){
            tempMatrix <- tempMatrix[-r,]
            counter <- counter + 1
            r <- r -1
          }
        }
        if(counter + r >= 6){
          r <- 6
        }
        r <- r + 1
      }
      #Adds the distance relationship to the neighbourhood list
      neighbourList[[(i-1)*10+j]] <- tempMatrix
    }
  }
  
  return(neighbourList)
}


computeDistance <- function(aNodeWeights, bNodeWeights){
  distanceBetweenAandB <- (1 - cor(aNodeWeights, bNodeWeights))
  return(distanceBetweenAandB)
}

findCoordinateFromIndex <- function(index){
  coordinate <- c(0, 0)
  # find Row coordinate
  coordinate[1] <- ceiling(index/10)
  # find Coloumn coordinate
  if(index %% 10 == 0){
    coordinate[2] <- 10
  } else{
  coordinate[2] <- (index %% 10)
  }
  return(coordinate)
}

findCoordinateForUMatrix <- function(node){
  coordinate <- c(0, 0)

  coordinate[1] <- ((node[1]*2) - 1)
  coordinate[2] <- ((node[2]*2) - 1)

  return(coordinate)
}

UMatrix <- function(){
  UMatrix <- matrix(0, 19, 19)
  # Compute UMatrix with the given distance relationships from the neighbourhood list.

  # Compute distance between nodes horizontally
  i <- 1
  j <- 1
  for(i_UMatrix in seq(19, 1, -2)){
    for(j_UMatrix in seq(2, 18, 2)){
      UMatrix[i_UMatrix,j_UMatrix] <- computeDistance(som[i, j, ], som[i, j+1, ])
      j <- j + 1
    }
    j <- 1
    i <- i + 1
  }
  #Compute distance between nodes vertically
  i <- 1
  j <- 1
  for(i_UMatrix in seq(18, 2, -2)){
    for(j_UMatrix in seq(1, 19, 2)){
      if(i_UMatrix %% 4 == 2){
        UMatrix[i_UMatrix,j_UMatrix] <- computeDistance(som[i, j, ], som[i+1, j, ])
        if(j_UMatrix != 19)
          UMatrix[i_UMatrix,j_UMatrix+1] <- computeDistance(som[i, j+1, ], som[i+1, j, ])
      }
      if(i_UMatrix %% 4 == 0){
        UMatrix[i_UMatrix,j_UMatrix] <- computeDistance(som[i, j, ], som[i+1, j, ])
        if(j_UMatrix != 19)
          UMatrix[i_UMatrix,j_UMatrix+1] <- computeDistance(som[i, j, ], som[i+1, j+1, ])
      }
      j <- j + 1
    }
    i <- i + 1
    j <- 1

  }
  
  # Compute average distance to neighbors for each Node
  nList <- neighbourList()
  i <- 1
  for(i_UMatrix in seq(19, 1, -2)){
    for(j_UMatrix in seq(1, 19, 2)){
      neighbors <- nList[[i]]
      node <- findCoordinateFromIndex(i)
      avgDist <- 0
      for(n in 1:dim(neighbors)[1]){
        avgDist <- avgDist + computeDistance(som[neighbors[n,1],neighbors[n,2], ], som[node[1],node[2], ])
      }
      avgDist <- (avgDist/(dim(neighbors)[1]))
      UMatrix[i_UMatrix, j_UMatrix] <- avgDist
      i <- i + 1
    }
  }

  return(UMatrix)
}

uMatrix <- UMatrix()
