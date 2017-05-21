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

UMatrix <- function(list){
  UMatrix <- matrix(0, 19, 19)
  # Compute UMatrix with the given distance relationships from the neighbourhood list.
  
}