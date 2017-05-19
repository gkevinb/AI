
createUMatrix <- function(){
  UMatrix <- matrix(0,10,10)
  for(i in 1:10){
    for(j in 1:10){
      if((i == 1 && j == 1) || (i == 10 && j == 1)){
        # the neuron has 2 neighbours - the first and last in the 1st column
        
      }
      if(j == 10){
        # the neuron has 3 neighbours - all of the neurons in the 10th column
        
      }
      if((i == 1 || i == 10) && (j > 1 && j < 10)){
        # the neuron has 4 neighbours - first and last row except the first and last index of the row
        
      }
      if((i > 1 && i < 10) && (j == 1)){
        # the neuron has 5 neighbours - NOT! the first and last in the 1st column
        
      }
      if((i > 1 && j > 1) && (i < 10 && j < 10)){
        # the neuron has 6 neighbours - every cell that doesn't touch the border of the matrix
        
      }
    }
  }
}