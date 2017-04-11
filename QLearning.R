# install.packages('nnet')
library(nnet)

# Initialization

# Initialize matrix R and Q, first term is values, 2nd row, 3rd coloum
rowLimit = 4
colLimit = 12
# Greedy factor: Lower greedy, Higher random
epsilon = 0.01;
# Learning Rate
alpha = 0.9;
# Exploration factor: Lower immediate reward, Higher later reward
gamma = 0.8;

# Initializing the rewards matrix
R = matrix(-1, rowLimit, colLimit)
R[4, 2:11] = -100
R[4, 12] = 100

Q = matrix(0, rowLimit, colLimit)
P = matrix(0, rowLimit, colLimit)

# Starting point, initial point
initialCell = c(4, 1)
currentCell = c(4, 1)
nextCell = c(4, 1)


# Functions
#Find all neighboring cells
findNeighbor <- function(cell, rowLim, colLim) {
  Neighbors = matrix(0, 0, 2)
  r = cell[1]
  c = cell[2]
  #UP
  if(1 < r)
    Neighbors <- rbind(Neighbors, c((r-1), c))
  #LEFT
  if(1 < c)
    Neighbors <- rbind(Neighbors, c((r), c-1))
  #DOWN
  if(r < rowLim)
    Neighbors <- rbind(Neighbors, c((r+1), c))
  #RIGHT
  if(c < colLim)
    Neighbors <- rbind(Neighbors, c((r), c+1))
  return(Neighbors)
}
# Finds the next cell to move to accodring to the greedy algorithm
epsilonGreedy <- function(neighborhood, epsilon, rewardMatrix) {
  #Random number between 0-1
  random = runif(1)
  if(random < epsilon) {
    n = nrow(neighborhood)
    winner = sample(1:n, 1)
    return(neighborhood[winner,])
  }
  else {
    rewards = c()
    for(i in 1:nrow(neighborhood)){
      rewards <- c(rewards, rewardMatrix[neighborhood[i,1],neighborhood[i,2]])
    }
    index = which.is.max(rewards)
    return(neighborhood[index,])
  }
}
# Finds max neighbor of Q matrix
findQMax <- function(cell, Matrix) {
  # dim function returns two values, the number of rows and number of columns
  N <- findNeighbor(cell, dim(Matrix)[1], dim(Matrix)[2])
  maxValue <- Matrix[N[1,1],N[1,2]]
  for(i in 2:nrow(N)) {
    currentValue = Matrix[N[i,1],N[i,2]]
    if(maxValue < currentValue)
      maxValue = currentValue
  }
  return(maxValue)
}

# Function Test
Neigh <- findNeighbor(c(1,12), rowLimit, colLimit)
cell <-epsilonGreedy(Neigh, 0.5, R)
print(cell)


# The Q-learning
for(j in 1:1000){
  
  # Initilizing Episode
  
  currentCell = initialCell
  # P = matrix(0, rowLimit, colLimit)
  P[currentCell[1], currentCell[2]] = P[currentCell[1], currentCell[2]] + 1
  
  # One Episode
  
  while(R[currentCell[1], currentCell[2]] != 100) {
    
    currentNeighbors = findNeighbor(currentCell, rowLimit, colLimit)
    nextCell = epsilonGreedy(currentNeighbors, epsilon, Q)
    
    # Q Learning Formula
    Q[currentCell[1],currentCell[2]] = Q[currentCell[1],currentCell[2]] + alpha * 
      (R[currentCell[1],currentCell[2]] + gamma * findQMax(nextCell, Q) - Q[currentCell[1],currentCell[2]])
    
    # If he falls into cliff
    if(R[currentCell[1], currentCell[2]] == -100){
      print("AAAAAAWWWWWWWWWWWWWWWWWWWWWW!!!!")
      currentCell = initialCell
    }
    else{
      currentCell = nextCell
    }
    
    P[currentCell[1], currentCell[2]] = P[currentCell[1], currentCell[2]] + 1
    
    
  }
}

print(Q)
print(P)
sum(P)



