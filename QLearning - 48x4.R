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
R = matrix(0, rowLimit * colLimit, 4)
colnames(R) <- c("UP","LEFT","DOWN","RIGHT")
R <- populateR(R)
R[37,4] = -100
R[26:35, 3] = -100
R[36, 3] = 100

#Matrix Numbering
# -------------------------------------
# |1 |2 |3 |4 |5 |6 |7 |8 |9 |10|11|12|
# -------------------------------------
# |13|14|15|16|17|18|19|20|21|22|23|24|
# -------------------------------------
# |25|26|27|28|29|30|31|32|33|34|35|36|
# -------------------------------------
# |S |38|39|40|41|42|43|44|45|46|47|G |

Q = matrix(0, rowLimit * colLimit, 4)
P = matrix(0, rowLimit, colLimit)

# Starting point, initial point
initialCell = c(37, 1)
currentCell = c(37, 1)
nextCell = c(37, 1)

# Functions
# Populate Rewards Matrix
populateR <- function(RMatrix) {
  rewards = matrix(0, rowLimit * colLimit, 4)
  for(i in 1:nrow(RMatrix)){
    #UP
    if(12 < i)
      rewards[i, 1] <- -1
    #LEFT
    if(i %% 12 != 1)
      rewards[i, 2] <- -1
    #DOWN
    if(i < 37)
      rewards[i, 3] <- -1
    #RIGHT
    if(i %% 12 != 0)
      rewards[i, 4] <- -1
  }
  RMatrix[,1:4] <- rewards[,1:4]
  return(RMatrix)
}

#Find all neighboring cells
findNeighbor <- function(cell) {
  Neighbors = matrix(0, 0, 2)
  r = cell[1]
  c = cell[2]
  #UP
  if(12 < r)
    Neighbors <- rbind(Neighbors, c((r-12), c))
  #LEFT
  if(r %% 12 != 1)
    Neighbors <- rbind(Neighbors, c((r-1), c-1))
  #DOWN
  if(r < 37)
    Neighbors <- rbind(Neighbors, c((r+12), c))
  #RIGHT
  if(r %% 12 != 0)
    Neighbors <- rbind(Neighbors, c((r+1), c+1))
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
  N <- findNeighbor(cell)
  print(N[1,1])
  print(N[1,2])
  maxValue <- Matrix[N[1,1],N[1,2]]
  for(i in 2:nrow(N)) {
    currentValue = Matrix[N[i,1],N[i,2]]
    if(maxValue < currentValue)
      maxValue = currentValue
  }
  return(maxValue)
}

# # Function Test
# Neigh <- findNeighbor(c(1,12))
# cell <-epsilonGreedy(Neigh, 0.5, R)
# print(cell)


# The Q-learning
for(j in 1:1000){
  
  # Initilizing Episode
  
  currentCell = initialCell
  # P = matrix(0, rowLimit, colLimit)
  #P[currentCell[1], currentCell[2]] = P[currentCell[1], currentCell[2]] + 1
  
  # One Episode
  
  while(R[currentCell[1], currentCell[2]] != 100) {
    
    currentNeighbors = findNeighbor(currentCell)
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
    
    #P[currentCell[1], currentCell[2]] = P[currentCell[1], currentCell[2]] + 1
    
    
  }
}

print(Q)
#print(P)
sum(P)