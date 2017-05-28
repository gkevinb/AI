# install.packages('nnet')
library(nnet)

# Initialization
set.seed(12345)
# Initialize matrix R and Q, first term is values, 2nd row, 3rd coloum
rowLimit = 4
colLimit = 12
# Greedy factor: Lower greedy, Higher random
epsilon = 0.2;
# Learning Rate
alpha = 0.2;
# Exploration factor: Lower immediate reward, Higher later reward
gamma = 0.7;

# Populate Rewards Matrix
populateR <- function(RMatrix) {
  rewards = matrix(0, rowLimit * colLimit, 4)
  for (i in 1:nrow(RMatrix)) {
    #UP
    if (12 < i)
      rewards[i, 1] <- -1
    #LEFT
    if (i %% 12 != 1)
      rewards[i, 2] <- -1
    #DOWN
    if (i < 37)
      rewards[i, 3] <- -1
    #RIGHT
    if (i %% 12 != 0)
      rewards[i, 4] <- -1
  }
  RMatrix[, 1:4] <- rewards[, 1:4]
  return(RMatrix)
}

populateMappingMatrix <- function(Map) {
  for (i in 1:12) {
    Map[i, 1] = 1
    Map[i, 2] = i
  }
  for (i in 13:24) {
    Map[i, 1] = 2
    Map[i, 2] = (i - 12)
  }
  for (i in 25:36) {
    Map[i, 1] = 3
    Map[i, 2] = (i - 24)
  }
  for (i in 37:48) {
    Map[i, 1] = 4
    Map[i, 2] = (i - 36)
  }
  return(Map)
}

# Initializing the rewards matrix
R = matrix(0, rowLimit * colLimit, 4)
#colnames(R) <- c("UP","LEFT","DOWN","RIGHT")
R <- populateR(R)
R[37,4] = -100
R[26:35, 3] = -100
R[36, 3] = 100
R[48, 1:4] = 100

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
MappingMatrix = matrix(0, rowLimit * colLimit, 2)
MappingMatrix <- populateMappingMatrix(MappingMatrix)
P = matrix(0, rowLimit, colLimit)
VisualQ = matrix(0, rowLimit, colLimit)

# Starting point, initial point
initialState = c(37, 1)
currentStateAction = c(37, 1)
nextStateAction = c(37, 1)
goalState = c(48, 3)

# Functions
#Find all neighboring cells
findNeighbor <- function(cell) {
  Neighbors = matrix(0, 0, 2)
  r = cell[1]
  #UP
  if(12 < r)
    Neighbors <- rbind(Neighbors, c((r-12), 1))
  #LEFT
  if(r %% 12 != 1)
    Neighbors <- rbind(Neighbors, c((r-1), 2))
  #DOWN
  if(r < 37)
    Neighbors <- rbind(Neighbors, c((r+12), 3))
  #RIGHT
  if(r %% 12 != 0)
    Neighbors <- rbind(Neighbors, c((r+1), 4))
  return(Neighbors)
}


# Finds the next cell to move to according to the greedy algorithm in Q matrix
epsilonGreedy <- function(state, epsilon, Matrix) {
  #Random number between 0-1
  neighborhood <- findNeighbor(state)
  random = runif(1)
  if(random < epsilon) {
    n = nrow(neighborhood)
    winner = sample(1:n, 1)
    return(neighborhood[winner,])
  }
  else {
    rewards = c()
    for(i in 1:nrow(neighborhood)){
      direction <- neighborhood[i,2]
      rewards <- c(rewards, Matrix[state[1], direction])
    }
    index = which.is.max(rewards)
    return(neighborhood[index,])
  }
}
# Finds max neighbor of Q matrix
findQMax <- function(stateAction, Matrix) {
  N <- findNeighbor(stateAction)
  values <- c()
  for(i in 1:nrow(N)){
    action <- N[i,2]
    values <- c(values, Matrix[stateAction[1], action])
  }
  return(max(values))
}

mapping <- function(state, Map) {
  cell <- c(0, 0)
  cell[1] <- Map[state[1], 1]
  cell[2] <- Map[state[1], 2]
  return(cell)
}

# Visualize Q values along the best path
visualizeQMatrix <- function(Matrix, initial, goal) {
  visualization = matrix(0, rowLimit, colLimit)
  currentState = c(0, 0)
  currentState = initial
  repeat{
    nextState = epsilonGreedy(currentState, 0, Matrix)
    reward = findQMax(currentState, Matrix)
    mapCell = mapping(nextState, MappingMatrix)
    visualization[mapCell[1], mapCell[2]] = reward
    currentState = nextState
    if(currentState[1] == goal[1]) {
      break
    }
  }
  return(visualization)
}
# Statistical variables of the average value and the standard deviation
rewardInfo <- matrix(0, nrow = 20, ncol = 1000)
pathInfo <- matrix(0, nrow = 20, ncol = 1000)
#Running the algorithm 20 times to create statistics
for(i in 1:20){
  Q = matrix(0, rowLimit * colLimit, 4)
  counter <- i
  
  # The SARSA
  for(j in 1:1000){
    reward <- c()
    P = matrix(0, rowLimit, colLimit)
    # Initilizing Episode
    currentStateAction = initialState
    mapCell = mapping(currentStateAction, MappingMatrix)
    P[mapCell[1], mapCell[2]] = P[mapCell[1], mapCell[2]] + 1
    
    nextStateAction = epsilonGreedy(currentStateAction, epsilon, Q)
    
    # One Episode
    repeat {
      nextnextStateAction = epsilonGreedy(nextStateAction, epsilon, Q)
      
      # Q Learning Formula
      Q[currentStateAction[1],nextStateAction[2]] = Q[currentStateAction[1],nextStateAction[2]] + alpha * 
        (R[currentStateAction[1],nextStateAction[2]] + gamma
         * Q[nextStateAction[1], nextnextStateAction[2]] - Q[currentStateAction[1],nextStateAction[2]])
      
      # gets the reward
      reward = c(reward, Q[currentStateAction[1], nextStateAction[2]])
      
      # If he falls into cliff
      if(nextStateAction[1] > 37 && nextStateAction[1] < 48){
        print("AAAAAAWWWWWWWWWWWWWWWWWWWWWW!!!!")
        currentStateAction = initialState
        nextStateAction = epsilonGreedy(currentStateAction, epsilon, Q)
        
      }
      else{
        currentStateAction = nextStateAction
        nextStateAction = nextnextStateAction
      }
      
      mapCell = mapping(currentStateAction, MappingMatrix)
      P[mapCell[1], mapCell[2]] = P[mapCell[1], mapCell[2]] + 1
      
      if(currentStateAction[1] == goalState[1]) {
        # Adding cumulative reward and path per episode
        rewardInfo[i, j] <- sum(reward)
        pathInfo[i, j] <- sum(P)
        break
      }
    }
  }
  print(counter)
}
VisualQ = visualizeQMatrix(Q, initialState, goalState) 
VisualQ
