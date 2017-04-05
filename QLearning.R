install.packages('nnet')
library(nnet)


# Initialize matrix R and Q, first term is values, 2nd row, 3rd coloum
rowLimit = 4
colLimit = 12

R = matrix(-1, rowLimit, colLimit)
Q = matrix(0, rowLimit, colLimit)
R[4, 2:11] = -100
R[4, 12] = 100

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
  if(1 < colLim)
    Neighbors <- rbind(Neighbors, c((r), c+1))
  return(Neighbors)
}

epsilonGreedy <- function(neighborhood, epsilon, rewardMatrix) {
  random = runif(1)
  if(random < epsilon) {
    winner = sample(1:4, 1)
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

Neigh <- findNeighbor(c(3,2), rowLimit, colLimit)
cell <-epsilonGreedy(Neigh, 0.5, R)
