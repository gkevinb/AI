# FOr Q-learning comment out the source function below
#source("assignment3/QLearning - 48x4.R")

# FOr SARSA comment out the source function below
#source("assignment3/Sarsa.R")

# Statistics processing
getAverage <- function(list) {
  avg <- matrix(0,1,1000)
  for (i in 1:1000) {
    avg[1,i] <- mean(list[, i])
  }
  return(avg)
}
avgReward <- getAverage(rewardInfo)
avgPath <- getAverage(pathInfo)

getSD <- function(list){
  sd <- matrix(0,1,1000)
  for (i in 1:1000) {
    sd[1,i] <- sd(list[, i])
  }
  return(sd)
}
sdReward <- getSD(rewardInfo)
sdPath <- getSD(pathInfo)

#Plotting of values

#avgReward <- round(avgReward, 5)
#Trim rewardList for visual purposes
#Trim rewardList for visual purposes
# trimRewardList <- function(rewards){
#   for(i in 1:length(rewards)){
#     if(rewards[i] == rewards[i+1]){
#       length(rewards) <- i
#       return(rewards)
#     }else{
#       return(rewards)
#     }
#   }
# }
#Make sure that each list is the same length
# avgReward <- trimRewardList(avgReward)
length(avgReward) <- length(1:100)
length(sdReward) <- length(avgReward)
length(avgPath) <- length(avgReward)
length(sdPath) <- length(avgReward)

#Plot cumulative reward per episode
plotRewards <- function(avg,sdev){
  plot(1:length(avg), avg, ylim = range(c(avg-sdev, avg+sdev)),
       pch=20, xlab = "Episodes", ylab = "Reward +/- SD", type="o",
       #main = "SARSA - Rewards Per Episode with SD Error Bars")
       main = "Q-Learning - Rewards Per Episode with SD Error Bars")
  axis(side=1, at=seq(0,100, by=5))
  axis(side=2, at=seq(-1500, 500, by=100))
  arrows(1:length(avg), avg-sdev, 1:length(avg), avg+sdev, length =0.02, angle =90, code=3, col =360)
}
plotRewards(avgReward,sdReward)

##Plot cumulative reward per episode, without the first measurement
plotRewardsLesser <- function(avg,sdev){
  plot(1:length(avg), avg, ylim = range(c(avg-sdev, avg+sdev)),
       pch=20, xlab = "Episodes", ylab = "Reward +/- SD", type="o",
       #main = "SARSA - Rewards Per Episode with SD Error Bars")
       main = "Q-Learning - Rewards Per Episode with SD Error Bars")
  axis(side=1, at=seq(0,100, by=5))
  axis(side=2, at=seq(-1500, 500, by=50))
  arrows(1:length(avg), avg-sdev, 1:length(avg), avg+sdev, length =0.02, angle =90, code=3, col =360)
}
avgReward <- avgReward[2:100]
sdReward <- sdReward[2:100]
plotRewardsLesser(avgReward,sdReward)

#Plot cumulative Path length per episode
plotPathLength <- function(avg,sdev){
  plot(1:length(avg), avg, ylim = range(c(avg-sdev, avg+sdev)),
       pch=20, xlab = "Episodes", ylab = "Path Length +/- SD", type="o",
       #main = "SARSA - Path Length Per Episode with SD Error Bars")
       main = "Q-Learning - Path Length Per Episode with SD Error Bars")
  axis(side=1, at=seq(0,100, by=5))
  axis(side=2, at=seq(0, 800, by=50))
  arrows(1:length(avg), avg-sdev, 1:length(avg), avg+sdev, length =0.02, angle =90, code=3, col =360)
}
plotPathLength(avgPath, sdPath)

#Plot cumulative Path length per episode, without the first measurement
plotPathLengthLesser <- function(avg,sdev){
  plot(1:length(avg), avg, ylim = range(c(avg-sdev, avg+sdev)),
       pch=20, xlab = "Episodes", ylab = "Path Length +/- SD", type="o",
       #main = "SARSA - Path Length Per Episode with SD Error Bars")
       main = "Q-Learning - Path Length Per Episode with SD Error Bars")
  axis(side=1, at=seq(0,100, by=5))
  axis(side=2, at=seq(0, 600, by=20))
  arrows(1:length(avg), avg-sdev, 1:length(avg), avg+sdev, length =0.02, angle =90, code=3, col =360)
}
avgPath <- avgPath[2:100]
sdPath <- sdPath[2:100]
plotPathLengthLesser(avgPath, sdPath)
