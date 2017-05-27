source("Assignment2/UMatrix.R")
meanDataRandom <- c()
meanDataNonRandom <-c()
convergenceRandom <-c()
convergenceNonRandom <- c()
for(i in seq(10,200, by=10)){
  list <- SOM(i, TRUE)
  som <- list[[1]]
  uMatrix <- UMatrix()
  meanDataRandom <- c(meanDataRandom,mean(uMatrix))
  convergenceRandom <- c(convergenceRandom,list[[2]])
  #NonRandom
  list <- SOM(i, FALSE)
  som <- list[[1]]
  uMatrix <- UMatrix()
  meanDataNonRandom <- c(meanDataNonRandom,mean(uMatrix))
  convergenceNonRandom <- c(convergenceNonRandom,list[[2]])
  print("----------------------------------------------------")
}
# Box plot for the Mean U-Matrix distance
# lmts <- range(meanDataRandom,meanDataNonRandom)
# 
# par(mfrow = c(1, 2))
# boxplot(meanDataRandom,ylim=lmts, main="U-Matrix Mean Distance - 20 Runs - Random Weights")
# boxplot(meanDataNonRandom,ylim=lmts, main="U-Matrix Mean Distance - 20 Runs - NonRandom Weights")

# Box PLot for the convergence time
lmts <- range(convergenceRandom,convergenceNonRandom)

par(mfrow = c(1, 2))
boxplot(convergenceRandom,ylim=lmts, main="Training Time(convergence) - 20 Runs - Random Weights")
boxplot(convergenceNonRandom,ylim=lmts, main="Training Time(convergence) - 20 Runs - NonRandom Weights")

