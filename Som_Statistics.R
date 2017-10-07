source("UMatrix.R")
meanData1 <- c()
meanData2 <-c()
convergence1 <-c()
convergence2 <- c()
meanData3 <- c()
convergence3 <- c()


# For Boxplot fill up
for(i in seq(10,100, by=10)){
  # Option 1
  list <- SOM(i, TRUE, 0.1, 5, 'lin')
  som <- list[[1]]
  uMatrix <- UMatrix()
  meanData1 <- c(meanData1,mean(uMatrix))
  convergence1 <- c(convergence1,list[[2]])
  # Option 2
  list <- SOM(i, FALSE, 0.1, 5, 'lin')
  som <- list[[1]]
  uMatrix <- UMatrix()
  meanData2 <- c(meanData2,mean(uMatrix))
  convergence2 <- c(convergence2,list[[2]])
  # # Option 3
  # list <- SOM(i, TRUE, 0.1, 5, 'inv')
  # som <- list[[1]]
  # uMatrix <- UMatrix()
  # meanData3 <- c(meanData3,mean(uMatrix))
  # convergence3 <- c(convergence3,list[[2]])
  print("----------------------------------------------------")
  print(i/10)
}
# Box plot for the Mean U-Matrix distance
lmts <- range(meanData1,meanData2)

par(mfrow = c(1, 2))
boxplot(meanData1,ylim=lmts,
        xlab = "Random Initial Weights",
        ylab = "U-Matrix Mean Distance")
boxplot(meanData2,ylim=lmts,
        xlab = "Non-Random Initial Weights",
        ylab = "U-Matrix Mean Distance")
# boxplot(meanData3,ylim=lmts,
#         xlab = "Inverse Decay Function",
#         ylab = "U-Matrix Mean Distance")

# Box PLot for the convergence time
lmts <- range(convergence1,convergence2)
dev.off()
par(mfrow = c(1, 2))
boxplot(convergence1, ylim=lmts,
        xlab = "Exponential Decay Function",
        ylab = "Training Time(Convergence)")
boxplot(convergence2, ylim=lmts,
        xlab = "Linear Decay Function",
        ylab = "Training Time(Convergence)")
# boxplot(convergence3, ylim=lmts,
#         xlab = "Inverse Decay Function",
#         ylab = "Training Time(Convergence)")

list <- SOM(56, TRUE, 0.1, 5, 'exp')
som <- list[[1]]
uMatrix <- UMatrix()
plotNumbers(SOMMatrixToList(som))
dev.off()


# For Line plot fill up for Width of Neighbor

meanData <- matrix(0, 8, 10)
convergence <-matrix(0, 8, 10)
for(widthN in 3:10){
  for(j in seq(10,100, by=10)){
    list <- SOM(j, TRUE, 0.1, widthN, 'exp')
    som <- list[[1]]
    uMatrix <- UMatrix()
    meanData[(widthN-2), (j/10)] <- mean(uMatrix)
    convergence[(widthN-2), (j/10)] <- list[[2]]
    print("--------------------------------------")
    print(j/10)
  }
  print("=========================================")
  print(widthN)
}

# Plot line for learning Rate

meanData <- matrix(0, 8, 10)
convergence <-matrix(0, 8, 10)
for(i in 1:8){
  for(j in seq(10,100, by=10)){
    list <- SOM(j, TRUE, (i/10), 5, 'exp')
    som <- list[[1]]
    uMatrix <- UMatrix()
    meanData[i, (j/10)] <- mean(uMatrix)
    convergence[i, (j/10)] <- list[[2]]
    print("--------------------------------------")
    print(j/10)
  }
  print("=========================================")
  print(i)
}


# FOR MEAN DATA
avg <- c()
sdev <- c()
for(i in 1:8){
  avg <- c(avg, mean(meanData[i,]))
  sdev <- c(sdev, sd(meanData[i,]))
}
x <- 1:8
x <- x/10

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19,
     main="Mean Distance with relation to Learning Rate",
     xlab = "Initial Learning Rate",
     ylab = "U-Matrix Mean Distance"
)
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)

# FOR CONVERGENCE
avg <- c()
sdev <- c()
for(i in 1:8){
  avg <- c(avg, mean(convergence[i,]))
  sdev <- c(sdev, sd(convergence[i,]))
}


plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19,
     main="Training Time with relation to Learning Rate",
     xlab = "Initial Learning Rate",
     ylab = "Training Time (Convergence)"
)
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
