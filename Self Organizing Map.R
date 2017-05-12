source("HandWitingRecognition.R")

dataRow <- dim(dataMatrix)[1]
dataCol <- dim(dataMatrix)[2]

############# SOM Architecture #####################

somRow = 10;
somCol = 10;

som = array(0, dim = c(somRow, somCol, dataCol))

############# Parameter Settings ###################
# Max number of iterations
N = 1000;

# Initial effective width
sigmaInitial = 5;

# Time constant for sigma
t1 <- N/log(sigmaInitial)

# Initialize a 10x10 matrix to store the distance of neurons
distance <- matrix(0,10,10)

# Initialize a 10x10 matrix to store neighbourhood functions of neurons
neighbourhoodF <- matrix(0,10,10)

# Initial Learning Rate
etaInitial <- 0.1

# Time constant for eta(learning rate)
t2 <- N

######################## Initialization ############################
#Assin random weights vetors to third dimension of som matrix
for(i in 1:somRow){
  for(j in 1:somCol){
    som[i, j,] <- runif(dataCol)
  }
}

#Initialize iteration count
n  <- 1

######################## Start of Iterative training ###############

