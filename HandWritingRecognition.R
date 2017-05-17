# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

#setwd('C:/Users/gkevi/Documents/RStudio/Scripts/AI/')
setwd("C:/Users/Stew-/Desktop/AI/")


load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist/train-images.idx3-ubyte')
  test <<- load_image_file('mnist/t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('mnist/train-labels.idx1-ubyte')
  test$y <<- load_label_file('mnist/t10k-labels.idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

show_compr_digit <- function(arr196, col=gray(12:1/12), ...) {
  image(t(arr196)[,14:1], col=col, xaxt="n", yaxt="n")
}

load_mnist()

#seperate a number from the training set
seperateNumber <- function(arr){
  pxMatrix <- matrix(0,28,28)
  for(i in 1:28){
    for(j in 1:28){
      pxMatrix[i,j] <- arr[(i-1)*28+j]
    }
  }
  return(pxMatrix)
}
#firstNumber <- matrix(0,28,28)
#firstNumber <- seperateNumber(train$x[1,])
# firstNumber

# Average pools the selected number
avgPooling <- function(inputMatrix){
  pxMatrix <- matrix(0,14,14)
  for(i in 1:14){
    for(j in 1:14){
      pxMatrix[i,j] <- round(mean(inputMatrix[(2*i-1):(2*i),(2*j-1):(2*j)]),0)
    }
  }
  return(pxMatrix)
}

#comprFirst <- matrix(0,14,14)
#comprFirst <- avgPooling(firstNumber)
# comprFirst

# Gets a list of 100 numbers from the training set, the list contains 100 matrices of 14x14
getNumbers <- function(numberMatrix){
  multipleArrays <- list()
  for(i in 1:100){
    multipleArrays[[i]] <- avgPooling(seperateNumber(numberMatrix[i,]))
  }
  return(multipleArrays)
}

numbersMatrix <- list()
numbersMatrix <- getNumbers(train$x)

# Returns a number in vector form rather than a matrix form
matrixToVector <- function(matrix){
  v <- c()
  for(i in 1:14){
    v <- c(v, matrix[i,])
  }
  return(v)
}

# Create a matrix that contains each number in a row
createDataMatrix <- function(){
  matrix <- matrix(0,100,196)
  for(i in 1:100){
    matrix[i,] <- matrixToVector(avgPooling(seperateNumber(train$x[i,])))
  }
  return(matrix)
}
dataMatrix <- createDataMatrix()
dataMatrix

#Plots 100 numbers
plotNumbers<- function(somList){
  par(mar=rep(0,4))
  layout(matrix(1:100, ncol=10, byrow=TRUE))
  for(i in 1:100){
    show_compr_digit(somList[[i]])
  }
}

SOMVectorToMatrix <- function(somVector){
  m <- matrix(0,14,14)
  somVector <- floor(somVector)
  for(i in 1:14){
    for(j in 1:14){
      m[i,j] <- somVector[(i-1)*14+j]
    }
  }
  return(m)
}

SOMMatrixToList <- function(som3DMatrix){
  multipleArrays <- list()
  for(i in 1:10){
    for(j in 1:10){
      multipleArrays[[(i-1)*10+j]] <- SOMVectorToMatrix(som3DMatrix[i,j,])
    }
  }
  return(multipleArrays)
}