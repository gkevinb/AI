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
  image(t(arr196)[,14:1], col=col,...)
}

load_mnist()

seperateNumber <- function(arr){
  pxMatrix <- matrix(0,28,28)
  for(i in 1:28){
    for(j in 1:28){
      pxMatrix[i,j] <- arr[(i-1)*28+j]
    }
  }
  return(pxMatrix)
}
firstNumber <- matrix(0,28,28)
firstNumber <- seperateNumber(train$x[1,])
firstNumber

avgPooling <- function(inputMatrix){
  pxMatrix <- matrix(0,14,14)
  for(i in 1:14){
    for(j in 1:14){
      pxMatrix[i,j] <- round(mean(inputMatrix[(2*i-1):(2*i),(2*j-1):(2*j)]),0)
    }
  }
  return(pxMatrix)
}

comprFirst <- matrix(0,14,14)
comprFirst <- avgPooling(firstNumber)
comprFirst

# Some testing
show_digit(train$x[932,])
train$n
train$y[932]
show_digit(test$x[932,])
test$n
test$y[932]

#install.packages("kohonen")
require(kohonen)
data_train_matrix <- as.matrix(scale(train$x))
dim(data_train_matrix)

sommap <- som(scale(data_train_matrix), grid = somgrid(5, 4,"hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
plot(sommap, main = "Wine data")
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")
sommap <- som(data_train_matrix, grid = som_grid)
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="circular")

