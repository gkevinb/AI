library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

#Function to create the polygon for each hexagon
Hexagon <- function (x, y, unitcell = 1, col = col) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
            x + unitcell/2), c(y + unitcell * 0.125, 
                               y + unitcell * 0.875, 
                               y + unitcell * 1.125, 
                               y + unitcell * 0.875, 
                               y + unitcell * 0.125, 
                               y - unitcell * 0.125), 
          col = col, border=NA)
}#function

#Start with a matrix that would be the numerical representation of you heatmap
#called Heatmap_Matrix
#This matrix has the same number of rows as the SOM map 
#and the same number of columns as the SOM map
#and each value in the Heatmap represents the value for one hexagon
#Here [1,1] will become the lower left node (1st row, 1st column), 
#[1,2] will become the node to the right
#[2,1] will be the first node to the left in the second row
#So visually you work your way from bottom left to top right
x <- as.vector(dataMatrix)

#Number of rows and columns of your SOM
SOM_Rows <- dim(dataMatrix)[1]
SOM_Columns <- dim(dataMatrix)[2]

#To make room for the legend
par(mar = c(0.4, 2, 2, 7))

#Initiate the plot window but do show any axes or points on the plot
plot(0, 0, type = "n", axes = FALSE, xlim=c(0, SOM_Columns), 
     ylim=c(0, SOM_Rows), xlab="", ylab= "", asp=1)

#Create the color palette 
#I use designer.colors to interpolate 50 colors between 
#the maxmimum number of allowed values in Brewer 
#Just replace this with any other color palette you like
ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))

#Make a vector with length(ColRamp) number of bins between the minimum and 
#maximum value of x. 
#Next match each point from x with one of the colors in ColorRamp
ColorCode <- rep("#FFFFFF", length(x)) #default is all white
Bins <- seq(min(x, na.rm=T), max(x, na.rm=T), length=length(ColRamp))
for (i in 1:length(x))
  if (!is.na(x[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-x[i]))] 

#Actual plotting of hexagonal polygons on map
offset <- 0.5 #offset for the hexagons when moving up a row
for (row in 1:SOM_Rows) {
  for (column in 0:(SOM_Columns - 1)) 
    Hexagon(column + offset, row - 1, col = ColorCode[row + SOM_Rows * column])
  offset <- ifelse(offset, 0, 0.5)
}

#Add legend to the right if you want to
image.plot(legend.only=TRUE, col=ColRamp, 
           zlim=c(min(x, na.rm=T), max(x, na.rm=T)))
