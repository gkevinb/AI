#SOM example using wines data set
library(kohonen)
data(wines)
set.seed(7)

#create SOM grid
sommap <- som(scale(wines), grid = somgrid(5, 4, "hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
groups<-3
som.hc <- cutree(hclust(dist(sommap$codes)), groups)
plot(sommap, main = "Wine data")

#plot
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(sommap, som.hc)
