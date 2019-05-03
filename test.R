data_table <- read.table(file.choose(), sep="\t", header=TRUE)

data_matrix <- data.frame(data_table$WholeWeight, data_table$ShellWeight)
data_matrix <- as.data.frame(data_matrix)
      
kc <- kmeans(data_matrix, 2)
kc
plot(data_matrix, col=kc$cluster, pch=20 ) 
points(data_matrix, col=data_table[,1], pch=21) 



plot(data_matrix, col=data_table[,1])
plot(iris[,1:2], col=iris[,5])



library("EMCluster")
ret <- starts.via.svd(data_matrix, nclass = 2, method = "em")
summary(ret)
plotem(ret, data_matrix)

emobj <- simple.init(data_matrix, nclass=2) 
ret <- emcluster(data_matrix, emobj, assign.class = TRUE, EMC )
summary(ret)
plotem(ret, data_matrix)

install.packages("gplots")
library("gplots")

data_matrix <- as.matrix(animals)
hclust2 <- function(data_matrix, method="average")
hclust(data_matrix, method=method)
dist2 <- function(data_matrix)
dist(data_matrix, method ="manhattan")
heatmap.2(data_matrix, distfun=dist2, hclustfun =hclust2, srtCol=0, cexCol=1)


