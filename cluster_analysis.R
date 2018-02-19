setwd("~/big_data/LAB")
#read features from csv
signals_features = read.csv(file = "features.csv", header = TRUE, sep ="," )

set.seed(123)
#apply kmeans clustering with a iteration of centers from 1 to 15
k_max = 15
wss = sapply(1:k_max, function(k){
  kmeans(signals_features, k, nstart=50,iter.max = 15 )$tot.withinss
})

wss
centers = 1:k_max
#plot how the model varies with changing centers from 1 to 15
plot(centers, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     col=ifelse(centers==4,"red","black"))


fit = kmeans(signals_features, 4)
library(fpc)
plotcluster(signals_features, fit$cluster,pch="o",
            xlab="",
            ylab="")
title(main = "K-means cluster of signals")

fit$centers