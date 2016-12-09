### REALTIME-CLUSTERS.R
### by Eric Collins (recollins@alaska.edu), University of Alaska Fairbanks
### last updated 8 August 2016

### This program classifies real-time data from the Healy's MetACQ underway system into spatio-temporal domains
### There is a separate program, realtime-plotter.r, that plots the data output by this program

print("calculating clusters")

#which parameters to use to determine clusters?
#they can be found in WICOR-parameters file
par_for_mds <- c("PC","AT","TW","LW","SW","PA","TT","SA","FL","OX","ST","TT.2","SA.2","SD.2","FL.2","FL.3","FL.4","OX.2","TR")

cluster.data <- plot.data[,par_for_mds]
cluster.epoch.s <- plot.data$epoch.s

#how many clusters
k <- 32

loop <- 1
numper <- 500
numloops <- 200
cluster.save <- matrix(nrow=nrow(plot.data),ncol=numloops)
#print(".")

while(loop < numloops) {
	sub.samples <- sample(1:nrow(cluster.data),numper)
	cluster.mat <- as.matrix(cluster.data[sub.samples,])
	cluster.mat <- na.omit(cluster.mat)
#print(loop)

	hc <- hclust( dist(t(scale(t(scale(cluster.mat^1.5))))), "cen")
	cluster.mat <- as.data.frame(cluster.mat)
#print(".")
	
	cluster.mat$clusters <- cutree(hc,k=k)
	cluster.mat$epoch.s <- cluster.epoch.s[sub.samples]
#print(".")

	cluster.sort <- cluster.mat[order(cluster.mat$epoch.s),]

	cluster.map <- cbind(unique(cluster.sort$clusters),1:k)
	cluster.map <- cluster.map[order(cluster.map[,1]),]
#print(".")

	cluster.sort$recluster <- sapply(cluster.sort$clusters,function(x) cluster.map[x,2])

	cluster.save[,loop] <- approx(x=cluster.sort$epoch.s, y=cluster.sort$recluster, xout=plot.data$epoch.s, "constant")$y
#print(".")

	loop <- loop + 1
}

plot.data$clusters <- round(rowMeans(cluster.save,na.rm=T))
plot.data$clusters[which(is.nan(plot.data$clusters))] <- NA

print("done clustering")
