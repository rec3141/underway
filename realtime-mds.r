library(vegan)

dat.in <- read.table("common_data.csv",header=T,sep="\t")
dat.in <- dat.in[order(dat.in$epoch.s),]
dat.in[duplicated(dat.in$epoch.s),] <- NA
dat.in <- na.omit(dat.in)

par_for_mds <- c("PC","AT","TW","LW","SW","PA","TT","SA","SD","FL","OX","ST","TT.2","SA.2","SD.2","FL.2","FL.3","FL.4","OX.2","TR","BA","epoch.s")

data.sub <- dat.in[,par_for_mds]
data.sub[data.sub < 0] <- NA
data.sub <- na.omit(data.sub)
data.sub.along <- cbind(data.sub$epoch.s,seq_along(data.sub$epoch.s))

loop <- 1
numper <- 500
numloops <- 1000
cluster.save <- matrix(nrow=nrow(dat.in),ncol=numloops)

while(loop < numloops) {
	sample.set <- sample(1:nrow(data.sub),numper)
	data.sam <- data.sub[sample.set,-ncol(data.sub)]

	data.mat <- as.matrix(data.sam)
	#data.heat <- heatmap(t(scale(t(scale(data.mat)))),keep.dendro=T)
	#data.mds <- metaMDS(data.sub)

	hc <- hclust( dist(t(scale(t(scale(data.mat^1.5))))), "cen")
	k <- 18
	data.sam$clusters <- cutree(hc,k=k)
	data.sam$epoch.s <- data.sub[sample.set,"epoch.s"]
	data.sort <- data.sam[order(data.sam$epoch.s),]

	cluster.map <- cbind(unique(data.sort$clusters),1:k)
	cluster.map <- cluster.map[order(cluster.map[,1]),]

	data.sort$recluster <- sapply(data.sort$clusters,function(x) cluster.map[x,2])

	cluster.save[,loop] <- approx(x=data.sort$epoch.s, y=data.sort$recluster, xout=dat.in$epoch.s, "constant")$y

#	plot(dat.in$TT.2 ~ dat.in$epoch.s,ylim=c(0,35),type="l")
#	lines(dat.in$SA.2 ~ dat.in$epoch.s,col="blue")
#	lines(dat.in$FL.2*0.1 ~ dat.in$epoch.s,col="green")
#	lines(cluster.save[,loop] ~ dat.in$epoch.s,col="red")
	loop <- loop + 1
	print(loop)
}

#plot(round(rowMeans(cluster.save,na.rm=T)) ~ dat.in$epoch.s,type="l")

#     memb <- cutree(hc, k = 10)
#     cent <- NULL
#     for(k in 1:10){
#       cent <- rbind(cent, colMeans(data.mat[memb == k, , drop = FALSE]))
#     }
#     hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
#     opar <- par(mfrow = c(1, 2))
#     plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
#     plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
#     par(opar)
