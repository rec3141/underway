	ctd.in <- readLines(con=ctd.con, n=-1L, ok=T, warn=T)
	if(length(ctd.in)>0) {
		ctd.mat <- matrix(data=as.numeric(unlist(strsplit(ctd.in,split=" +",perl=T))),ncol=length(ctd.colnames),byrow=T)
		colnames(ctd.mat) <- ctd.colnames
#		ctd.save <- na.omit(ctd.save)
		ctd.save <- rbind(ctd.save, ctd.mat)
		#print(ctd.save)

	cluster.names <- c("conductivity","temperature","depth","oxygen.conc","transmission","fluor","par")
	data.in <- ctd.save[complete.cases(ctd.save[,cluster.names]),cluster.names]

#wait until it starts going down?
#	data.in <- data.in[data.in$depth>4,]
	}

	loop <- 1
	k <- 3
#	numper <- nrow(data.in)
	numper <- 100
	if(numper > nrow(data.in)) {next}
	numloops <- 100
	cluster.save <- matrix(nrow=nrow(data.in),ncol=numloops)

	while(loop < numloops) {
		sample.set <- sample(1:nrow(data.in),numper)
#		sample.set <- 1:nrow(data.in)
		data.mat <- data.in[sample.set,]
		hc <- hclust( dist(t(scale(t(scale(data.mat^1.5))))), "cen")
		data.mat$clusters <- cutree(hc,k=k)
		data.sort <- data.mat[order(data.mat$depth),]

		cluster.map <- cbind(unique(data.sort$clusters),1:k)
		cluster.map <- cluster.map[order(cluster.map[,1]),]

		data.sort$recluster <- sapply(data.sort$clusters,function(x) cluster.map[x,2])

		cluster.save[,loop] <- approx(x=data.sort$depth, y=data.sort$recluster, xout=data.in$depth, "constant")$y
		loop <- loop + 1
#		print(loop)
	}
	par(mfrow=c(1,2))
	plot(ctd.save$depth ~ ctd.save[[plot.par]], ylim=c(max(ctd.save$depth,na.rm=T),0), type="l")

     memb <- cutree(hc, k = k)
     cent <- NULL
     for(j in 1:k){
       cent <- rbind(cent, colMeans(data.mat[memb == j, , drop = FALSE]))
     }

	cent <- as.data.frame(cent)
#	points(cent$depth ~ cent[[plot.par]], col="red")
	text(cent$depth ~ cent[[plot.par]], labels=round(cent$depth),cex=2)

	plot(data.in$depth ~ round(rowMeans(cluster.save,na.rm=T)),type="l", ylim=c(max(ctd.save$depth),0))

	this.depth <- round(data.in$depth[nrow(data.in)] + 2)
	max.depth <- round(max(data.in$depth))
	if(this.depth > max.depth) {going.down=TRUE} else {going.down=FALSE}