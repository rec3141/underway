library(vegan)

dat.in <- read.table("common_data.csv",header=T,sep="\t")
par_for_mds <- c("PC","AT","TW","LW","SW","PA","TT","SA","SD","FL","OX","ST","TT.2","SA.2","SD.2","FL.2","FL.3","FL.4","OX.2","TR","BA")
#c("TW2","TI2","PA2","AT2","BP2","OX2","TT2.1","SA2.1","SD2.1","FL1","FL1.1","FL1.2","TR2","LA1.1","LO1.1","BT2","GT1.2","ZD1.1")

data.sub <- dat.in[,par_for_mds]
data.sub[data.sub < 0] <- NA
data.sub <- na.omit(data.sub)
data.sub <- sample(1:nrow(data.sub),100)
data.mds <- metaMDS(data.sub)

seq_points <- round(seq(1,nrow(data_for_mds),length.out=20))
plot(data_for_mds[,"FL1.1"] ~ data_for_mds[,"ZD1.1"])
text(x=data_for_mds[seq_points,"ZD1.1"], y=max(data_for_mds[,"FL1.1"])*0.95,labels=data_for_mds[seq_points,"date"],srt=270)
