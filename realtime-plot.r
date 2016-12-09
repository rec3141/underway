DEPRECATED
#seq_points <- round(seq(1,nrow(com.data),length.out=4))

plot_date <- format(com.data[,"date_time"], "%a %b %d %H:%M", tz=Sys.timezone())
plot_date <- plot_date[seq_points]

png(file="realtime.png",width=1600,height=960)
par(mfrow = c(2, 4), cex=1.3)

#1
plot(com.data[,"SA"] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(11,16),xlab="time",ylab="temperature")
text(x=com.data[seq_points,"date"], y=11,labels=plot_date, pos=4, srt=90)

#2
plot(com.data[,68] ~ com.data[,72],col=topo.colors(nrow(com.data)),xlim=c(30,32.5),ylim=c(11,16),xlab="salinity",ylab="temperature")

#3
plot(com.data[,108] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(0,600),xlab="time",ylab="fluorescence")
text(x=com.data[seq_points,"date"], y=0,labels=plot_date, pos=4, srt=90)

#4
plot(com.data[,110] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(0,100),xlab="time",ylab="PhyC")
text(x=com.data[seq_points,"date"], y=0,labels=plot_date, pos=4, srt=90)

#5
plot(com.data[,72] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(30,32.5),xlab="time",ylab="salinity")
text(x=com.data[seq_points,"date"], y=30.5,labels=plot_date, pos=4, srt=90)

#6
library(maps)
library(ggplot2)
nz <- map_data("world")
nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
       geom_polygon(fill = "white", colour = "black")
nzmap + coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=c(-180,-145),ylim=c(50,80))
#points(com.data[,158] ~ com.data[,160],col=topo.colors(nrow(com.data)),xlim=c(-180,-150),ylim=c(50,60),xlab="longitude",ylab="latitude")
points(com.data[,158] ~ com.data[,160],col=topo.colors(nrow(com.data)))

#7
plot(com.data[,90] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(4,9),xlab="time",ylab="Oxygen saturation")
text(x=com.data[seq_points,"date"], y=4,labels=plot_date, pos=4, srt=90)

#8
plot(com.data[,100] ~ com.data[,"date"],col=topo.colors(nrow(com.data)),xlim=c(com.data[1,"date"], as.POSIXct(Sys.time(), tz = Sys.timezone())), ylim=c(23,25),xlab="time",ylab="SigmaT")
text(x=com.data[seq_points,"date"], y=20,labels=plot_date, pos=4, srt=90)


dev.off()
system("cp realtime.png ~/Sites/realtime.png")
#Sys.sleep(5)
