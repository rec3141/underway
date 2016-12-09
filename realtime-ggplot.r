library(ggplot2)
library(gdata)
library(maps)
library(scales)
library(plyr)
source("multiplot.R")
options(digits=12, digits.secs = 3, stringsAsFactors=F)

print("setup plotting")
lat.range <- c(sort(com.data$LA)[round(0.01*nrow(com.data))]-2, sort(com.data$LA)[round(0.99*nrow(com.data))]+2)
lon.range <- c(sort(com.data$LO)[round(0.01*nrow(com.data))]-2, sort(com.data$LO)[round(0.99*nrow(com.data))]+2)

#1 plot salinity
col.data <- com.data$SA.2
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(28,33)))
col.row[col.data < 28] <- 1
col.row[col.data > 33] <- nrow(com.data)
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.salinity <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=SA), col="grey") + #biochem lab
	geom_point(data=com.data, aes(x=date_time, y=SA.2), col=mapcolors) + #portside 
	xlab("date time") + ylab("Salinity") + ylim(28,35)

m.salinity <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range , ylim=lat.range) + 
 geom_line(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01, size=2) +
 geom_line(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01, size=2) +
 ggtitle("Salinity")


#2 plot T/S diagram; salinity at port ssw; temperature at sea chest intake
p.ts <- ggplot() +
	geom_point(data=com.data, aes(x=SA, y=ST), col="grey") +
	geom_point(data=com.data, aes(x=SA.2, y=ST), col=rev(topo.colors(nrow(com.data)))) +
	xlim(28,33) + ylim(5,15) + xlab("Salinity") + ylab("Temperature (C)")

m.ts <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=rev(topo.colors(nrow(com.data))), alpha=0.01) +
 ggtitle("Temperature vs Salinity")


#3 flowthu rate
col.data <- com.data$FI.2
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(min(col.data),max(col.data))))
col.row[col.row < 0] <- 1
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.flow <- ggplot() + 
	geom_point(data=com.data, aes(x=date_time, y=FI), col="grey") +
	geom_point(data=com.data, aes(x=date_time, y=FI.2), col=mapcolors) +
	ylab("flow rate")

m.flow <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) + 
 ggtitle("flow rate")

#4 temperature
col.data <- com.data$TT.2
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(8,15)))
col.row[col.data > 15 ] <- nrow(com.data)
col.row[col.data < 8 ] <- 1
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.temperature <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=ST), col="pink") + #sea chest
	geom_point(data=com.data, aes(x=date_time, y=TT), col="grey") + #biochem lab 
	geom_point(data=com.data, aes(x=date_time, y=AT), col="cyan") + #airtemp 
	geom_point(data=com.data, aes(x=date_time, y=TT.2), col=mapcolors) + #portside 
	xlab("date time") + ylab("Temperature (C)") 

m.temperature <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Temperature")

#5 fluorescence
col.data <- com.data$FL.2
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(min(col.data),max(col.data))))
col.row[col.row < 0] <- 1
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.fluor <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=FL), col="grey") + #biochem lab chla
	geom_point(data=com.data, aes(x=date_time, y=FL.4), col="pink") + #portside CDOM
	geom_point(data=com.data, aes(x=date_time, y=FL.3), col="cyan") + #portside phyco
	geom_point(data=com.data, aes(x=date_time, y=FL.2), col=mapcolors) + #portside chla
	xlab("date time") + ylab("Chl a (ug/L)")

m.fluor <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Chlorophyll fluorescence")

#6 oxygen
col.data <- com.data$OX.2
col.row <- round( rescale(col.data,c(1,nrow(com.data)), from=c(6,7.5)))
col.row[col.data < 6] <- 1
col.row[col.data > 7.5] <- nrow(com.data)

mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.oxygen <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=OX), col="grey") + #biochem lab oxygen
	geom_point(data=com.data, aes(x=date_time, y=OX.2), col=mapcolors) + #portside oxygen
	xlab("date time") + ylab("Oxygen") + ylim(6,8)

m.oxygen <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) + 
 ggtitle("Oxygen")


#7 density
col.data <- com.data$SD.2
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(23,max(col.data))))
col.row[col.data < 23] <- 1
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.density <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=SD), col="grey") + #biochem lab sigmat
	geom_point(data=com.data, aes(x=date_time, y=SD.2), col=mapcolors) + #portside sigmat
	xlab("date time") + ylab("Density (sigmaT)") + ylim(22,25)

m.density <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Density (sigma t)")

#8 transmissivity
col.data <- com.data$TR
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(80,max(col.data))))
col.row[col.data < 80] <- 1
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.trans <- ggplot() +
#	geom_point(data=com.data, aes(x=date_time, y=BA), col="grey") + #portside beam attenuation
	geom_point(data=com.data, aes(x=date_time, y=TR), col=mapcolors) + #portside transmission
	xlab("date time") + ylab("% Transmissivity")

m.trans <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Transmissivity")

#9 light
col.data <- com.data$PA
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(min(col.data),max(col.data))))
col.row[col.row < 0] <- 1
mapcolors <- topo.colors(nrow(com.data))[col.row]

p.light <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=SW), col="cyan") + #portside beam attenuation
	geom_point(data=com.data, aes(x=date_time, y=LW), col="pink") + #portside beam attenuation
	geom_point(data=com.data, aes(x=date_time, y=PA), col=mapcolors) + #portside transmission
	xlab("date time") + ylab("PAR")

m.light <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("PAR")

#10 PCO2
col.data <- com.data$PC
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(200,350)))
col.row[col.data < 250] <- 1
col.row[col.data > 350] <- nrow(com.data)
mapcolors <- rev(topo.colors(nrow(com.data)))[col.row]

p.pco2 <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=PC), col=mapcolors) + #portside transmission
	xlab("date time") + ylab("pCO2") +
	ylim(200,350)

m.pco2 <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("pCO2")

### Read from Underway data

uw.in <- read.xls(xls="Log_Samples.xls",sheet="underway")
uw.dat <- uw.in[complete.cases(uw.in),]
uw.dat$epoch.start <- as.numeric(format(as.POSIXlt(paste0(uw.dat$start.date," ",uw.dat$start.time)),"%s"))
uw.dat$epoch.end <- as.numeric(format(as.POSIXlt(paste0(uw.dat$end.date," ",uw.dat$end.time)),"%s"))
uw.dat$y <- seq_along(uw.dat$epoch.start)
uw.rows <- NULL
for(i in 1:nrow(uw.dat)) {
	uw.rows <- rbind(
		uw.rows, 
		cbind(
				seq( uw.dat$epoch.start[i], uw.dat$epoch.end[i]),
				rep( uw.dat$y[i], uw.dat$epoch.end[i]-uw.dat$epoch.start[i] + 1)
			)
		)
}
colnames(uw.rows) <- c("epoch.s","uwclust")
uw.rows <- as.data.frame(uw.rows)
uw.col <- join(com.data[,c("epoch.s","LA","LO","date_time")], uw.rows, match="first")
com.data$uw <- uw.col$uwclust
col.data <- com.data$uw
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(min(col.data,na.rm=T),max(col.data,na.rm=T))))
#col.row[is.na(col.row)] <- NA
mapcolors <- rev(rainbow(nrow(com.data)))[col.row]

#segments much more efficient to plot
#	geom_segment(data=uw.dat, aes(x=as.POSIXlt(uw.dat$epoch.start,origin="1970-01-01"), xend=as.POSIXlt(uw.dat$epoch.end,origin="1970-01-01"), y=y, yend=y), col=rev(rainbow(nrow(uw.dat))), size=3) +

p.uw <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=uw), col=mapcolors, size=3) +
	xlab("date time") + ylab("underway samples")

m.uw <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=uw.col, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Underway samples")


print("calculating clusters")
source("underway-clusters.r")
com.data$clusters <- round(rowMeans(cluster.save,na.rm=T))
com.data$clusters[which(is.nan(com.data$clusters))] <- NA
col.data <- com.data$clusters
col.row <- round(rescale(col.data,c(1,nrow(com.data)), from=c(min(col.data,na.rm=T),max(col.data,na.rm=T))))
mapcolors <- rev(rainbow(nrow(com.data)))[col.row]

lastupdate <- data.frame("label"=as.POSIXlt(Sys.time()), "x"=com.data$date_time[1000], "y"=max(com.data$clusters))

p.clusters <- ggplot() +
	geom_point(data=com.data, aes(x=date_time, y=clusters), col=mapcolors) + 
	xlab("date time") + ylab("clusters") +
	geom_label(data=lastupdate, aes(x=x, y=y, label=label))

m.clusters <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection="orthographic", orientation=c(70, -160, 0), xlim=lon.range ,ylim=lat.range) + 
 geom_point(data=gpstime, aes(x=lon, y=lat), color="grey", alpha=0.01) +
 geom_point(data=com.data, aes(x=LO, y=LA), color=mapcolors, alpha=0.01) +
 ggtitle("Spatial domain clusters")

	
### now plot them all together
print("plotting")
png(file="realtime.png",width=4000,height=800)
multiplot(p.clusters, m.clusters, p.uw, m.uw, p.temperature, m.temperature, p.salinity, m.salinity, p.fluor, m.fluor, p.ts, m.ts, p.density, m.density, p.trans, m.trans, p.pco2, m.pco2, p.oxygen, m.oxygen, p.light, m.light, p.flow, m.flow, cols=12)
dev.off()
print("done plotting")

file.copy("realtime.png","~/Sites/realtime.png",over=T)
