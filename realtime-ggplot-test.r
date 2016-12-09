library(data.table)
library(ggplot2)
library(gdata)
library(maps)
library(scales)
library(plyr)
library(vegan)
options(digits=12, digits.secs = 3, stringsAsFactors=F)

rm(list=ls())


start.time <- proc.time()
elapsed.time <- function() (proc.time() - start.time)[3]

while ( elapsed.time() < 3600 ) {

print("setup plotting")

while(1) {
try({
load("com.data.r")
})
if(length(com.data)>0) {break}
Sys.sleep(30)
}

#clean up messy UDP lines missing lineendings
com.data[com.data$ZD_date <0 ,"ZD_date"] <- NA
com.data <- na.omit(com.data)
	
print(tail(com.data$local_time))

map.projection = "orthographic"
map.orientation = c(70, -160, 0)
range.lat <- c(55,83)
range.lon <- c(-180, -150)


print("#0 Waypoints")
#0 Waypoints

waypoint.in <- read.xls(xls="1601_Station_Plan.xls",sheet="Plan DD")
waypoint.data <- waypoint.in[,1:4]
waypoint.data <- waypoint.data[complete.cases(waypoint.data),]
l.waypoint.point <- geom_point(data=waypoint.data, aes(x=longitude, y=latitude), color="black", fill="black")
l.waypoint.label <-	geom_label(data=waypoint.data, aes(x=longitude, y=latitude, label=X), size=2, fill="white", alpha="0.2", nudge_x=0.1*(abs(range.lat[1]-range.lat[2])))

print("#-1 RV ARAON waypoints")
araon.in <- read.xls(xls="borderlands-previous-expeditions.xls",sheet="araon")
araon.data <- araon.in[,c(4,5,9)]
araon.data <- araon.data[complete.cases(araon.data),]
l.araon.point <- geom_point(data=araon.data, aes(x=longitude, y=latitude), color="red", fill="red")
l.araon.label <-	geom_label(data=araon.data, aes(x=longitude, y=latitude, label=site), size=2, col="red", fill="white", alpha="0.2", nudge_x=0.1*(abs(range.lat[1]-range.lat[2])))


######## PLOTTING FUNCTION, CALL WITH FILE NAME

prep.plot <- function(plot.data, plot.file) {

print(tail(plot.data))

### SETUP RANGES


range.salinity = c(20,33)
range.temperature = c(-2,15)
range.flow = c(0,4)
range.fluor = c(min(plot.data$FL.2/50,na.rm=T),max(plot.data$FL.2/50,na.rm=T))
range.oxygen = c(6,15)
range.pco2 = c(0,500)
range.density = c(18,max(plot.data$SD.2,na.rm=T))
range.light = c(min(plot.data$PA,na.rm=T),max(plot.data$PA,na.rm=T))
range.transmissivity = c(0,100)
range.depth = c(0,3500)

######## prep data

print("#1 salinity")
#1 plot salinity
col.data <- plot.data$SA.2
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.salinity))
col.row[col.data < range.salinity[1]] <- 1
col.row[col.data > range.salinity[2]] <- nrow(plot.data)
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.salinity <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=SA), col="grey") + #biochem lab
	geom_point(data=plot.data, aes(x=local_time, y=SA.2), col=mapcolors) + #portside 
	xlab("UTC time") + ylab("Salinity") + ylim(range.salinity)

m.salinity <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon , ylim=range.lat) + 
# geom_point(data=plot.data, aes(x=LO, y=LA), color="grey", size=2) +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors, size=2) +
 l.waypoint.point + l.waypoint.label 
 ggtitle("Salinity")


print("#2 T/S diagram")
#2 plot T/S diagram; salinity at port ssw; temperature at sea chest intake
p.ts <- ggplot() +
	geom_point(data=plot.data, aes(x=SA, y=ST), col="grey") +
	geom_point(data=plot.data, aes(x=SA.2, y=ST), col=rev(topo.colors(nrow(plot.data)))) +
	xlim(range.salinity) + ylim(range.temperature) + xlab("Salinity") + ylab("Temperature (C)")

m.ts <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
# geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=rev(topo.colors(nrow(plot.data)))) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Temperature vs Salinity")


print("#3 flowthu rate")
#3 flowthu rate
col.data <- plot.data$FI.2
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.flow))
col.row[col.row < range.flow[1]] <- 1
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.flow <- ggplot() + 
	geom_point(data=plot.data, aes(x=local_time, y=FI), col="grey") +
	geom_point(data=plot.data, aes(x=local_time, y=FI.2), col=mapcolors) +
	ylab("flow rate") +ylim(range.flow)

m.flow <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
# geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) + 
 l.waypoint.point + l.waypoint.label +
 ggtitle("flow rate")

print("#4 temperature")
#4 temperature
col.data <- plot.data$TT.2
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.temperature))
col.row[col.data < range.temperature[1] ] <- 1
col.row[col.data > range.temperature[2] ] <- nrow(plot.data)
mapcolors <- topo.colors(nrow(plot.data))[col.row]

p.temperature <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=ST), col="pink") + #sea chest
	geom_point(data=plot.data, aes(x=local_time, y=TT), col="grey") + #biochem lab 
	geom_point(data=plot.data, aes(x=local_time, y=AT), col="cyan") + #airtemp 
	geom_point(data=plot.data, aes(x=local_time, y=TT.2), col=mapcolors) + #portside 
	xlab("UTC time") + ylab("Temperature (C)") + ylim(range.temperature)

m.temperature <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
# geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Temperature")

print("#5 fluorescence")
#5 fluorescence
col.data <- plot.data$FL.2/50 #/50 for rough correction from RFU to mg/m3
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.fluor))
col.row[col.row < 0] <- 1
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.fluor <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=FL), col="grey") + #portside chla
	geom_point(data=plot.data, aes(x=local_time, y=FL.4/5), col="pink") + #portside CDOM
	geom_point(data=plot.data, aes(x=local_time, y=FL.3/20), col="cyan") + #portside phyco
	geom_point(data=plot.data, aes(x=local_time, y=FL.2/50), col=mapcolors) + #biochem lab chla
	xlab("UTC time") + ylab("Chl a (mg/m^3)") + ylim(range.fluor)

m.fluor <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
# geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Chlorophyll fluorescence")

print("#6 oxygen")
#6 oxygen
col.data <- plot.data$OX.2
col.row <- round( rescale(col.data,c(1,nrow(plot.data)), from=range.oxygen))
col.row[col.data < range.oxygen[1]] <- 1
col.row[col.data > range.oxygen[2]] <- nrow(plot.data)

mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.oxygen <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=OX), col="grey") + #biochem lab oxygen
	geom_point(data=plot.data, aes(x=local_time, y=OX.2), col=mapcolors) + #portside oxygen
	xlab("UTC time") + ylab("Oxygen") + ylim(range.oxygen)

m.oxygen <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 #geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) + 
 l.waypoint.point + l.waypoint.label +
 ggtitle("Oxygen")


print("#7 density")
#7 density
col.data <- plot.data$SD.2
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.density ) )
col.row[col.data < range.density[1]] <- 1
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.density <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=SD), col="grey") + #biochem lab sigmat
	geom_point(data=plot.data, aes(x=local_time, y=SD.2), col=mapcolors) + #portside sigmat
	xlab("UTC time") + ylab("Density (sigmaT)") + ylim(range.density)

m.density <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 #geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Density (sigma t)")

print("#8 transmissivity")
#8 transmissivity
col.data <- plot.data$TR
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.transmissivity ) )
col.row[col.data < range.transmissivity[1]] <- 1
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.trans <- ggplot() +
#	geom_point(data=plot.data, aes(x=local_time, y=BA), col="grey") + #portside beam attenuation
	geom_point(data=plot.data, aes(x=local_time, y=TR), col=mapcolors) + #portside transmission
	xlab("UTC time") + ylab("% Transmissivity") + ylim(range.transmissivity)

m.trans <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 #geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Transmissivity")

print("#9 light")
#9 light
col.data <- plot.data$PA
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.light ) )
col.row[col.row < 0] <- 1
mapcolors <- topo.colors(nrow(plot.data))[col.row]

p.light <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=SW), col="cyan") + #portside beam attenuation
	geom_point(data=plot.data, aes(x=local_time, y=LW), col="pink") + #portside beam attenuation
	geom_point(data=plot.data, aes(x=local_time, y=PA), col=mapcolors) + #portside transmission
	xlab("UTC time") + ylab("PAR")

m.light <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 #geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("PAR")

print("#10 PCO2")
#10 PCO2
col.data <- plot.data$PC
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.pco2 ) )
col.row[col.data < range.pco2[1] ] <- 1
col.row[col.data > range.pco2[2] ] <- nrow(plot.data)
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.pco2 <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=PC), col=mapcolors) + #portside transmission
	xlab("UTC time") + ylab("pCO2") +
	ylim(range.pco2)

m.pco2 <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 #geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("pCO2")

print("#11 depth")
#11 depth;
col.data <- plot.data$BT
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.depth ) )
col.row[col.data < range.depth[1]] <- 1
col.row[col.data > range.depth[2]] <- nrow(plot.data)
mapcolors <- rev(topo.colors(nrow(plot.data)))[col.row]

p.depth <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=LF), col="red") + # low freq 3.5 khz depth
	geom_point(data=plot.data, aes(x=local_time, y=HF), col="cyan") + # high freq 12khz depth
	geom_point(data=plot.data, aes(x=local_time, y=MB), col="grey") + # multibeam depth
	geom_point(data=plot.data, aes(x=local_time, y=BT), col=mapcolors) + # bottom depth
	xlab("UTC time") + ylab("Depth") + ylim(range.depth)

m.depth <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon , ylim=range.lat) + 
 geom_point(data=plot.data, aes(x=LO, y=LA), color="grey", size=2) +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors, size=2) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Depth")


print("#12 underway samples")
#12 underway samples
uw.in <- NULL
if(file.exists("/Volumes/Public/HLY1601/Docs/Microbes/Log_Samples.xls")) {
	try({
		uw.in <- read.xls(xls="/Volumes/Public/HLY1601/Docs/Microbes/Log_Samples.xls",sheet="uw.dna")
	})
} else {
	try({
	uw.in <- read.xls(xls="Log_Samples.xls",sheet="uw.dna")
		})
}
if(length(uw.in)>0) {
	uw.dat <- uw.in[complete.cases(uw.in),1:5]
	uw.dat$epoch.start <- as.numeric(format(as.POSIXlt(paste0(uw.dat$start.date," ",uw.dat$start.time),tz=""),"%s"))
	uw.dat$epoch.end <- as.numeric(format(as.POSIXlt(paste0(uw.dat$end.date," ",uw.dat$end.time),tz=""),"%s"))
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
	colnames(uw.rows) <- c("epoch.s","uw")
	uw.rows <- as.data.frame(uw.rows)
	uw.rows <- uw.rows[order(uw.rows$epoch.s),]
	#join with whole dataset

	uw.col <- join(plot.data[,c("epoch.s","LA","LO","local_time")],uw.rows, match="first")
	plot.data$uw <- uw.col$uw
	col.data <- plot.data$uw

	plot.data$uw <- plot.data$uw[sample.set]
	plot.data$uw[is.na(plot.data$uw)] <- -1

	range.uw = c(min(plot.data$uw,na.rm=T),max(plot.data$uw,na.rm=T))
	col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.uw ) )

	#now rescale to sample.set
	mapcolors <- rev(rainbow(nrow(plot.data)))[col.row[sample.set]]

	#segments much more efficient to plot
	#	geom_segment(data=uw.dat, aes(x=as.POSIXlt(uw.dat$epoch.start,origin="1970-01-01"), xend=as.POSIXlt(uw.dat$epoch.end,origin="1970-01-01"), y=y, yend=y), col=rev(rainbow(nrow(uw.dat))), size=3) +

	p.uw <- ggplot() +
		geom_point(data=plot.data, aes(x=local_time, y=uw), col=mapcolors, size=3) +
		xlab("UTC time") + ylab("underway samples") + ylim(range.uw)

	m.uw <- ggplot() + 
	 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
	 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
	 geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
	 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
	 l.waypoint.point + l.waypoint.label +
	 ggtitle("Underway samples")
} else {
	p.uw <- ggplot()
	m.uw <- ggplot() +  
	geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 	coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 	l.waypoint.point + l.waypoint.label +
	ggtitle("Underway samples") 	
}

print("#13 spatial domains")
#13 spatial domains

#takes plot.data as input, outputs plot.data$clusters
source("underway-clusters.r",local=T)
range.clusters = c(min(plot.data$clusters,na.rm=T),max(plot.data$clusters,na.rm=T))

col.data <- plot.data$clusters
col.row <- round(rescale(col.data,c(1,nrow(plot.data)), from=range.clusters ) )
mapcolors <- rev(rainbow(nrow(plot.data)))[col.row]

lastupdate <- data.frame("label"=format(as.POSIXlt(Sys.time()),"Last updated on %a %B %d at %H:%M %Z"), "x"=plot.data$local_time[round(nrow(plot.data)/2)], "y"=max(plot.data$clusters))

p.clusters <- ggplot() +
	geom_point(data=plot.data, aes(x=local_time, y=clusters), col=mapcolors, size=3) + 
	geom_label(data=lastupdate, aes(x=x, y=y, label=label),hjust=0.5,vjust=1) +
	xlab("UTC time") + ylab("clusters") + ylab(range.clusters)

m.clusters <- ggplot() + 
 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
 geom_point(data=plot.data, aes(x=LO, y=LA), color="grey") +
 geom_point(data=plot.data, aes(x=LO, y=LA), color=mapcolors) +
 l.waypoint.point + l.waypoint.label +
 ggtitle("Spatial domain clusters")
 
print(tail(plot.data))
print(dim(plot.data))
print("setup complete")

	print("plotting")

 	source("multiplot.R")

	png(file=plot.file,width=4000,height=800)
	multiplot(p.clusters, m.clusters, p.fluor, m.fluor, p.temperature, m.temperature, p.salinity, m.salinity, p.depth, m.depth, p.ts, m.ts, p.density, m.density, p.trans, m.trans, p.pco2, m.pco2, p.oxygen, m.oxygen, p.light, m.light, p.uw, m.uw, p.flow, m.flow, cols=13)
#	ggsave(file=plot.file,width=20,height=4,units="in")
	dev.off()

 }

 
 
 ##### CALL PLOTTING
 
	#clear open devs
	graphics.off()

	#print station map
		plot.data <- com.data
		print("plotting station map")
	try({
#		png(file="stationmap.png",width=8,height=800)
		m.stations <- ggplot() + 
		 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
		 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon , ylim=range.lat) + 
		 geom_point(data=plot.data, aes(x=LO, y=LA), color=rev(topo.colors(nrow(plot.data)))) +
		 l.waypoint.point + l.araon.point + 
		 l.waypoint.label + 
#		 l.araon.label + 
		 ggtitle("HLY1601 (black) & RV ARAON (red)")

		m.stations
		ggsave(file="stationmap.png", width=8, height=8)

#		dev.off()
		file.copy("stationmap.png","~/Sites/stationmap.png",over=T)
		print("done plotting station map")
		})


	# call plotting for last 96h data
	print("PLOTTING 96h DATA")

	sample_96h <- which(com.data$epoch.s > com.data$epoch.s[nrow(com.data)]- 4*24*60*60)
	sample.set <- round(seq( sample_96h[1] , nrow(com.data) , len=1000))
	plot.data <- com.data[sample.set,]
	range.lat <- c(sort(plot.data$LA)[round(0.01*nrow(plot.data))]-2, sort(plot.data$LA)[round(0.99*nrow(plot.data))]+4)
	range.lon <- c(sort(plot.data$LO)[round(0.01*nrow(plot.data))]-8, sort(plot.data$LO)[round(0.99*nrow(plot.data))]+8)
	try({
		prep.plot(plot.data,"realtime-96h.png")
		print("done plotting 96h data")
		file.copy("realtime-96h.png","~/Sites/realtime-96h.png",over=T)
	})

	# call plotting for last 48h data
	print("PLOTTING 48H DATA")

	sample_48h <- which(com.data$epoch.s > com.data$epoch.s[nrow(com.data)]- 2*24*60*60)
	sample.set <- round(seq( sample_48h[1] , nrow(com.data) , len=1000))
	plot.data <- com.data[sample.set,]
	range.lat <- c(sort(plot.data$LA)[round(0.01*nrow(plot.data))]-2, sort(plot.data$LA)[round(0.99*nrow(plot.data))]+4)
	range.lon <- c(sort(plot.data$LO)[round(0.01*nrow(plot.data))]-8, sort(plot.data$LO)[round(0.99*nrow(plot.data))]+8)
	try({
		prep.plot(plot.data,"realtime-48h.png")
		print("done plotting 48h data")
		file.copy("realtime-48h.png","~/Sites/realtime-48h.png",over=T)
	})
 
	# call plotting for last 1h data
	print("PLOTTING 4H DATA")

		sample_4h <- which(com.data$epoch.s > com.data$epoch.s[nrow(com.data)] - 4*60*60)
		sample.set <- round(seq( sample_4h[1] , nrow(com.data) , len=1000))
		plot.data <- com.data[sample.set,]
		range.lat <- c(sort(plot.data$LA)[round(0.01*nrow(plot.data))] - 0.2, sort(plot.data$LA)[round(0.99*nrow(plot.data))]+0.2)
		range.lon <- c(sort(plot.data$LO)[round(0.01*nrow(plot.data))] - 0.4, sort(plot.data$LO)[round(0.99*nrow(plot.data))]+0.4)
#		range.lat <- c(73.5,74.5)
#		range.lon <- c(-162,-158)
	try({
		prep.plot(plot.data,"realtime-4h.png")
		print("done plotting 4h data")
		file.copy("realtime-4h.png","~/Sites/realtime-4h.png",over=T)
	})


	# call plotting for all data
	print("PLOTTING ALL DATA")

	sample.set <- round(seq(1,nrow(com.data),len=1000))
	plot.data <- com.data[sample.set,]

		range.lat <- c(sort(plot.data$LA)[round(0.01*nrow(plot.data))]-2, sort(plot.data$LA)[round(0.99*nrow(plot.data))]+8)
		range.lon <- c(sort(plot.data$LO)[round(0.01*nrow(plot.data))]-8, sort(plot.data$LO)[round(0.99*nrow(plot.data))]+8)
	try({
		prep.plot(plot.data,"realtime.png")
		print("done plotting all data")
		file.copy("realtime.png","~/Sites/realtime.png",over=T)
	})


}

while(1) {
try({source("realtime-ggplot-test.r")})
Sys.sleep(30)
}
