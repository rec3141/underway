### REALTIME-PLOTTER.R
### by Eric Collins (recollins@alaska.edu), University of Alaska Fairbanks
### last updated 8 August 2016

### This program plots real-time data from the Healy's underway system
### There is a separate program, realtime-runner.r, that processes the data for input into this program

library(data.table)
library(ggplot2)
library(gdata)
library(maps)
library(scales)
library(plyr)
library(vegan)
options(digits=12, digits.secs = 3, stringsAsFactors=F)

rm(list=ls())


##SETUP

# requires files com.data.r exported from realtime-runner.r
if(!file.exists("com.data.r")) stop("data file not available")
# can use Station Plan if available:
if(!file.exists("Station_Plan.xls")) warn("Station_Plan.xls not available -- cannot plot waypoints")
# uses external R script to calculate spatial domain clusters
if(!file.exists("realtime-clusters.r")) warn("realtime-clusters.r not available -- cannot calculate spatial domains")

start.time <- proc.time()
elapsed.time <- function() (proc.time() - start.time)[3]

#run once per hour
while ( elapsed.time() < 3600 ) {

	print("setup plotting")

	#sometimes need to wait for input to be written by realtime-runner.r
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

	#WARNING: this is hard coded for our working area
	map.projection = "orthographic"
	map.orientation = c(70, -160, 0)
	range.lat <- c(55,83)
	range.lon <- c(-180, -150)


	print("#0 Waypoints")
	#0 Waypoints
	try({
		waypoint.in <- read.xls(xls="Station_Plan.xls",sheet="Plan")
		waypoint.data <- waypoint.in[,1:4]
		waypoint.data <- waypoint.data[complete.cases(waypoint.data),]
		l.waypoint.point <- geom_point(data=waypoint.data, aes(x=longitude, y=latitude), color="black", fill="black")
		l.waypoint.label <-	geom_label(data=waypoint.data, aes(x=longitude, y=latitude, label=X), size=2, fill="white", alpha="0.2", nudge_x=0.1*(abs(range.lat[1]-range.lat[2])))
	})


	### I include the plotting function within the loop so I can change it on the fly
	
	######## PLOTTING FUNCTION, CALL WITH FILE NAME

	prep.plot <- function(plot.data, plot.file) {

		#print(tail(plot.data))

		### SETUP RANGES

		# WARNING: these are hard coded for our working ranges
		range.salinity = c(20,33)
		range.temperature = c(-2,15)
		range.flow = c(0,4)
		range.fluor = c(min(plot.data$FL.2/50,na.rm=T),max(plot.data$FL.2/50,na.rm=T)) #WARNING: this is hard coded for relative fluorescence units
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
			geom_point(data=plot.data, aes(x=local_time, y=TR), col=mapcolors) + #portside transmission
			xlab("UTC time") + ylab("% Transmissivity") + ylim(range.transmissivity)

		m.trans <- ggplot() + 
		 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
		 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon ,ylim=range.lat) + 
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


		print("#12 spatial domains")
		#12 spatial domains

		#takes plot.data as input, outputs plot.data$clusters
		source("realtime-clusters.r",local=T)
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

		png(file=plot.file,width=4000,height=800)
		multiplot(p.clusters, m.clusters, p.fluor, m.fluor, p.temperature, m.temperature, p.salinity, m.salinity, p.depth, m.depth, p.ts, m.ts, p.density, m.density, p.trans, m.trans, p.pco2, m.pco2, p.oxygen, m.oxygen, p.light, m.light, p.flow, m.flow, cols=12)
		dev.off()

	}


	# Multiple plot function
	#
	# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
	# - cols:   Number of columns in layout
	# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
	#
	# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
	# then plot 1 will go in the upper left, 2 will go in the upper right, and
	# 3 will go all the way across the bottom.
	#
	multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	  library(grid)

	  # Make a list from the ... arguments and plotlist
	  plots <- c(list(...), plotlist)

	  numPlots = length(plots)

	  # If layout is NULL, then use 'cols' to determine layout
	  if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
						ncol = cols, nrow = ceiling(numPlots/cols))
	  }

	 if (numPlots==1) {
		print(plots[[1]])

	  } else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			print(paste0("printing ",i))
		  # Get the i,j matrix positions of the regions that contain this subplot
		  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

		  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
										  layout.pos.col = matchidx$col))
		}
	  }
	}


 
	 ##### CALL PLOTTING
 
		#clear open devs
		graphics.off()

		#print station map
			plot.data <- com.data
			print("plotting station map")
		try({
			m.stations <- ggplot() + 
			 geom_polygon(data=map_data("world"), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
			 coord_map(projection=map.projection, orientation=map.orientation, xlim=range.lon , ylim=range.lat) + 
			 geom_point(data=plot.data, aes(x=LO, y=LA), color=rev(topo.colors(nrow(plot.data)))) +
			 l.waypoint.point + 
			 l.waypoint.label + 
			 ggtitle("Cruise Plan")

			m.stations
			ggsave(file="stationmap.png", width=8, height=8)

			file.copy("stationmap.png","stationmap.png",over=T)
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
			file.copy("realtime-96h.png","realtime-96h.png",over=T)
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
			file.copy("realtime-48h.png","realtime-48h.png",over=T)
		})
 
		# call plotting for last 1h data
		print("PLOTTING 4H DATA")

			sample_4h <- which(com.data$epoch.s > com.data$epoch.s[nrow(com.data)] - 4*60*60)
			sample.set <- round(seq( sample_4h[1] , nrow(com.data) , len=1000))
			plot.data <- com.data[sample.set,]
			range.lat <- c(sort(plot.data$LA)[round(0.01*nrow(plot.data))] - 0.2, sort(plot.data$LA)[round(0.99*nrow(plot.data))]+0.2)
			range.lon <- c(sort(plot.data$LO)[round(0.01*nrow(plot.data))] - 0.4, sort(plot.data$LO)[round(0.99*nrow(plot.data))]+0.4)
		try({
			prep.plot(plot.data,"realtime-4h.png")
			print("done plotting 4h data")
			file.copy("realtime-4h.png","realtime-4h.png",over=T)
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
			file.copy("realtime.png","realtime.png",over=T)
		})


}



#### RERUN IF IT STOPS
while(1) {
	try({source("realtime-plotter.r")})
	Sys.sleep(30)
}