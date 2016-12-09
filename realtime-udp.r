# THIS PROGRAM CAPTURES REAL TIME DATA FROM
# THE CGC HEALY METACQ SYSTEM AND PLOTS IT 

# To capture UDP data on MacOSX:
#sudo route add -net 224.0.0.0/5 127.0.0.1
#sudo route add -net 232.0.0.0/5 192.168.22.255

library(data.table)
library(ggplot2)
library(gdata)
library(maps)
library(scales)
library(plyr)
source("multiplot.R")
options(digits=12, digits.secs = 3, stringsAsFactors=F)
library(vegan)


#to restart broken run set to FALSE
read_archive=T;

#working directory
setwd("/scratch/HLY1601/")

#directory for sensor backup data, mounted on a shared drive smb://healynas
sensor.dir <- "/Volumes/Data/hly1601/data/sensor"

#directory containing MetAcq raw data within sensor.dir
met.dir <- "met_raw"
dir.create(met.dir)

#directory for serial backup data, mounted on a shared drive smb://healynas
serial.dir <- "/Volumes/Data/hly1601/data/sensor/serial_logger"

#directory containing depth data within serial.dir
depth.dir <- "em122-cb-depth"
dir.create(depth.dir)

#directory containing gps data within serial.dir
gps.dirs <- c("posmv-gps","seapath-gps","mx512-gps","ag132-gps","ashtech-adu5")
for(d in gps.dirs) dir.create(d)

# Read in the table of parameter names
wicor.par <- read.table("WICOR-parameters.txt",sep="\t",header=T,colClasses="character")

# UDP dump file
udp.file <- "udp_dump.csv"

start.time <- proc.time()
photo.time <- proc.time()
elapsed.time <- function() (proc.time() - start.time)[3]
timelapse.time <- function() (proc.time() - photo.time)[3]

met.data <- data.frame()
met.new <- data.frame()
udp.in <- data.frame()
depth.data <- data.frame()
gps.data <- data.frame()

while ( elapsed.time() < 3600 ) {

	loop.start <- proc.time()
	
	# call the timelapse maker every 50 minutes
	
	if (timelapse.time() > 3000) {
	ffmpeg <- system("ps",intern=T)
	if (!length(grep("ffmpeg",ffmpeg))) {
		try({
		system("/Volumes/scratch/HLY1601/run-ffmpeg.sh &")
		})
		print("calling timelapse maker externally")
	}
	photo.time <- proc.time()
	}

	if (read_archive) { 

		#start the UDP receiver if it's not already running
		ps <- system("ps",intern=T)
		if (length(grep("udp_rcv",ps))) {
			print("UDP receiver running")
		} else { 
			system(paste0("/usr/bin/perl ./udp_rcv.pl >> ", udp.file,"&"))
			print("Starting UDP receiver")
		}


		# Read in the corrected daily files (updated hourly)
		print("Reading archived met data")
		if(dir.exists(paste0(sensor.dir,"/",met.dir))) {
			met.files <- list.files(paste0(sensor.dir,"/",met.dir),pattern="*.MET$") 
		} else {
			print("NETWORK DOWN")
			met.files <- list.files(met.dir,pattern="*.MET$")
		}

		for (metfile in met.files) {
			try( {
			file.copy(paste0(sensor.dir,"/",met.dir,"/",metfile),paste0(met.dir,"/",metfile),overwrite=T)
			#clean up dirty files that have wrong number of columns
			system(paste0("perl -ne 'tr/ //s; $z=tr/ //; print $_ if $z==119;' ", met.dir, "/", metfile," > ", met.dir, "/", metfile, ".clean.csv"),wait=T)
			met.new <- read.table(paste0(met.dir,"/",metfile,".clean.csv"),header=T,fill=T,comment.char="",colClasses="character")
			print(metfile)
			met.new[["ymd"]] <- rep(strsplit(metfile,"[.]")[[1]][1],nrow(met.new))
			met.data <- rbind(met.data,met.new)
			})
		}
		met.data <- na.omit(met.data)


#		# Read in the corrected daily files from old cruises
#		print("Reading previous expedition archived met data")
#		
#		if(dir.exists(paste0(sensor.dir,"/",met.dir))) {
#			met.files <- list.files(paste0(sensor.dir,"/",met.dir),pattern="*.MET$") 
#		} else {
#			print("NETWORK DOWN")
#			met.files <- list.files(met.dir,pattern="*.MET$")
#		}
#
#		for (metfile in met.files) {
#			try( {
#			file.copy(paste0(sensor.dir,"/",met.dir,"/",metfile),paste0(met.dir,"/",metfile))
##			system(paste0("perl -ne 'tr/ //s; $z=tr/ //; print $_ if $z==119;' ", met.dir, "/", metfile," > ", met.dir, "/", metfile, ".clean.csv"),wait=T)
#			met.new <- read.table(paste0(met.dir,"/",metfile,".clean.csv"),header=T,fill=T,comment.char="",colClasses="character")
#			print(metfile)
#			met.new[["ymd"]] <- rep(strsplit(metfile,"[.]")[[1]][1],nrow(met.new))
#			met.data <- rbind(met.data,met.new)
#			})
#		}
#		met.data <- na.omit(met.data)



		# Read in raw depth data
		print("Reading archived depth data")
		#1467849600.252 $KIDPT,22.57,8.50,12000.0*7c
	
		if(dir.exists(paste0(serial.dir,"/",depth.dir))) { 
			depth.files <- list.files(paste0(serial.dir,"/",depth.dir),pattern="*.raw$")
		} else {
			depth.files <- list.files(depth.dir,pattern="*.raw$")
		}
		try({
			for(depthfile in depth.files) {
				file.copy(paste0(serial.dir,"/",depth.dir,"/",depthfile),paste0(depth.dir,"/",depthfile),overwrite=T)
				print(depthfile)
				da <- read.csv(paste0(depth.dir,"/",depthfile),head=F,colClasses="character")
				da$epoch.s <- round(as.numeric(sapply(da$V1, function(x) strsplit(x,split=" +",perl=T)[[1]][1])))
				# time and depth
				depth.data <- rbind(depth.data,cbind(as.numeric(da$epoch.s),as.numeric(da$V2)))
			}
		})
		colnames(depth.data) <- c("epoch.s","depth")


		# Read in raw GPS data
		print("Reading archived GPS data")

		for(gps.dir in gps.dirs) {
			if(dir.exists(paste0(serial.dir,"/",gps.dir))) { 
				gps.files <- list.files(paste0(serial.dir,"/",gps.dir),pattern="*.raw$")
			} else {
				gps.files <- list.files(gps.dir,pattern="*.raw$")
			}
			
			try({
			for(gpsfile in gps.files) {
				file.copy(paste0(serial.dir,"/",gps.dir,"/",gpsfile),paste0(gps.dir,"/",gpsfile),overwrite=T)
				print(gpsfile)
				ga <- read.csv(paste0(gps.dir,"/",gpsfile),head=F,colClasses="character")
				ga <- ga[which(substr(ga$V1,5,6)=="GA"),]
				#300: is a hack to avoid the beginning of the log files, which don't start right at midnight
				ga <- ga[300:nrow(ga),]
				ga.ymd <- rep(substr(strsplit(gpsfile,"_")[[1]][2],1,8),nrow(ga))

				gps.data <- rbind(gps.data,cbind(ga[,c(2,3,5)],ga.ymd))
			}
		})
		}
		
		#hard coded for our region
		colnames(gps.data) <- c("timestamp","s_lat","s_lon","ymd")
		gps.data$LA <- as.numeric(substr(gps.data$s_lat,1,2)) + as.numeric(substr(gps.data$s_lat,3,10))/60
		gps.data$LO <- -1 * (as.numeric(substr(gps.data$s_lon,1,3)) + as.numeric(substr(gps.data$s_lon,4,10))/60)

		gps.data$gmt_time <- strptime(paste0(gps.data$ymd,gps.data$timestamp),"%Y%m%d%H%M%OS",tz="GMT")
		gps.data$epoch.s <- format(gps.data$gmt_time,"%s",tz="GMT")

		gps.data <- gps.data[order(gps.data$epoch.s),]
		gps.data <- gps.data[!duplicated.default(gps.data$epoch.s),]

		#finish read_archive
		read_archive=FALSE

	}
	
	# Get new UDP data since last loop
	print("Reading real-time data from the network")
	try( {
		udp.in <- read.table(udp.file,fill=T,sep=",",skip=1,colClasses="character")
		udp.in <- na.omit(udp.in)
	}, silent=T)
	print(nrow(udp.in))

	# Now merge hourly and new data

	print("Configuring data")
	udp.data <- udp.in[c(2,3,seq(6,242,2))]

	for(i in 3:ncol(udp.data)) udp.data[,i] <- as.numeric(udp.data[,i])
	udp.data <- udp.data[complete.cases(udp.data),]
	colnames(udp.data) <- make.names(c("dmy","timestamp",wicor.par$COR.name))
print("ok1")	
	colnames(met.data) <- make.names(c("timestamp", wicor.par$COR.name[1:(ncol(met.data)-2)], "ymd"))
	for(i in 2:ncol(met.data)) met.data[,i] <- as.numeric(met.data[,i])

	#figure out the timing
	udp.data$gmt_time <- strptime(paste0(udp.data$dmy,udp.data$timestamp),"%d%m%y%H%M%S",tz="GMT")
	met.data$gmt_time <- strptime(paste0(met.data$ymd,met.data$timestamp),"%y%m%d%H%M%S",tz="GMT")
print("ok2")	

	udp.data[["epoch.s"]] <- as.numeric(format(udp.data$gmt_time, "%s"),tz="GMT")
	met.data[["epoch.s"]] <- as.numeric(format(met.data$gmt_time, "%s"),tz="GMT")

	# redundancy for GPS location
	udp.ZD_date <- as.data.frame(cbind(udp.data$ZD,udp.data$ZD.2,udp.data$ZD.3,udp.data$ZD.4),colClasses="numeric")
	udp.ZD_date[udp.ZD_date < 1e9] <- as.numeric(format(as.POSIXlt(udp.ZD_date[udp.ZD_date < 1e9], origin="1989-08-17",tz="GMT"),"%s",tz="GMT"))
	udp.ZD_date[udp.ZD_date < 1e9] <- -99
	udp.data$ZD_date <- apply(udp.ZD_date, 1, max)
	#this shows some instruments are reporting different times based on GMT/local ?
	met.data$ZD_date <- apply(data.frame(cbind(met.data$ZD,met.data$ZD.2,met.data$ZD.3,met.data$ZD.4)), 1, max)
print("ok3")	
	
	#merge and subset the archived and new files

	par.subset <- c("gmt_time","epoch.s","ZD_date","PC","AT","TW","TI","LW","SW","PA","TT","SA","SD","FL","OX","ST","TT.2","SA.2","SD.2","FL.2","FL.3","FL.4","OX.2","TR","BA","LA","LO","FI","FI.2","BT","LF","HF","MB")
	com.data <- rbind( met.data[,par.subset] , udp.data[,par.subset])
	com.data <- na.omit(com.data)
	com.data$local_time <- as.POSIXlt(com.data$epoch.s,origin="1970-01-01")

	#clean up messy UDP lines missing lineendings
	com.data[com.data$ZD_date < 0, "ZD_date"] <- NA
	com.data <- na.omit(com.data)
	
print("ok4")	

	#sort by time
	com.data <- com.data[order(com.data$epoch.s),]

#	dups <- duplicated(com.data$epoch.s)
#	com.data <- com.data[-dups,]

# clean up data; need to stay in order
#	com.data[com.data[,"epoch.s"] < 1e9, "epoch.s" ] <- NA
	com.data[com.data == -99] <- -1
#	com.data <- na.omit(com.data)
print("ok5")
print("writing output files")

	write.table(file="common_data.csv",com.data,sep="\t",quote=F)

	file.copy("common_data.csv","~/Sites/tmp.common_data.csv",overwrite=T)
	file.rename("~/Sites/tmp.common_data.csv","~/Sites/realtime_data.csv")

	print("Running plotting script externally")
	save(com.data,file="com.data.r")

#let it call itself
#	system("Rscript realtime-ggplot-test.r &")

loop.end <- proc.time()
print(paste0("loop took ",loop.end - loop.start, "seconds"))
}


# call itself after an hour
rm(list=ls())
source("realtime-udp.r")


