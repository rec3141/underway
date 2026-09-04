# Runderway: A real-time analytical suite for underway oceanographic data
R scripts for real-time analysis of underway flow-through data aboard the USCGC Healy and other ships running MetAcq software

REALTIME DATA PLOTTER FOR METACQ
by Eric Collins (recollins@alaska.edu), University of Alaska Fairbanks
last updated 8 August 2016

This program plots real-time data from the MetAcq underway system running on the CGC Healy and populates an intranet website with the results.
There are 2 underway systems aboard the Healy, generally we use the portside system except the fluorometer is currently set to use the FWD science lab system

There are a number of programs that do the actual work:

realtime-start.sh starts the program
realtime-runner.r processes the data, is called by realtime-start.sh
realtime-plotter.r plots the data, is called by realtime-start.sh
realtime-udp-rcv.r collects the realtime data from the network via UDP multicast, is called by realtime-runner.r
realtime-timelapse.sh makes timelapse videos from aloftcon imagery, is called from realtime-runner.r
realtime-clusters.r calculates spatial clusters, is called from realtime-plotter.r

Input files:

cruise-number.txt which has the current cruise number, used to format directory structure, e.g. "HLY1601"
*** THIS FILE NEEDS TO BE UPDATED EVERY CRUISE

Station_Plan.xls to include station locations, four columns: site, longitude, latitude, depth; example is included
*** THIS FILE NEEDS TO BE UPDATED EVERY CRUISE

WICOR-parameters.txt which has the list of instruments and their various codes.
*** The WICOR parameters are subject to change depending on the MetACQ settings and should be checked before using
*** this file should be auto-generated in a future version

Output files:

stationmap.png containing a station map
realtime-data.csv containing a tab-delimited text file of a resampled subset of the realtime data
com.data.r containing the underway data in a raw R file

realtime.png containing the realtime data output graphs for the whole cruise
realtime-4h.png containing the realtime data output graphs for the past 4 hours
realtime-48h.png containing the realtime data output graphs for the past 48 hours
realtime-96h.png containing the realtime data output graphs for the past 96 hours

In the folder 'timelapse':
timelapse/timelapse.mp4 containing the timelapse video for the whole cruise
timelapse/timelapse-4h.mp4 containing the timelapse video for the past 4 hours
timelapse/timelapse-48h.mp4 containing the timelapse video for the past 48 hours
timelapse/timelapse-96h.mp4 containing the timelapse video for the past 96 hours

In the folder 'met_raw':
met_raw/%Y%m%d.MET.clean.csv is a daily copy of the cleaned-up .MET file

In various folders:
The program currently makes local copies of the archived data to avoid service interruption.
If this is running on the ship's servers that could be avoided.


Configuration:

1) Update cruise-number.txt with the current cruise number
2) Update realtime-plotter.r with data ranges consistent with current cruise, these are the defaults:
3) May need to update the directories for input/output in realtime-runner.r, depending on the server setup

### SETUP RANGES
range.salinity = c(20,33) #minimum, maximum salinity
range.temperature = c(-2,15) #minimum, maximum temperature
range.flow = c(0,4) #minimum, maximum flow rate voltage
range.fluor = c(min(plot.data$FL.2/50,na.rm=T),max(plot.data$FL.2/50,na.rm=T)) #minimum, maximum fluorescence reading. FL is calibrated (?), FL.2 is raw
range.oxygen = c(6,15) #minimum, maximum oxygen
range.pco2 = c(0,500) #minimum, maximum pCO2
range.density = c(18,max(plot.data$SD.2,na.rm=T)) #minimum, maximum density
range.light = c(min(plot.data$PA,na.rm=T),max(plot.data$PA,na.rm=T)) #minimum, maximum PAR
range.transmissivity = c(0,100)  #minimum, maximum transmissivity
range.depth = c(0,3500) #minimum, maximum depth
		
Dependencies:

The libraries are included in the folder 'lib', but their dependences are not.
Use 'install.packages()' in R or 'cpan' with perl to install them

1) R and R libraries:

library(data.table)
library(ggplot2)
library(gdata)
library(maps)
library(scales)
library(plyr)
library(vegan)

2) perl and perl libraries:

use IO::Socket::Multicast;

3) bash or equivalent for shell commands -- currently works on MacOSX and Ubuntu Linux

4) ffmpeg, included in the 'timelapse' folder
