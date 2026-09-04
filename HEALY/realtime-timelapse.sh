### REALTIME-TIMELAPSE.SH
### by Eric Collins (recollins@alaska.edu), University of Alaska Fairbanks
### last updated 8 August 2016

### This program makes timelapse videos from the Healy's aloftcon webcam images
### There is a separate program, realtime-runner.r, that calls this program every hour


#cruise number
CRUISE=`cat cruise-number.txt`
SMCRUISE=`cat cruise-number.txt | tr 'A-Z' 'a-z'`
#working directory
WORKINGDIR=/scratch/$CRUISE/timelapse/
mkdir WORKINGDIR
cd $WORKINGDIR

#source image directory
	
IMAGDIR=/Volumes/Data/$SMCRUISE/data/imagery

J=0; for FILE in `find $IMAGDIR -name "*.jpeg"`; do J=$((J+1)); K=`printf '%07d' $J`; ln -sf $FILE $K.jpeg; done

#last 4 hours
DATE=$(date -v -4H "+%Y%m%d-%H")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
./ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y tmp.timelapse-4h.mp4
mv tmp.timelapse-4h.mp4 timelapse-4h.mp4

#last 48 hours
DATE=$(date -v -2d "+%Y%m%d")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
./ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y tmp.timelapse-48h.mp4
mv tmp.timelapse-48h.mp4 timelapse-48h.mp4

#last 96 hours
DATE=$(date -v -4d "+%Y%m%d")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
./ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y tmp.timelapse-96h.mp4
mv tmp.timelapse-96h.mp4 timelapse-96h.mp4

#whole cruise
START=0000001
./ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y tmp.timelapse.mp4
mv tmp.timelapse.mp4 timelapse.mp4