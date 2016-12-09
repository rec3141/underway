cd /scratch/HLY1601/imagery/

J=0; for FILE in `find /Volumes/Data/hly1601/data/imagery -name "*.jpeg"`; do J=$((J+1)); K=`printf '%07d' $J`; ln -sf $FILE $K.jpeg; done

#last 4 hours
DATE=$(date -v -4H "+%Y%m%d-%H")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
/work/cryomics/apps/ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y ~/Sites/timelapse/tmp.healy-timelapse-4h.mp4
mv ~/Sites/timelapse/tmp.healy-timelapse-4h.mp4 ~/Sites/timelapse/healy-timelapse-4h.mp4

#last 48 hours
DATE=$(date -v -2d "+%Y%m%d")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
/work/cryomics/apps/ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y ~/Sites/timelapse/tmp.healy-timelapse-48h.mp4
mv ~/Sites/timelapse/tmp.healy-timelapse-48h.mp4 ~/Sites/timelapse/healy-timelapse-48h.mp4

#last 48 hours
DATE=$(date -v -4d "+%Y%m%d")
START=`ls -l | grep $DATE | tr -s ' ' | cut -f9 -d' ' | cut -f1 -d'.' | sort | head -n 1`
/work/cryomics/apps/ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y ~/Sites/timelapse/tmp.healy-timelapse-96h.mp4
mv ~/Sites/timelapse/tmp.healy-timelapse-96h.mp4 ~/Sites/timelapse/healy-timelapse-96h.mp4

#whole cruise
START=0000001
/work/cryomics/apps/ffmpeg -framerate 24 -start_number $START -i %07d.jpeg -c:v libx264 -r 24 -y ~/Sites/timelapse/tmp.healy-timelapse.mp4
mv ~/Sites/timelapse/tmp.healy-timelapse.mp4 ~/Sites/timelapse/healy-timelapse.mp4