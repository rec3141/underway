ctd.colnames <- c("empty","scan.count","conductivity","temperature","pressure","depth","oxygen.conc","salinity","time","transmission","par","fluor","sigmat","lat","lon")

#local testing
#ctd.con <- socketConnection(host="192.168.22.100", port=49116)

#actual CTD
ctd.con <- socketConnection(host="192.168.21.83", port=49171)

#need to wait for connection
Sys.sleep(1)

#read.ctd <- function(x) {tryCatch(readLines(con=x), finally=TRUE)}

ctd.save <- data.frame()

plot.par <- "temperature"

going.down=T

while(going.down) {

source("ctd-loop.r")

}
dev.next()

hc.final <- hclust( dist(t(scale(t(scale(data.mat^1.5))))), "cen")
memb.final <- cutree(hc.final, k = k)
cent.final <- NULL
for(j in 1:k){
  cent.final <- rbind(cent.final, colMeans(data.mat[memb.final == j, , drop = FALSE]))
}
cent.final <- as.data.frame(cent.final)
plot(ctd.save$depth ~ ctd.save[[plot.par]], ylim=c(max(ctd.save$depth),0), type="l")
text(cent.final$depth ~ cent.final[[plot.par]], labels=round(cent.final$depth),cex=2)

write.table(file=paste0(format.POSIXct(Sys.time(),"%Y-%m-%d-%H%M%S"), ".ctd.csv"),data.in, sep="\t",col.names=T,quote=F)


