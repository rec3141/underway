library(gdate)
library(ggplot2)

uw.in <- read.xls(xls="Log_Samples.xls",sheet="uw.dna")
uw.dat <- uw.in[complete.cases(uw.in),]

uw.dat$epoch.start <- as.numeric(format(as.POSIXlt(paste0(uw.dat$start.date," ",uw.dat$start.time)),"%s"))
uw.dat$epoch.end <- as.numeric(format(as.POSIXlt(paste0(uw.dat$end.date," ",uw.dat$end.time)),"%s"))

uw.dat$y <- seq_along(uw.dat$epoch.start)

ggplot() +
	geom_segment(data=uw.dat, aes(x=epoch.start, xend=epoch.end, y=y, yend=y), col=rainbow(nrow(uw.dat)), size=3)
