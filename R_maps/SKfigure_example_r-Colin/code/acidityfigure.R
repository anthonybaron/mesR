setwd("~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/")
rdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/'
sdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/output/'

##create maps
install.packages("maps")
install.packages("mapproj")
install.packages("mapdata")
install.packages("rgeos")
install.packages("maptools")
install.packages("sp")
install.packages("raster")
install.packages("rgdal")

library(maps)
library(mapdata)
library(mapproj)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)

##read lat/long
meta <- read.csv("~/Documents/work_vm/Research/Saskatchewan/2014/data/output/WhitfieldSKdata_meta.csv")

bcw <- read.csv("BCw.csv")
bcw <- bcw[2:198,]

bcw$lat <- meta[match(bcw[,1],meta[,1]),6]
bcw$long <- meta[match(bcw[,1],meta[,1]),7]

##project lat long to utm zone 13
xy <- cbind(bcw$long,bcw$lat)
xy <- project(xy,"+proj=utm +zone=13 ellps=WGS84")
bcw$east <- xy[,1]
bcw$north <- xy[,2]

shapedir <- "~/Documents/work_vm/Research/Saskatchewan/2013/reports/final/simulation results/raw/shape/"
ecoregions <- readShapeSpatial(paste(shapedir,"BIOTAecoregions.shp",sep=""))

###
png(file = paste(sdir,"Figure5.tiff",sep=""), height = 7.5, width = 10, res = 300, units = "in")
layout(matrix(c(1,2,3,1,2,4), nrow = 2, ncol= 3, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,1))

par(mar=c(0,0,0,0))
plot(ecoregions, border =gray(0.1))
ccode <- bcw$CLS
ccode[bcw[,names(bcw)=="CLS"]<= 25] <- "red"
ccode[bcw[,names(bcw)=="CLS"]<= 100 & bcw[,names(bcw)=="CLS"]> 25] <- "orange"
ccode[bcw[,names(bcw)=="CLS"]<= 500 & bcw[,names(bcw)=="CLS"]> 100] <- "yellow"
ccode[bcw[,names(bcw)=="CLS"]> 500] <- "green"
points(bcw$east,bcw$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(bcw$east), y = min(bcw$north)+125000, inset = 0.5, bg = "white", c("<25", "25\u2013100", "100\u2013500", ">500"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("CL S")))
text(x= max(bcw$east)-30000, y = min(bcw$north)-100000, "A")

plot(ecoregions, border =gray(0.1))
ccode <- bcw$CL.S.N.
ccode[bcw[,names(bcw)=="CL.S.N."]<= 25] <- "red"
ccode[bcw[,names(bcw)=="CL.S.N."]<= 100 & bcw[,names(bcw)=="CL.S.N."]> 25] <- "orange"
ccode[bcw[,names(bcw)=="CL.S.N."]<= 500 & bcw[,names(bcw)=="CL.S.N."]> 100] <- "yellow"
ccode[bcw[,names(bcw)=="CL.S.N."]> 500] <- "green"
points(bcw$east,bcw$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(bcw$east), y = min(bcw$north)+125000, inset = 0.5, bg = "white", c("<25", "25\u2013100", "100\u2013500", ">500"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("CL S+N")))
text(x= max(bcw$east)-30000, y = min(bcw$north)-100000, "B")

###
exc <- read.csv("ex.csv")

exc$lat <- meta[match(exc[,1],meta[,1]),6]
exc$long <- meta[match(exc[,1],meta[,1]),7]

##project lat long to utm zone 13
xy <- cbind(exc$long,exc$lat)
xy <- project(xy,"+proj=utm +zone=13 ellps=WGS84")
exc$east <- xy[,1]
exc$north <- xy[,2]

par(pty="s")
par(mar=c(4,4,3,3))
tabvals <- table(exc$s.exceed)
tabvals2 <- table(exc$sn.exceed)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-50,200),type="l", xlab=expression(Exceedance~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="Cumulative fraction",col=sample(1) )
abline(v=0, col=(8))
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-50,200),type="l", xlab="",ylab="", col=(10))
legend_texts = expression( 'CL S','CL S+N')
legend(-49,1, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("black","red"), cex=0.6)
text(x= 150, y = 0.2, "C")

hexc <- read.csv("H.ex.csv")

tabvals <- table(hexc$s.exceed.H)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-50,200),type="l", ylim=c(0,1), xlab=expression(Exceedance~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="Cumulative fraction",col=sample(1) )
abline(v=0, col=(8))
legend_texts = expression( 'CL S (Harvest)')
legend(-49,1, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5), col=sample(1), cex=0.6)
text(x= 150, y = 0.2, "D")

dev.off()