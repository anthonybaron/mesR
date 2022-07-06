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
png(file = paste(sdir,"Figure6.tiff",sep=""), height = 7.5, width = 10, res = 300, units = "in")
layout(matrix(c(1,2,1,3), nrow = 2, ncol= 2, byrow = TRUE), 
       widths=c(1,1), heights=c(1,1))

par(mar=c(0,0,0,0))
plot(ecoregions, border =gray(0.1))
ccode <- bcw$CLnutN
ccode[bcw[,names(bcw)=="CLnutN"]<= 10] <- "red"
ccode[bcw[,names(bcw)=="CLnutN"]<= 20 & bcw[,names(bcw)=="CLnutN"]> 10] <- "orange"
ccode[bcw[,names(bcw)=="CLnutN"]> 20] <- "green"
points(bcw$east,bcw$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(bcw$east), y = min(bcw$north)+125000, inset = 0.5, bg = "white", c("0\u201310", "10\u201320", "20\u201330"), 
       pch=16, cex=1.5, col = c("red", "orange", "green"),
       title = expression(paste("CL"[nut]*"N")))
text(x= max(bcw$east)-30000, y = min(bcw$north)-100000, "A")


###
nutNmed <- read.csv("CLnutN.median.csv")

par(mar=c(2,1,2,2))
CLnutN.median <- read.csv("CLnutN.median.csv")
CLnutN.median$CLnutN.meq <- CLnutN.median$CLnutN / 10
CLnutN.median$region <- factor(CLnutN.median$region, levels=c("SLU", "TLU", "AP", "CRU", "MBU"))
boxplot(CLnutN.median$CLnutN.meq ~CLnutN.median$region, ylim = c(0, 50), ylab = "")
axis(3, at = NULL, labels= FALSE); axis(4, at = , labels= FALSE)
mtext(side=2, text=expression('CL'[nut]*"N" *~'(mmol'[c] *~'m'^"\u20132 "*'yr'^"\u20131"*')'), line=2.5)
text(x= 5, y = 45, "B")


###
Perc.allsum <- read.csv("Perc.allsum.csv")

tabvals <- table(Perc.allsum$ex.05)
tabvals2<- table(Perc.allsum$ex.50)
tabvals3 <- table(Perc.allsum$ex.95)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-10,40),type="l", xlab=expression('Exceedance of CL'[nut]*'N' ~ '(mmol'[c] *~'m'^"\u20132 "*'yr'^"/u20131"*')'),ylab="",cex.lab=1.3,col="grey" )
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-10,40),type="l", xlab="",ylab="", col="black")
par(new = TRUE)
plot(names(tabvals3),(1:length(tabvals3))/length(tabvals3),xlim=c(-10,40),type="l", xlab="",ylab="", col="gray50")
abline(v=0, col=(8))
legend_texts = expression('5'^th*'Percentile','50'^th*'Percentile','95'^th*'Percentile')
legend(-11,1.02, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("grey","black", "gray50"), cex=0.8)
text(x= 35, y = 0.2, "C")

dev.off()