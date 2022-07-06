setwd("~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/")
rdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/'
sdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/output/'

pit <- read.csv("~/Documents/work_vm/Research/Saskatchewan/2014/data/output/WhitfieldSKdata_horizon.csv") ##site coordinates
pit$H <- 10^-pit[,10] #get [H] for weighting

##get lumped pit data for pH, OM, C, Sand, Hg
##weighted pit means
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
###calculate weighted mineral for pit (plyr)
pit$wgt <- pit$Rzcorr*pit$BD_2mm #add weighting column
pit <- pit[!is.na(pit$wgt),]##remove null weights
pit <- pit[!pit$Horizon.number == 1,] ##remove lfh

pitmean <- ddply(pit, .(Site), summarise,
                 H=weighted.mean(H,wgt, na.rm=TRUE),
                 OM=weighted.mean(LOI_.,wgt, na.rm=TRUE),
                 Sand=weighted.mean(sand_.,wgt, na.rm=TRUE),
                 Hg=weighted.mean(Total.Hg..ug.kg.,wgt, na.rm=TRUE))
pitmean$C <- pitmean[,3]/1.724 ##van bemmelen factor assumes OM is 58% Carbon
pitmean$pH <- -log10(pitmean[,2])

##
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

pitmean$lat <- meta[match(pitmean[,1],meta[,1]),6]
pitmean$long <- meta[match(pitmean[,1],meta[,1]),7]

##project lat long to utm zone 13
xy <- cbind(pitmean$long,pitmean$lat)
xy <- project(xy,"+proj=utm +zone=13 ellps=WGS84")
pitmean$east <- xy[,1]
pitmean$north <- xy[,2]

##simulation results min and add lat/long
#mydd2 <- read.csv(paste(sdir,"50opt_mindep_5scen_results.csv",sep=""))

#mydd2$lat <- meta[match(mydd2[,2],meta[,1]),6]
#mydd2$long <- meta[match(mydd2[,2],meta[,1]),7]

##project lat long to utm zone 13
#xy <- cbind(mydd2$long,mydd2$lat)
#xy <- project(xy,"+proj=utm +zone=13 ellps=WGS84")
#mydd2$east <- xy[,1]
#mydd2$north <- xy[,2]
###############################################################
shapedir <- "~/Documents/work_vm/Research/Saskatchewan/2013/reports/final/simulation results/raw/shape/"
ecoregions <- readShapeSpatial(paste(shapedir,"BIOTAecoregions.shp",sep=""))

plot(ecoregions) ##just to visualize

###
##
#
png(file = paste(sdir,"Figure3.tiff",sep=""), height = 7.5, width = 10, res = 300, units = "in")
layout(matrix(c(1,2,3,4,5,6,7,8), 2, 4, byrow = TRUE), 
       widths=c(1,1), heights=c(1,3))

hist(pitmean$pH, main = NULL, xlab = "pH", cex=1.5)
hist(pitmean$C, main = NULL, xlab = "Carbon (%)", cex=1.5, ylab=NULL)
hist(pitmean$Sand, main = NULL, xlab = "Sand (%)", cex=1.5, ylab=NULL)
hist(pitmean$Hg, main = NULL, xlab = expression(paste("Hg (", mu,"g kg"^"-1" *")")), cex=1.5, ylab=NULL)

par(mar=c(0,0,0,0))
plot(ecoregions, border =gray(0.1))
ccode <- pitmean$pH
ccode[pitmean[,names(pitmean)=="pH"]<= 4.5] <- "red"
ccode[pitmean[,names(pitmean)=="pH"]<= 5.5 & pitmean[,names(pitmean)=="pH"]> 4.5] <- "orange"
ccode[pitmean[,names(pitmean)=="pH"]<= 6.5 & pitmean[,names(pitmean)=="pH"]> 5.5] <- "yellow"
ccode[pitmean[,names(pitmean)=="pH"]> 6.5] <- "green"
points(pitmean$east,pitmean$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(pitmean$east), y = min(pitmean$north)+50000, inset = 0.5, bg = "white", c("< 4.5", "4.5\u20135.5", "5.5\u20136.5", ">6.5"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("pH")))

plot(ecoregions, border =gray(0.1))
ccode <- pitmean$C
ccode[pitmean[,names(pitmean)=="C"]<= 1] <- "red"
ccode[pitmean[,names(pitmean)=="C"]<= 5 & pitmean[,names(pitmean)=="C"]> 1] <- "orange"
ccode[pitmean[,names(pitmean)=="C"]<= 10 & pitmean[,names(pitmean)=="C"]> 5] <- "yellow"
ccode[pitmean[,names(pitmean)=="C"]> 10] <- "green"
points(pitmean$east,pitmean$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(pitmean$east), y = min(pitmean$north)+50000, inset = 0.5, bg = "white", c("< 1", "1\u20135", "5\u201310", ">10"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("Carbon (%)")))

plot(ecoregions, border =gray(0.1))
Sand <- pitmean[!is.na(pitmean$Sand),]
ccode <- Sand$Sand
ccode[Sand[,names(Sand)=="Sand"]<= 25] <- "red"
ccode[Sand[,names(Sand)=="Sand"]<= 50 & Sand[,names(Sand)=="Sand"]> 25] <- "orange"
ccode[Sand[,names(Sand)=="Sand"]<= 75 & Sand[,names(Sand)=="Sand"]> 50] <- "yellow"
ccode[Sand[,names(Sand)=="Sand"]> 75] <- "green"
points(Sand$east,Sand$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(Sand$east), y = min(Sand$north)+50000, inset = 0.5, bg = "white", c("< 25", "25\u201350", "50\u201375", ">75"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("Sand (%)")))


plot(ecoregions, border =gray(0.1))
Hg <- pitmean[!is.na(pitmean$Hg),]
ccode <- Hg$Hg
ccode[Hg[,names(Hg)=="Hg"]<= 10] <- "red"
ccode[Hg[,names(Hg)=="Hg"]<= 25 & Hg[,names(Hg)=="Hg"]> 10] <- "orange"
ccode[Hg[,names(Hg)=="Hg"]<= 50 & Hg[,names(Hg)=="Hg"]> 25] <- "yellow"
ccode[Hg[,names(Hg)=="Hg"]> 50] <- "green"
points(Hg$east,Hg$north,col = ccode, cex = 0.75, pch = 16)
legend(x= min(Hg$east), y = min(Hg$north)+50000, inset = 0.5, bg = "white", c("< 10", "10\u201325", "25\u201350", ">50"), 
       pch=16, cex=1.5, col = c("red", "orange", "yellow", "green"),
       title = expression(paste("Hg (", mu,"g kg"^"\u20131" *")")))

dev.off()