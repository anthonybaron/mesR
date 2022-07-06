setwd("~/Critical Loads/code")
rdir <- "~/Critical Loads/raw/"
sdir <- "~/Critical Loads/output/"

## UNCERTAINTY ANALYSIS ###
nsim <- 1000
sitebased <- read.csv("~/Critical Loads/raw/sitebased.csv")
sitebased$Q <- sitebased$Runoff *10000

#sitebased[is.na(sitebased)] <- 0
library(truncnorm)

Nisum<- data.frame(matrix(NA, nrow=199, ncol=3)) 
Nisum[,1] <- sitebased$Site
Nisum[,2] <- sitebased$region

for (ind1 in 1:199){
  Nisum[,3]  <-((((sitebased$BD)* (sitebased$N/100) * (sitebased$Rootdepth)*10000)/ (if(sitebased$region[ind1] == "SLU"){
    soil.age <- 9000
  } else if (sitebased$region[ind1] == "TLU"){
    soil.age <- 9000
  } else if (sitebased$region[ind1] == "AP"){
    soil.age <- 9500
  } else if (sitebased$region[ind1] == "CRU"){
    soil.age <- 10000
  } else if (sitebased$region[ind1] == "MBU"){
    soil.age <- 11000
  })) *714.28)  ##g/m2yr to eq/hayr
}

Nisum[Nisum[,3]==0,3] <- NA
Ni <- aggregate(Nisum[3],by = Nisum[2], mean,na.rm=T)
NiSD <- aggregate(Nisum[3],by = Nisum[2], sd,na.rm=T)

AP <- rtruncnorm(1000,quantile(Nisum[3],0.05,na.rm=T),min(quantile(Nisum[3],0.95,na.rm=T),max(rnorm(1000,Ni[which(Ni[1]=="AP"),2],NiSD[which(Ni[1]=="AP"),2]))),Ni[which(Ni[1]=="AP"),2],NiSD[which(Ni[1]=="AP"),2])

MBU <- rtruncnorm(1000,quantile(Nisum[3],0.05,na.rm=T),min(quantile(Nisum[3],0.95,na.rm=T),max(rnorm(1000,Ni[which(Ni[1]=="MBU"),2],NiSD[which(Ni[1]=="MBU"),2]))),Ni[which(Ni[1]=="MBU"),2],NiSD[which(Ni[1]=="MBU"),2])

CRU <- rtruncnorm(1000,quantile(Nisum[3],0.05,na.rm=T),min(quantile(Nisum[3],0.95,na.rm=T),max(rnorm(1000,Ni[which(Ni[1]=="CRU"),2],NiSD[which(Ni[1]=="CRU"),2]))),Ni[which(Ni[1]=="CRU"),2],NiSD[which(Ni[1]=="CRU"),2])

SLU <- rtruncnorm(1000,quantile(Nisum[3],0.05,na.rm=T),min(quantile(Nisum[3],0.95,na.rm=T),max(rnorm(1000,Ni[which(Ni[1]=="SLU"),2],NiSD[which(Ni[1]=="SLU"),2]))),Ni[which(Ni[1]=="SLU"),2],NiSD[which(Ni[1]=="SLU"),2])

TLU <- rtruncnorm(1000,quantile(Nisum[3],0.05,na.rm=T),min(quantile(Nisum[3],0.95,na.rm=T),max(rnorm(1000,Ni[which(Ni[1]=="TLU"),2],NiSD[which(Ni[1]=="TLU"),2]))),Ni[which(Ni[1]=="TLU"),2],NiSD[which(Ni[1]=="TLU"),2])

rnorm2 <- function(n,mean,sd) { rtruncnorm(nsim,max(0.1,mean-sd),mean+sd,mean,sd) }
allsum <- data.frame(matrix(NA, nrow=1000, ncol=199))

for (ind1 in 1:199)  {
#Nu = eq.ha.yr  
  allsum[ind1] <- ( ((((if(sitebased$canopy[ind1] == "jack pine"){
    N.content <- 196
  } else if (sitebased$canopy[ind1] == "trembling aspen"){
    N.content <- 368
  } else if (sitebased$canopy[ind1] == "black spruce"){
    N.content <- 238
    #N.content <- 165
  } else if (sitebased$canopy[ind1] == "white birch"){
    N.content <- 730
  } else if (sitebased$canopy[ind1] == "white spruce"){
    N.content <- 326
  }) *
   
      (if((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "SLU")){
        ba <- rtruncnorm(1000,max(0.1,8.18-2.86),8.18+2.86,8.18,2.86)
        #ba <- rnorm(nsim, 8.18, 2.86)
      } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "SLU")){
        ba <- rtruncnorm(1000,max(0.1,5.1-1.67),5.1+1.67,5.1,1.67)
        #ba <- rnorm(nsim, 5.1, 1.67)
      } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "TLU")){
        ba <- rtruncnorm(1000,max(0.1,8.18-2.86),8.18+2.86,8.18,2.86)
        #ba <- rnorm(nsim, 8.18, 2.86)
      } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "TLU")){
        ba <- rtruncnorm(1000,max(0.1,5.1-1.67),5.1+1.67,5.1,1.67)
        #ba <- rnorm(nsim, 5.1, 1.67)
      } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "AP")){
        ba <- rtruncnorm(1000,max(0.1,13.2-3.63),13.2+3.63,13.2,3.63)
        #ba <- rnorm(nsim, 13.2, 3.63)
      } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "AP")){
        ba <- rtruncnorm(1000,max(0.1,9.83-3.14),9.83+3.14,9.83,3.14)
        #ba <- rnorm(nsim, 9.83, 3.14)
      } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "AP")){
        ba <- rtruncnorm(1000,max(0.1,8.94-2.22),8.94+2.22,8.94,2.22)
        #ba <- rnorm2(nsim, 8.94, 2.22)
      } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "AP")){
        ba <- rtruncnorm(1000,max(0.1,5.4-1.66),5.4+1.66,5.4,1.66)
        #ba <- rnorm2(nsim, 5.4, 1.66)
      } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "CRU")){
        ba <- rtruncnorm(1000,max(0.1,13.2-3.63),13.2+3.63,13.2,3.63)
        #ba <- rnorm2(nsim, 13.2, 3.63)
      } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "CRU")){
        ba <- rtruncnorm(1000,max(0.1,9.83-3.14),9.83+3.14,9.83,3.14)
        #ba <- rnorm2(nsim, 9.83, 3.14)
      } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "CRU")){
        ba <- rtruncnorm(1000,max(0.1,8.94-2.22),8.94+2.22,8.94,2.22)
        #ba <- rnorm2(nsim, 8.94, 2.22)
      } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "CRU")){
        ba <- rtruncnorm(1000,max(0.1,5.4-1.66),5.4+1.66,5.4,1.66)
        #ba <- rnorm2(nsim, 5.4, 1.66)
      } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "MBU")){
        ba <- rtruncnorm(1000,max(0.1,20.28-4.5),20.28+4.5,20.28,4.5)
        #ba <- rnorm2(nsim, 20.28, 4.5)
      } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "MBU")){
        ba <- rtruncnorm(1000,max(0.1,19.86-4.46),19.86+4.46,19.86,4.46)
        #ba <- rnorm2(nsim, 19.86, 4.46)
      } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "MBU")){
        ba <- rtruncnorm(1000,max(0.1,14.07-1.73),14.07+1.73,14.07,1.73)
        #ba <- rnorm2(nsim, 14.07, 1.73)
      } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "MBU")){
        ba <- rtruncnorm(1000,max(0.1,8.3-2.12),8.3+2.12,8.3,2.12)
        #ba <- rnorm2(nsim, 8.3, 2.12)
      } else if ((sitebased$canopy[ind1] == "white spruce")&(sitebased$region[ind1] == "MBU")){
        ba <- rtruncnorm(1000,max(0.1,18.7-2.33),18.7+2.33,18.7,2.33)
        #ba <- rnorm2(nsim, 18.7, 2.33)
      })/ 
      
      (if(sitebased$canopy[ind1] == "jack pine"){
        BA.pa <- 30.4
      } else if (sitebased$canopy[ind1] == "black spruce"){
        BA.pa <- 11.66
        #BA.pa <- 36.7
      } else if (sitebased$canopy[ind1] == "trembling aspen"){
        BA.pa <- 34.7  
      } else if (sitebased$canopy[ind1] == "white birch"){
        BA.pa <- 3.1
      } else if (sitebased$canopy[ind1] == "white spruce"){
        BA.pa <- 44.9
      }))/
    
  (if((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "SLU")){
      age <- rnorm2(nsim, 60.75, 7.79)
    } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "SLU")){
      age <- rnorm2(nsim, 105, 3.81)
    } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "TLU")){
      age <- rnorm2(nsim, 60.75, 7.79)
    } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "TLU")){
      age <- rnorm2(nsim, 105, 3.81)
    } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "AP")){
      age <- rnorm2(nsim, 74.25, 8.62)
    } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "AP")){
      age <- rnorm2(nsim, 66.67, 8.16)
    } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "AP")){
      age <- rnorm2(nsim, 68, 2.96)
    } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "AP")){
      age <- rnorm2(nsim, 90.43, 5.66)
    } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "CRU")){
      age <- rnorm2(nsim, 74.25, 8.62)
    } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "CRU")){
      age <- rnorm2(nsim, 66.67, 8.16)
    } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "CRU")){
      age <- rnorm2(nsim, 68, 2.96)
    } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "CRU")){
      age <- rnorm2(nsim, 90.43, 5.66)
    } else if ((sitebased$canopy[ind1] == "jack pine")&(sitebased$region[ind1] == "MBU")){
      age <- rnorm2(nsim, 68.75, 8.29)
    } else if ((sitebased$canopy[ind1] == "trembling aspen")&(sitebased$region[ind1] == "MBU")){
      age <- rnorm2(nsim, 69.25, 8.32)
    } else if ((sitebased$canopy[ind1] == "white birch")&(sitebased$region[ind1] == "MBU")){
      age <- rnorm2(nsim, 60.7, 2.83)
    } else if ((sitebased$canopy[ind1] == "white spruce")&(sitebased$region[ind1] == "MBU")){
      age <- rnorm2(nsim, 85, 3.56)
    } else if ((sitebased$canopy[ind1] == "black spruce")&(sitebased$region[ind1] == "MBU")){
      age <- rnorm2(nsim, 91.33, 3.56)
    }))*71.428) +
  
    ####Ni = eq/ha/yr
    
  (if(sitebased$region[ind1] == "SLU"){
      Ni <- SLU
    } else if (sitebased$region[ind1] == "TLU"){
      Ni <- TLU
    } else if (sitebased$region[ind1] == "AP"){
      Ni <- AP
    } else if (sitebased$region[ind1] == "CRU"){
      Ni <- CRU
    } else if (sitebased$region[ind1] == "MBU"){
      Ni <- MBU
    })+
  
  
    ####N le(acc) = eq/ha/yr
    (((runif(nsim, sitebased$Q[ind1]-(sitebased$Q[ind1]*0.10), sitebased$Q[ind1]+(sitebased$Q[ind1]*0.10))) * (if(sitebased$canopy[ind1] == "jack pine"){
      N.le <- runif(nsim, 0.00765, 0.02095)
    } else if (sitebased$canopy[ind1] == "black spruce" ){
      N.le <- runif(nsim, 0.00765, 0.02095)
    } else if (sitebased$canopy[ind1] == "trembling aspen" ){
      N.le <- runif(nsim, 0.0143, 0.0276)
    } else if (sitebased$canopy[ind1] == "white birch" ){
      N.le <- runif(nsim, 0.0143, 0.0276)
    } else if (sitebased$canopy[ind1] == "white spruce" ){
      N.le <- runif(nsim, 0.00765, 0.02095)
    }))/
      ####fde 
      (if(sitebased$fde.range.num[ind1] == 1){
        fde <- -(runif(nsim, 0, 0.05))+1
      } else if(sitebased$fde.range.num[ind1] == 2){
        fde <- -(runif(nsim, 0.05, 0.10))+1
      } else if(sitebased$fde.range.num[ind1] == 3){
        fde <- -(runif(nsim, 0.10, 0.20))+1 
      })) )
  }


write.csv(allsum, paste(sdir, "allsum.csv",sep = ""))



#make cumulative distribution fig for 0.05, 0.50 and 0.95 quantle
#convert allsum from eq/ha/yr to mmolc/m2/yr
allsum.test <- allsum/10

Percentiles <- sapply(allsum.test, quantile,0.05, names=FALSE)
Percentiles <- as.data.frame(Percentiles)
Percentiles$p.50 <- sapply(allsum.test, quantile,0.50, names=FALSE)
Percentiles$p.95 <- sapply(allsum.test, quantile,0.95, names=FALSE)
colnames(Percentiles)[1] <- "p.05"

tabvals <- table(Percentiles$p.05)
tabvals2<- table(Percentiles$p.50)
tabvals3 <- table(Percentiles$p.95)
par(pin=c(4.5,4.5))
par(mar=c(1,2,1,1))
par(oma=c(0,2,0,0))
par(pty="s")
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(0,40),type="l", xlab=expression('CL'[nut]*'N' ~ '(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="Cumulative Fraction",col="grey" )
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(0,40),type="l", xlab="",ylab="", col="black")
par(new = TRUE)
plot(names(tabvals3),(1:length(tabvals3))/length(tabvals3),xlim=c(0,40),type="l", xlab="",ylab="", col="gray50")

legend_texts = expression('5'^th*'Percentile','50'^th*'Percentile','95'^th*'Percentile')
legend(0,1, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("grey","black", "gray50"), cex=0.6)
title(ylab = expression('Cumulative Fraction'),cex.lab=1,
      outer = TRUE, line = 0.5)
summary(allsum.test)

par(pin=c(4.5,4.5))
par(mar=c(2,1,2,2))
par(oma=c(1,3,0,0))
CLnutN.median <- read.csv("~/Critical Loads/raw/CLnutN.median.csv")
CLnutN.median$CLnutN.meq <- CLnutN.median$CLnutN / 10
CLnutN.median$region <- factor(CLnutN.median$region, levels=c("SLU", "TLU", "AP", "CRU", "MBU"))
boxplot(CLnutN.median$CLnutN.meq ~CLnutN.median$region, ylim = c(0, 50), ylab = "")
axis(3, at = NULL, labels= FALSE); axis(4, at = , labels= FALSE)
mtext(side=2, text=expression('CL'[nut]*"N" *~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'), line=2.5)
CLnutN.50 <- Percentiles$p.50
write.csv(CLnutN.50, paste(sdir, "CLnutN.50.csv",sep = ""))
