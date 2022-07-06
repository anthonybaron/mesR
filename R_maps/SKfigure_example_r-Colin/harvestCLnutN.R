setwd("~/Critical Loads/code")
rdir <- "~/Critical Loads/raw/"
sdir <- "~/Critical Loads/output/"

allsum <- read.csv("~/Critical Loads/output/allsum.csv")
allsum <- allsum[-1]
allsum.meq <- allsum/10
H.allsum <- read.csv("~/Critical Loads/output/harvest_allsum.csv")
H.allsum <- H.allsum[-1]
H.allsum.meq <- H.allsum/10


Perc.allsum <- sapply(allsum.meq, quantile,0.05, names=FALSE)
Perc.allsum <- as.data.frame(Perc.allsum)
Perc.allsum$p.50 <- sapply(allsum.meq, quantile,0.50, names=FALSE)
Perc.allsum$p.95 <- sapply(allsum.meq, quantile,0.95, names=FALSE)
colnames(Perc.allsum)[1] <- "p.05"

Perc.allsum.H <- sapply(H.allsum.meq, quantile,0.05, names=FALSE)
Perc.allsum.H <- as.data.frame(Perc.allsum.H)
Perc.allsum.H$p.50 <- sapply(H.allsum.meq, quantile,0.50, names=FALSE)
Perc.allsum.H$p.95 <- sapply(H.allsum.meq, quantile,0.95, names=FALSE)
colnames(Perc.allsum.H)[1] <- "p.05"

all <- Perc.allsum$p.50
all <- as.data.frame(all)
colnames(all)[1] <- "p.50"
all$Hp.50 <- Perc.allsum.H$p.50

par(mfrow=c(1,3))
par(pin=c(4.5,4.5))
par(mar=c(4,1,2,2))
par(oma=c(3,3,1,0))
par(pty="s")

tabvals <- table(all$p.50)
tabvals2<- table(all$Hp.50)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-10,40),type="l", xlab=expression('CL'[nut]*'N' ~ '(mmol'[c] *~'m'^-2 *'yr'^-1*')'), ylab="", cex.lab=1.3,col="black" )
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-10,40),type="l", xlab="",ylab="", col="grey50")
legend_texts = expression('CL'[nut]*'N Harvest','CL'[nut]*'N Fire')
legend(-11,1.02, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("grey50","black"), cex=0.8)


spd<- read.csv("~/Critical Loads/raw/soil plot deposition.csv")
sitebased <- read.csv("~/Critical Loads/raw/sitebased.csv")
Ndep <- spd[,c("Site.id", "g.m2013.N_eq.ha")]
colnames(Ndep)[1] <- "Site"
colnames(Ndep)[2] <- "Ndep"
Ndep <- merge(sitebased, Ndep, by="Site")
Ndep$Ndep.meq <- Ndep$Ndep /10
Perc.allsum$ex.05 <- Perc.allsum$p.05 - Ndep$Ndep.meq
Perc.allsum$ex.50 <- Perc.allsum$p.50 - Ndep$Ndep.meq
Perc.allsum$ex.95 <- Perc.allsum$p.95 - Ndep$Ndep.meq

Perc.allsum.H$ex.05 <- Perc.allsum.H$p.05 - Ndep$Ndep.meq
Perc.allsum.H$ex.50 <- Perc.allsum.H$p.50 - Ndep$Ndep.meq
Perc.allsum.H$ex.95 <- Perc.allsum.H$p.95 - Ndep$Ndep.meq

tabvals <- table(Perc.allsum$ex.05)
tabvals2<- table(Perc.allsum$ex.50)
tabvals3 <- table(Perc.allsum$ex.95)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-10,40),type="l", xlab=expression('Exceedance of CL'[nut]*'N' ~ '(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="",cex.lab=1.3,col="grey" )
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-10,40),type="l", xlab="",ylab="", col="black")
par(new = TRUE)
plot(names(tabvals3),(1:length(tabvals3))/length(tabvals3),xlim=c(-10,40),type="l", xlab="",ylab="", col="gray50")
abline(v=0, col=(8))
legend_texts = expression('5'^th*'Percentile','50'^th*'Percentile','95'^th*'Percentile')
legend(-11,1.02, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("grey","black", "gray50"), cex=0.8)


tabvals <- table(Perc.allsum.H$ex.05)
tabvals2<- table(Perc.allsum.H$ex.50)
tabvals3 <- table(Perc.allsum.H$ex.95)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-10,40),type="l", xlab=expression('Exceedance of CL'[nut]*'N' ~ '(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="",cex.lab=1.3,col="grey" )
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-10,40),type="l", xlab="",ylab="", col="black")
par(new = TRUE)
plot(names(tabvals3),(1:length(tabvals3))/length(tabvals3),xlim=c(-10,40),type="l", xlab="",ylab="", col="gray50")
abline(v=0, col=(8))
legend_texts = expression('5'^th*'Percentile','50'^th*'Percentile','95'^th*'Percentile')
legend(-11,1.02, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("grey","black", "gray50"), cex=0.8)

#write.csv(all,paste(sdir,"CLnutN.harv.csv", sep=""))
title(ylab = expression('Cumulative Fraction'),cex.lab=1.6,
      outer = TRUE, line = 1.2)
