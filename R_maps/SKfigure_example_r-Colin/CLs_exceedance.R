setwd("~/Critical Loads/code")
rdir <- "~/Critical Loads/raw/"
sdir <- "~/Critical Loads/output/"


FULLdata <- read.csv("~/Critical Loads/output/FULLdata2015.csv")
spd <- read.csv("~/Critical Loads/raw/soil plot deposition.csv")
H.CLsn <- read.csv("~/Critical Loads/output/harvest_CL_S_N_data2015.csv")
H.CLsn <- H.CLsn[c("Site", "region","CLs_n.meq")]
colnames(H.CLsn)[3]<- "H.CLs_n.meq"
CLsn <- read.csv("~/Critical Loads/output/CL_S_N_data2015.csv")
CLsn <- CLsn[c("Site", "CLs_n.meq")]
CLs <- read.csv("~/Critical Loads/output/FULLdata2015.csv")
CLs <- CLs[c("Site", "CLs.meq")]
H.CLs<- read.csv("~/Critical Loads/output/harvest_FULLdata2015.csv")
H.CLs <- H.CLs[c("Site", "CLs.meq")]
colnames(H.CLs)[2]<- "H.CLs.meq"
boat <- merge(H.CLsn, CLsn, by="Site")
boat <- merge(boat, CLs, by ="Site")
boat <- merge(boat, H.CLs, by = "Site")

Sdep <- spd[,c("Site.id", "g.m2013.S_eq.ha")]
Ndep <- spd[,c("Site.id", "g.m2013.N_eq.ha")]
SNdep <- spd[,c("Site.id", "g.m2013.S_eq.ha", "g.m2013.N_eq.ha")]
SNdep$SNdep.meq <- (SNdep$g.m2013.S_eq.ha + SNdep$g.m2013.N_eq.ha)/10
colnames(SNdep)[1] <- "Site"
colnames(Sdep)[1] <- "Site"
colnames(Sdep)[2] <- "Sdep"
colnames(Ndep)[1] <- "Site"
colnames(Ndep)[2] <- "Ndep"
Sdep$Sdep.meq <- Sdep$Sdep /10
Ndep$Ndep.meq<- Ndep$Ndep/10
ex <- merge(boat, Sdep, by = "Site")
ex <- merge(ex, SNdep, by="Site")
ex <- merge(ex, Ndep, by="Site")
ex.2 <- read.csv("~/Critical Loads/raw/REVISEDexceedance.csv")
ex <- merge(ex.2, Ndep, by="Site")
ex$s.exceed <- ex$CLs.meq - ex$Sdep.meq

ex$sn.exceed <- ex$s.exceed + ex$ex.50

par(mfrow=c(1,2))
par(pin=c(4.5,4.5))
par(mar=c(4,1,2,2))
par(oma=c(3,3,1,0))
par(pty="s")
tabvals <- table(ex$s.exceed)
tabvals2 <- table(ex$sn.exceed)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-50,200),type="l", xlab=expression(Exceedance~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="",col=sample(1) )
abline(v=0, col=(8))
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-50,200),type="l", xlab="",ylab="", col=(10))
legend_texts = expression( 'CL'[S],'CL'[(S+N)])
legend(-49,1, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("black","red"), cex=0.6)


###Harvest, MBU only 


H.ex <- ex[(ex$region=="MBU"),]

H.ex$s.exceed.H <- H.ex$H.CLs.meq - H.ex$Sdep.meq

H.ex$sn.exceed.H. <- H.ex$H.ex.50 +H.ex$s.exceed.H

tabvals <- table(H.ex$s.exceed.H)
tabvals2 <- table(H.ex$sn.exceed.H.)
plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-50,200),type="l", ylim=c(0,1), xlab=expression(Exceedance~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="",col=sample(1) )
abline(v=0, col=(8))
par(new = TRUE)
plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-50,200), ylim=c(0,1),type="l", xlab="",ylab="", col=(10))
legend_texts = expression( 'CL'[S]*~ 'Harvest','CL'[(S+N)]*~ 'Harvest')
legend(-49,1, legend = legend_texts, # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("black","red"), cex=0.6)


title(ylab = expression('Cumulative Fraction'),
      xlab="",cex.lab=1,
      outer = TRUE, line = 1.5)


#tabvals <- table(H.ex$Harvest.CLs.meq)
#tabvals2 <- table(H.ex$Harvest.CLs_n.meq)
#plot(names(tabvals),(1:length(tabvals))/length(tabvals),xlim=c(-50,200),type="l", ylim=c(0,1), xlab=expression(Exceedance~'(mmol'[c] *~'m'^-2 *'yr'^-1*')'),ylab="Cumulative Fraction",col=sample(1) )
#abline(v=0, col=(8))
#par(new = TRUE)
#plot(names(tabvals2),(1:length(tabvals2))/length(tabvals2),xlim=c(-50,200), ylim=c(0,1),type="l", xlab="",ylab="", col=(10))
#legend_texts = expression( 'CL'[S]*~ 'Harvest','CL'[(S+N)]*~ 'Harvest')
#legend(-49,1, legend = legend_texts, # puts text in the legend
#       lty=c(1,1), # gives the legend appropriate symbols (lines)
#       lwd=c(2.5,2.5),col=c("black","red"), cex=0.6)

#H.ex$test <- H.ex$sn.exceed.H - H.ex$s.exceed.H
write.csv(ex,paste(sdir,"ex.csv", sep=""))

write.csv(H.ex,paste(sdir,"H.ex.csv", sep=""))
