setwd("~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/")
rdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/'
sdir <- '~/Documents/work_vm/Research/Saskatchewan/2014/2014summaryreport/r/output/'

bcw <- read.csv("BCw.csv")
bcw <- bcw[2:198,]
bcw$ecoregion <- substr(bcw[,1],1,1)
bcw[bcw[,12]=="A",12] <- "Selwyn Lake Upland"
bcw[bcw[,12]=="B",12] <- "Tazin Lake Upland"
bcw[bcw[,12]=="C",12] <- "Athabasca Plain"
bcw[bcw[,12]=="D",12] <- "Churchill River Upland"
bcw[bcw[,12]=="E",12] <- "Mid-Boreal Upland"

png(file = paste(sdir,"Figure4.tiff",sep=""), height= 8, width=10.5, res = 300, units="in")
par(mar=c(2.5,4.5,0.5,0.1))
boxplot(bcw[,4]~bcw[,12], ylab = expression('Base cation weathering rate (mmol'[c]*" m"^"\u20132"*" yr"^"\u20131"*")"), ylim = c(0,1000))
dev.off()
