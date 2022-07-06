## check for matches between new and old raw data, and for accuracy of flow 
## reconstruction script


odata <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/1972-1992 training.csv", header = FALSE)
odata$date <- as.Date(paste(odata$V1, odata$V2, odata$V3,sep='-'))

##subset new data to the training observations Ali used (csv file from 'Nazemi_BP flow reconstruction.R')
ndata <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_output/BP flow infilled with proportions (1972-2019).csv") 
ndata <- ndata[c(which(ndata[,2]==min(odata$date)):which(ndata[,2]==max(odata$date))),] ##subset to training set
ndata[3:9][is.na(ndata[3:9])] <- 0 ##set na to zero

##plot original vs new data, check for differences
plot(odata$V15,ndata$X05JG004) ##BP input
sum(odata$V15-ndata$X05JG004)
#ok

plot(odata$V5,ndata$X05JG013) ##Ridge
sum(odata$V5-ndata$X05JG013)
which(!(odata$V5-ndata$X05JG013) == 0) ##rows with discrepancies
#two rows different (in winter... when gauge wasn't operational?)

plot(odata$V4,ndata$X05JG014) ##Iskwao
sum(odata$V4-ndata$X05JG014, na.rm = T)

plot(odata$V11,ndata$X05JG006) ##Dief
sum(odata$V11-ndata$X05JG006)
#ok

##
##using data almost identical to Ali reasons for differences appear due to flags



##plot original estimate vs new estimate, calculate R2 of new, check for differences
##BP inflow
plot(odata$V13,ndata$predicted_05JG004)
abline(0,1)
sum(odata$V13-ndata$predicted_05JG004)
which(!(odata$V13-ndata$predicted_05JG004) == 0) ##rows with discrepancies
#good


##iskwao
plot(odata$V7,ndata$predicted_05JG014)
abline(0,1)
sum(odata$V7-ndata$predicted_05JG014)
which(!(odata$V7-ndata$predicted_05JG014) == 0) ##rows with discrepancies

plot(odata$V4,odata$V7)
abline(0,1)
summary(lm(odata$V7~odata$V4)) ##original fit of predictions
##r2 = 0.71
plot(ndata$X05JG014,ndata$predicted_05JG014)
abline(0,1)
summary(lm(ndata$predicted_05JG014~ndata$X05JG014))##new fit of predictions
##r2 = 0.71



##ungauged
plot(odata$V9,ndata$predicted_Ungauged, col = format(as.Date(ndata$date), "%m"))
abline(0,1)


odata$calculated_Ungauged <- odata$V15 - odata$V5 - odata$V4 - odata$V11
plot(odata$calculated_Ungauged,odata$V9) ##calculated vs predicted ungauged
abline(0,1)
summary(lm(odata$V9~odata$calculated_Ungauged)) ##new fit of predictions
##r2 == 0.51

ndata$calculated_Ungauged <- ndata$X05JG004 - ndata$X05JG013 - ndata$X05JG014 -ndata$X05JG006
plot(ndata$calculated_Ungauged,ndata$predicted_Ungauged) ##calculated vs predicted ungauged
abline(0,1)
summary(lm(ndata$predicted_Ungauged~ndata$calculated_Ungauged)) ##new fit of predictions
##r2 == 0.51


plot(odata$V5, odata$V9, col = odata$V2) ##predictive equation Ali
plot(ndata$X05JG013, ndata$predicted_Ungauged, col = format(as.Date(ndata$date), "%m")) ##predictive equation Colin
points(odata$V5, odata$V9,pch = 2)

old_warm <- subset(odata,V2 == 4 | V2 == 5 | V2 == 6 | V2 == 7 | V2 == 8 | V2 == 9 |V2 == 10 | V2 == 11)
fit1 <- lm(old_warm$V9~poly(as.numeric(old_warm$V5),1,raw=TRUE))
summary(fit1)
plot(old_warm$V5, old_warm$V9, col = odata$V2, xlim = c(0,60), ylim = c(0,60)) ##predictive equation Ali
points(old_warm$V5, predict(fit1, data.frame(x=old_warm$V5)), col="blue")
fit2 <- lm(old_warm$V9~poly(as.numeric(old_warm$V5),2,raw=TRUE))
points(old_warm$V5, predict(fit2, data.frame(x=old_warm$V5)), col="red")
summary(fit2)

old_warm$colin_ungauged <- -0.05958*old_warm$V5^2+2.77*old_warm$V5-0.1463 #Check Colin equation
points(old_warm$V5, old_warm$colin_ungauged, col = "green")



     