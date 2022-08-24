#For autoprocessing of daily runs on Aqualog that exports ABS, SEM, and, BEM .dat files for every sample
#RU peak area is scanned through RU function and area typed below by user
#SEM and BEM files are scanned without IFE and blank subtration.
#Excitations are columns and Emissions are rows
#SEM and BEM files have headers for rows and columns in row 1 and column 1
#ABS files have row labels in column 1
#Labels are required by code to structure data, if nm wavelengths are missing then code will fail withouth minor modifications 

#First
#Must update run file list
#Structure should have file name in column 1 and sample ID in column 2
files<-read.csv("files.csv")
#Raman Peak Area or QS area; best to update at level of daily scans
RamanArea = 4832.46569599248 #Enter manually
InstCorrect<-read.csv("D:\\DOM\\UVM_DOM\\Pedrial_AquologInstCorrection_CJWsetting.csv", header = TRUE, row.names = 1)

#This is used to select color scheme for contour plots
library("fields", lib.loc="\\\\winfiles1.campus.ad.uvm.edu/cwilli46/MyDocs/R/win-library/3.4")

#MUST SET WORKING DIRECTORY - Session - Set working directory - choose directory

#For manual one by one testing
#abs<-read.table(paste0("20171104_001S0020","ABS.dat"),header=FALSE, fill = TRUE)
#sem<-read.table(paste0("20171104_001S0020","SEM.dat"),header=FALSE, fill = TRUE)
#bem<-read.table(paste0("20171104_001S0020","BEM.dat"),header=FALSE, fill = TRUE)


#################
#This code will automatical correct, find summary infromation, and export corrected eem and graph for each file in the "files" 
output<-NULL
for (j in 1:nrow(files)){
  abs<-read.table(paste0(files[j,1],"ABS.dat"),header=FALSE, fill = TRUE)
  sem<-read.table(paste0(files[j,1],"SEM.dat"),header=FALSE)
  bem<-read.table(paste0(files[j,1],"BEM.dat"),header=FALSE)

#Set Ex and Em wavelength range for eem matrix
Em.eem<-as.numeric(as.character(sem[-1,1]))
Em.eem<-round(Em.eem, digits = 2)
Ex.eem<-rev(as.numeric(as.character(sem[1,-1])))
Ex.eem<-round(Ex.eem, digits = 2)  

#Change order to go from UV to Vis wavlengths and remove labels
sem1<-sem[-1,-1]
sem.rev <- sem1[,rev(names(sem1))]
rownames(sem.rev) <- Em.eem
colnames(sem.rev) <- Ex.eem
sem.rev<-as.matrix(sem.rev)

bem1<-bem[-1,-1]
bem.rev <- bem1[,rev(names(bem1))]
rownames(bem.rev) <- Em.eem
colnames(bem.rev) <- Ex.eem
bem.rev<-as.matrix(bem.rev)

abs.rev <- abs[rev(rownames(abs)),]

#Interpolate abs file at digits = 2
abs.interp<-as.data.frame(approx(abs.rev[,1],abs.rev[,2], method = "linear", xout = seq(min(abs.rev[,1]),max(abs.rev[,1]) , by = 0.01), rule = 2, ties = mean))
abs.interp<-as.data.frame(cbind(round(abs.interp$x, digits = 2),abs.interp[,2]))
rownames(abs.interp) <- abs.interp[,1]
colnames(abs.interp) <- c("nm","cm")

#convert abs values to A and a and subtract average abs between 700 and 800 nm from A and a
#A (m-1) = abs (cm-1)*100
#a (absorption coefficient, m-1) = abs (cm-1)*100*2.303
#mean(abs.interp[which(abs.interp$nm >= 700 & abs.interp$nm <= 800)
A<-NULL
for (s in 1:nrow(abs.interp)){
  A.temp<-ifelse(max(abs.interp[,1]) > 700,(abs.interp[s,2] - mean(abs.interp[which(abs.interp$nm >= 700 & abs.interp$nm <= 800),"cm"]))*100,abs.interp[s,2]*100)
  A<-c(A,A.temp)
}
a<-A*2.303
abs.interp<-cbind(abs.interp,A,a)
rm(a,A,A.temp,s)

######################
#Grab meaningful values from abs data
A254<-abs.interp["254","A"]
A280<-abs.interp["280","A"]
A350<-abs.interp["350","A"]
A440<-abs.interp["440","A"]

#Spectral Slopes and Ratio
a275to295<-abs.interp[which(abs.interp$nm >= 275 & abs.interp$nm <= 295),]
a350to400<-abs.interp[which(abs.interp$nm >= 350 & abs.interp$nm <= 400),]
S275to295<-max(abs(ifelse(a275to295$a > 0,(lm(log(a275to295$a)~a275to295$nm))$coefficients[[2]],NA)))
S350to400<-max(abs(ifelse(a350to400$a > 0,(lm(log(a350to400$a)~a350to400$nm))$coefficients[[2]],NA)))
SR <- ifelse(S275to295 > 0 & S350to400 > 0,S275to295/S350to400,NA)

#############
#Correct sem for instrument, IFE, blank, and set to Raman Units
#When using aqualog instrument correction must be removed then IFE applied and data recorrected, blank subtracted and then set to RU
#This follows recommended order of The Chapman Conference on Organic Matter Fluoresence 2008 per Katherine H. Harrold UNC thesis and Rose Cory
#for aqualog instrument correction file is the quotient of Mcorrect / Xcorrect and this value is divided into the output matrix
#IFE, corrected intensity = measured intensity (without instrument correction) / 10^(-0.5*(Aex + Aem)), where A is the absorbance in cm (same length as sem cuvette)

sem.rev.uncor <- sem.rev / InstCorrect
colnames(sem.rev.uncor) <- Ex.eem

Em.eem.df<-as.data.frame(Em.eem)
colnames(Em.eem.df)<-"nm"
Em.Abs<-merge(Em.eem.df,abs.interp, all.x = TRUE)
Em.Abs[is.na(Em.Abs)]<-0 #This replaces nm not measured for absorbance with 0

Ex.eem.df<-as.data.frame(round(Ex.eem, digits = 2))
colnames(Ex.eem.df)<-"nm"
Ex.Abs<-merge(Ex.eem.df,abs.interp, all.x = TRUE)
Ex.Abs[is.na(Ex.Abs)]<-0 #This replaces nm not measured for absorbance with 0

#The loop makes an IFE absorbance matrix by summing Em and Ex absorbance (cm-1) by Em row
Abs.m<-NULL
for (i in 1:nrow(Em.Abs)){
  load.temp<-(Em.Abs[i,2]+Ex.Abs[,2])
  Abs.m<-rbind(Abs.m,load.temp)
}
colnames(Abs.m) <- Ex.eem
rownames(Abs.m) <- Em.eem
#IFE correction
semIFE <- sem.rev.uncor / 10^(-0.5*(Abs.m))
#Insurment correction, blank subrtaction, and RU normalization
semRU<-(semIFE*InstCorrect-bem.rev)/RamanArea
colnames(semRU) <- Ex.eem
rownames(semRU) <- Em.eem

rm(a275to295,a350to400, i,load.temp,bem,bem1,bem.rev,sem,sem1,sem.rev,sem.rev.uncor,semIFE,abs,abs.rev,abs.interp,Abs.m,Em.eem.df,Em.Abs,Ex.Abs,Ex.eem.df)

###################
#Make 1nm by 1nm semRU matrix
semRU1nm.temp<-NULL
for (k in 1:ncol(semRU)){
  Em.temp<-as.data.frame(approx(rownames(semRU),semRU[,k], method = "linear", xout = seq(round(min(as.numeric(rownames(semRU))), digits = 0),round(max(as.numeric(rownames(semRU))),digits = 0),by = 1), ties = mean))
  semRU1nm.temp<-as.data.frame(cbind(semRU1nm.temp,Em.temp[,2]))
}
colnames(semRU1nm.temp)<-Ex.eem
rownames(semRU1nm.temp)<-Em.temp[,1]

semRU1nm<-NULL
for (z in 1:nrow(semRU1nm.temp)){
  Ex.temp<-as.data.frame(approx(colnames(semRU1nm.temp),semRU1nm.temp[z,],method = "linear", xout = seq(round(min(as.numeric(colnames(semRU1nm.temp))),digits = 0),round(max(as.numeric(colnames(semRU1nm.temp))),digits = 0),by = 1), ties = mean))
  semRU1nm<-as.data.frame(cbind(semRU1nm,Ex.temp[,2]))
}
colnames(semRU1nm)<-Em.temp[,1]
rownames(semRU1nm)<-Ex.temp[,1]
semRU1nm<-as.data.frame(t(as.matrix(semRU1nm)))
rm(k,z,semRU1nm.temp,Ex.temp,Em.temp)

#First order Rayleigh scattter removal
CutBase <- matrix(as.numeric(colnames(semRU1nm)),nrow=nrow(semRU1nm), ncol=ncol(semRU1nm), byrow=TRUE,dimnames = list(rownames(semRU1nm), colnames(semRU1nm)))
CutRegion <- ifelse((as.numeric(rownames(CutBase)) <= CutBase+20) & (as.numeric(rownames(CutBase)) >= CutBase-20), -1, 1)
EEM.cut <- (ifelse(CutRegion == 1,as.matrix(semRU1nm),CutRegion))

#Second order Rayleigh scatter removal
CutRegion2<-ifelse((as.numeric(rownames(CutBase)) <= CutBase*2+10) & (as.numeric(rownames(CutBase)) >= CutBase*2-10), -1, 1)
EEM.cut<-(ifelse(CutRegion2 == 1,as.matrix(EEM.cut),CutRegion2))
colnames(EEM.cut) <- colnames(semRU1nm)
rownames(EEM.cut) <- rownames(semRU1nm)

###################
#Fluoresence Index (FI) Mcknight et al. 2001 modified for instrument correction
#If Ex 370 nm was in interval, equation is: FI <-semRU["470","370"]/semRU["520","370"]
FI <-EEM.cut["470","370"]/EEM.cut["520","370"]

#Beta:Alpha ratio (freshness index) Wilson & Xenopolous 2009; Parlanti et al. 2000
BA <- EEM.cut["380","310"]/max(EEM.cut[c("420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435"),"310"])

#HIX (HUmification Index)
#HIX = sum(Ex254 from em 435 to 480) / sum(Ex254 from em 300 to 345) (Zsolnay et al. 1999)
#HIX.ohno = sum(Ex254 from em 435 to 480) / (sum(Ex254 from em 435 to 480) + sum(Ex254 from em 300 to 345)) (Ohno 2002)
HIX <- sum(EEM.cut[c("435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480"),"254"])/sum(EEM.cut[c("300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345"),"254"])
HIX.ohno <- sum(EEM.cut[c("435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480"),"254"])/(sum(EEM.cut[c("300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345"),"254"])+sum(EEM.cut[c("435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480"),"254"]))

#Coble Peaks, #Coble et al. 1990; Cobel et al. 1998; Coble 1996
#Please note; peaks were not identified explicitly in each EEM. 
#Instead, the max fluoresence (RU) in the region for the expected peak is collected for each peak. 

#PeakA (Ex/Em - 260/380-460; UV humic-like)
PeakA<-max(EEM.cut[c("380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460"),"260"])
#PeakB (Ex/Em - 275/305-310; Tyrosine-like; Protein-like)
PeakB<-max(EEM.cut[c("305","306","307","308","309","310"),"275"])
#PeakC (Ex/Em - 320-360/420-480; Visible humic-like)
PeakC<-max(EEM.cut[c("420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480"),c("320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360")])
#PeakD (Ex/Em - 380-400/500-520; Soil fulvic acid-like)
PeakD<-max(EEM.cut[c("500","501","502","503","504","505","506","507","508","509","510","511","512","513","514","515","516","517","518","519","520"),c("380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400")])
#PeakE (Ex/Em - 450-460/510-530; Soil fulvic acid-like)
PeakE<-max(EEM.cut[c("510","511","512","513","514","515","516","517","518","519","520","521","522","523","524","525","526","527","528","529","530"),c("450","451","452","453","454","455","456","457","458","459","460")])
#PeakM (Ex/Em - 290-312/370-420; Marine or microbial humic-like)
PeakM<-max(EEM.cut[c("370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420"),c("290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312")])
#PeakN (Ex/Em - 280/370; Unknown, microbial-like)
PeakN<-max(EEM.cut["370","280"])
#PeakP (Ex/Em - 398/660; Chlorophyll-like)
PeakP<-ifelse(max(as.character(rownames(EEM.cut)))>=660,max(EEM.cut["660","398"]),NA)
#PeakT (Ex/Em - 275/340; Tryptophan-like; Protein-like)
PeakT<-max(EEM.cut["340","275"])

#Combine indices and write corrected semRU data file

data.j<-as.data.frame(cbind(as.character(files[j,2]),as.character(files[j,1]),A254,A280,A350,A440,S275to295,S350to400,SR,BA,FI,HIX,HIX.ohno,PeakA,PeakB,PeakC,PeakD,PeakE,PeakM,PeakN,PeakP,PeakT))
#files are printed without row and column names
write.table(semRU,file=paste0(files[j,2],".csv"),row.names=FALSE,col.names=FALSE,sep=",")
output<-as.data.frame(rbind(output,data.j))

#Prints contour plot of 1 by 1 nm EEM with scatter peaks removed
png(filename=paste0(files[j,2],".png"), 
    type="cairo",
    bg = "white",
    units="in", 
    width=5, 
    height=3, 
    pointsize=12, 
    res=75)
par(mar = c(4,4,1,3))
filled.contour(as.numeric(colnames(EEM.cut)),as.numeric(rownames(EEM.cut)),t(as.matrix(EEM.cut)),col = tim.colors(n=32), zlim=cbind(0,max(EEM.cut)),xlab="Excitation (nm)",ylab="Emission (nm)", nlevels = 32)
#This function is part of fields package
#contour(image.plot(as.numeric(colnames(EEM.cut)),as.numeric(rownames(EEM.cut)),t(as.matrix(EEM.cut)), zlim=cbind(0,max(EEM.cut)),xlab="Excitation (nm)",ylab="Emission (nm)"))
dev.off()

}
rm(j,data.j, S275to295, S350to400, CutBase,CutRegion,CutRegion2,EEM.cut,semRU,semRU1nm,SR,PeakA,PeakB,PeakC,PeakD,PeakE,PeakM,PeakN,PeakP,PeakT,HIX, HIX.ohno,FI,A254,A280,A350,A440,BA,Em.eem,Ex.eem)

write.table(output,file="output.csv",row.names=FALSE,sep=",")

