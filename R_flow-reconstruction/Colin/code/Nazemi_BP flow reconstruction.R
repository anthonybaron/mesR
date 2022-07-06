## (1) This script reads hydrometric station data.
## This represents a transfer (and extension) of the approach provided in .xlsx 
## form by Ali Nazemi while a Senior Hydrologist with SK WSA (in 2016).
## 
## (2) Reconstructs missing flow from observational data following Nazemi data 
## are infilled, and observational and reconstructed records are combined for 
## each station, and flows are broken down by source area (with some caveats).

## Details on the approach are here:
## Proportional flows from locations of the upstream of the BPL are determined 
## (estimated). 
## The ungauged portions are estimated using linear regression of the nearby
## gauges.
## The period of estimation originally covered from January 1, 2001 to October 
## 31, 2013 (and 1972-1992 was used as training for this approach).
## Please see the schematic of the system in the sheet called 
## “Upstream BPL_Train (1972-1992)” in the file BPL upstream_backwater_H&C_April2016. 

# For estimating Iskwao Creek (Y) based on Ridge Creek (X)
# --------------------------------------------------------------------
#  If cold season: Y=0.3112*(X)^0.4537
#  If warm season: If X=0 then Y=0; Otherwise, Y=0.4957*X+0.1185  

# For estimating Ungauged part (U) based on Ridge Creek (X)
# --------------------------------------------------------------------
#  If cold season: U=0.01292*X^3-0.303*X^2+1.249*X-0.0956
#  If warm season: U=-0.05958*X^2+2.77*X-0.1463

# Cold months include December, January, February and March and the rest of 
# months are warm. 
# 
# U includes both positive and negative values. Negative values are related to 
# the episodes, in which the ungauged instream part (including Eyebrow Lake)
# retain part of the water coming from upstream (i.e. diverted flow from LD as 
# well as Iskwao and Ridge creeks). 
# 
# These equations were selected among a pool of 420 forms of linear and
# non-linear regression forms and they stand out based on Bayesian Information 
# Criterion. 
# 
# Coefficient of determination (R2) for estimating the inflow to BPL stays as 
# 0.87 during training period.

library(tidyverse)
library(lubridate)
theme_set(theme_bw(base_size = 12))

## 1
## read station data

# 05JG004 = Qu'Appelle River Above Buffalo Pound Lake
s_05JG004 <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG004_withmissing.csv",skip = 1)  
s_05JG004 <- subset(s_05JG004, PARAM == 1)
s_05JG004$date <- as.Date(s_05JG004[,4]-1,origin = paste(s_05JG004[,3],"01-01",sep = "-")) 

# 05JG006 = Elbow Diversion Canal at Drop Structure
s_05JG006 <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG006_withmissing.csv",skip = 1)  
s_05JG006 <- subset(s_05JG006, PARAM == 1)
s_05JG006$date <- as.Date(s_05JG006[,4]-1,origin = paste(s_05JG006[,3],"01-01",sep = "-"))

# 05JG013 = Ridge Creek Near Bridgeford
s_05JG013 <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG013_withmissing.csv",skip = 1)  
s_05JG013 <- subset(s_05JG013, PARAM == 1)
s_05JG013$date <- as.Date(s_05JG013[,4]-1,origin = paste(s_05JG013[,3],"01-01",sep = "-"))

# 05JG014 = Iskwao Creek Near Craik
s_05JG014 <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG014_withmissing.csv",skip = 1)  
s_05JG014 <- subset(s_05JG014, PARAM == 1)
s_05JG014$date <- as.Date(s_05JG014[,4]-1,origin = paste(s_05JG014[,3],"01-01",sep = "-"))



##create dataframe for flow data
station_flow <- setNames(data.frame(matrix(ncol = 5, nrow = length(seq(as.Date("1972-10-01"),as.Date("2019-12-31"),"days")))),
                         c("date", "05JG004.cms", "05JG006.cms", "05JG013.cms", "05JG014.cms"))
station_flow$date <- seq(as.Date("1972-10-01"),as.Date("2019-12-31"),"days") ##starting date from Nazemi training set

station_flow$`05JG004.cms` <- s_05JG004[match(station_flow$date,s_05JG004$date),5] #m3 s-1
station_flow$`05JG006.cms` <- s_05JG006[match(station_flow$date,s_05JG006$date),5] #m3 s-1
station_flow$`05JG013.cms` <- s_05JG013[match(station_flow$date,s_05JG013$date),5] #m3 s-1
station_flow$`05JG014.cms` <- s_05JG014[match(station_flow$date,s_05JG014$date),5] #m3 s-1



## 2
## infill data and estimate proportion of flows
station_flow[is.na(station_flow[,which(colnames(station_flow)=="05JG013.cms")]), which(colnames(station_flow)=="05JG013.cms")] <- 0 ##missing observations to zero

##reconstruct Iskwao Creek
station_flow$predicted_05JG014.cms <- ifelse(format(as.Date(station_flow$date), "%m") %in% c("12", "01", "02", "03"), 0.3112*station_flow$`05JG013.cms`^0.4537, # cold season
                                          ifelse(station_flow$`05JG013.cms` == 0, 0, 0.4957*station_flow$`05JG013.cms`+0.1185)) # warm season

##reconstruct Ungauged catchment
station_flow$predicted_Ungauged.cms <- ifelse (format(as.Date(station_flow$date), "%m") %in% c("12", "01", "02", "03"), 
                                           0.01292*station_flow$`05JG013.cms`^3-0.303*station_flow$`05JG013.cms`^2+1.249*station_flow$`05JG013.cms`-0.0956, # cold season
                                           -0.05958*station_flow$`05JG013.cms`^2+2.77*station_flow$`05JG013.cms`-0.1463) # warm season

# plot Ungauged (predicted) vs. Ridge Creek Near Bridgeford (05JG013) 
plot(station_flow$`05JG013.cms`,station_flow$predicted_Ungauged.cms)
# two distinct relationships, must be warm and cold seasons? 

## reconstruct BP inflow
## BP inflow is sum of gauged (observed) and ungauged (predicted) flows
station_flow$predicted_05JG004.cms <- station_flow$`05JG013.cms` + station_flow$predicted_05JG014.cms + station_flow$`05JG006.cms` + station_flow$predicted_Ungauged.cms

## combine observational and reconstructed records
station_flow$combined_05JG014.cms <- ifelse (is.na(station_flow$`05JG014.cms`),station_flow$predicted_05JG014.cms,station_flow$`05JG014.cms`)
station_flow$combined_05JG004.cms <- ifelse (is.na(station_flow$`05JG004.cms`),station_flow$predicted_05JG004.cms,station_flow$`05JG004.cms`)

## proportional flows
station_flow$percent_Dief.006 <- station_flow$`05JG006.cms`/station_flow$combined_05JG004.cms*100
station_flow$percent_Ridge.013 <- station_flow$`05JG013.cms`/station_flow$combined_05JG004.cms*100
station_flow$percent_Iskwao.014 <- station_flow$combined_05JG014.cms/station_flow$combined_05JG004.cms*100
station_flow$percent_Ungauged <- station_flow$predicted_Ungauged.cms/station_flow$combined_05JG004.cms*100

# write.csv(station_flow,"./R_flow-reconstruction/Colin/data/BP_flow_output/BP flow infilled with proportions (1972-2019).csv")



## some supporting visuals

# plots proportion from Diefenbaker
plot(station_flow$date,station_flow$`05JG006.cms`/station_flow$combined_05JG004.cms*100, xlim = c(as.Date("1980-01-01"), as.Date("2019-12-31")), ylim = c(0,1000))
##note that dief as proportion is only really at reasonable levels for periods where flows for 05JG004 are predicted, e.g. 
plot(station_flow$date,station_flow$`05JG006.cms`/station_flow$predicted_05JG004.cms*100, xlim = c(as.Date("1980-01-01"), as.Date("2019-12-31")), ylim = c(0,1000)) ##predicted 004 flow
plot(station_flow$date,station_flow$`05JG006.cms`/station_flow$`05JG004.cms`*100, xlim = c(as.Date("1980-01-01"), as.Date("2019-12-31")), ylim = c(0,1000)) ##observational record
##proportional flows should only be used with extreme caution, if at all
plot(station_flow$date,station_flow$`05JG013.cms`/station_flow$combined_05JG004.cms*100, xlim = c(as.Date("1980-01-01"), as.Date("2019-12-31")))

plot(station_flow$`05JG004.cms`,station_flow$`05JG006.cms`) ##raw flows from Diefenbaker and to Buffalo Pound
abline(0,1)



  






station_flow %>% 
  ggplot(aes(date, percent_Dief.006)) + 
  geom_point(alpha = 1/5, colour = ifelse(station_flow$percent_Dief.006 > 100, "red", "green")) +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31")),
       y = c(NA, 1000)) +
  geom_hline(yintercept = 100, colour = "blue")

station_flow %>% 
  ggplot(aes(`05JG004.cms`, `05JG006.cms`)) +
  geom_point(shape = 1) +
  labs(x = "Qu'Appelle River above BPL",
       y = "Elbow Diversion (L. Diefenbbaker)")

station_flow1 %>% 
  ggplot(aes(`05JG004.cms`, `05JG006.cms`)) +
  facet_wrap(~ year) +
  geom_point(shape = 1) +
  labs(x = "Qu'Appelle River above BPL",
       y = "Elbow Diversion (L. Diefenbbaker)")
