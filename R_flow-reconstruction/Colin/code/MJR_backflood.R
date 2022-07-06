## This script reads Moose Jaw River backflooding estimates and parses out 
## information into condensed file with friendly headings.

mjr <- read.csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/MJR_modelled backflooding.csv", skip = 2)

library(dplyr)
library(tidyverse)
mjr <- mjr %>% select_if(~ !any(is.na(.))) %>% as_tibble()

names(mjr)[4:9] <- c("40 cms threshold_uniform demand_lower bound.cms" , "40 cms threshold_uniform demand_upper bound.cms" , "40 cms threshold_allocation demand_lower bound.cms" ,
                     "40 cms threshold_allocation demand_upper bound.cms" , "40 cms threshold_WRMM demand_lower bound.cms" , "40 cms threshold_WRMM demand_upper bound.cms")

names(mjr)[13:18] <- c("50 cms threshold_uniform demand_lower bound.cms" , "50 cms threshold_uniform demand_upper bound.cms" , "50 cms threshold_allocation demand_lower bound.cms" ,
                     "50 cms threshold_allocation demand_upper bound.cms" , "50 cms threshold_WRMM demand_lower bound.cms" , "50 cms threshold_WRMM demand_upper bound.cms")

names(mjr)[22:27] <- c("60 cms threshold_uniform demand_lower bound.cms" , "60 cms threshold_uniform demand_upper bound.cms" , "60 cms threshold_allocation demand_lower bound.cms" ,
                       "60 cms threshold_allocation demand_upper bound.cms" , "60 cms threshold_WRMM demand_lower bound.cms" , "60 cms threshold_WRMM demand_upper bound.cms")

mjr$date <- as.Date(paste(mjr$Year, mjr$month, mjr$day,sep='-'))
mjr <- mjr[,c(1:3, 28, 4:9,13:18,22:27)]

# write.csv(mjr,"./R_flow-reconstruction/Colin/data/BP_flow_output/Moose Jaw River backflooding estimates (2000-2013).csv")


plot(mjr$date, mjr$`40 cms threshold_uniform demand_upper bound.cms`, type = "l")
points(mjr$date, mjr$`50 cms threshold_uniform demand_upper bound.cms`, type = "l", col = "red")
points(mjr$date, mjr$`60 cms threshold_uniform demand_upper bound.cms`, type = "l", col = "blue")

mjr %>% 
  ggplot(aes(date, `40 cms threshold_uniform demand_upper bound.cms`)) +
  geom_line(size = 8, col = "grey") +
  geom_line(aes(date, `50 cms threshold_uniform demand_upper bound.cms`), 
            col = "red", size = 4.5, alpha = 1/2) +
  geom_line(aes(date, `60 cms threshold_uniform demand_upper bound.cms`), 
            col = "blue", size = 1)

