library(readr)
library(dplyr)
library(tidyr)

drainage_areas <- function() {
  
  drain_raw <- read_csv("./R_flow-reconstruction/Colin/data/BP-drainage-areas/catchments.csv")
  
  drain_all <- drain_raw %>% 
    rename(station_num = `Station Number`,
           station_name = `Station Name`,
           province = Province,
           status = Status,
           latitude = Latitude,
           longitude = Longitude,
           year_from = `Year From`,
           year_to = `Year To`,
           drainag_area = `Drainage Area`,
           sediment = Sediment,
           real_time = `Real-Time`,
           datum_name = `Datum Name`) %>% 
    # filter(station_num %in% c("05JG004", "05JG006", "05JG013", "05JG014")) %>% 
    mutate(station_name = ifelse(station_num == "05JG004", "Qu'Appelle River above Buffalo Pound Lake",
                                 ifelse(station_num == "05JG006", "Diefenbaker",
                                        ifelse(station_num == "05JG013", "Ridge Creek",
                                               ifelse(station_num == "05JG014", "Iskwao Creek", 
                                                      ifelse(station_num == "05JG005", "Buffalo Pound Lake near Tuxford",
                                                             ifelse(station_num == "05JG009", "Buffalo Pound Lake at Pumping Station",
                                                                    ifelse(station_num == "05JG010", "Qu'Appelle River above Eyebrow Lake",
                                                                           ifelse(station_num == "05JG011", "Qu'Appelle River below Eyebrow Lake", station_name))))))))) %>% 
    arrange(desc(latitude))
  
  drain <- drain_all %>% select(station_num, station_name, latitude:AAFC_effective)
  
  return(drain)
  
}




