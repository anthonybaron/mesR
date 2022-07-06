library(tidyverse)
library(lubridate)

parameters <- c("Chlorophyll a", "Sulphate", "SUVA", "Phosphate (ortho)", 
                "Phosphate (total)", "DOC", "UV 254", "Colour (Apparent)", 
                "Conductivity", "pH", "Alkalinity (total)", "Hardness (total)", 
                "Turbidity", "Temperature", "pH")

BP_longterm <- read_csv("data/BPWTP_labdat_current.csv") %>%
  filter(datasheet == "RawWater",
         parameter %in% parameters) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-c(datasheet, station, parm_eval, parm_tag, result_flag, result_org,
            sheet_year, year, month, week, unit, parm_unit)) 

doc <- BP_longterm %>% 
  filter(parameter == "DOC") %>% 
  pivot_wider(names_from = parameter, values_from = result)

pH <- BP_longterm %>% 
  filter(parameter == "pH") %>% 
  pivot_wider(names_from = parameter, values_from = result)

suva <- BP_longterm %>% 
  filter(parameter == "SUVA") %>% 
  pivot_wider(names_from = parameter, values_from = result)

alk <- BP_longterm %>% 
  filter(parameter == "Alkalinity (total)") %>% 
  pivot_wider(names_from = parameter, values_from = result)

chla <- BP_longterm %>% 
  filter(parameter == "Chlorophyll a") %>% 
  pivot_wider(names_from = parameter, values_from = result)

sulph <- BP_longterm %>% 
  filter(parameter == "Sulphate") %>% 
  pivot_wider(names_from = parameter, values_from = result)

orthoP <- BP_longterm %>% 
  filter(parameter == "Phosphate (ortho)") %>% 
  pivot_wider(names_from = parameter, values_from = result)

totalP <- BP_longterm %>% 
  filter(parameter == "Phosphate (total)") %>% 
  pivot_wider(names_from = parameter, values_from = result)

# uv254 <- BP_longterm %>% 
#   filter(parameter == "UV 254") %>% View()
#   pivot_wider(names_from = parameter, values_from = result)

ccolour <- BP_longterm %>% 
  filter(parameter == "Colour (Apparent)") %>% 
  pivot_wider(names_from = parameter, values_from = result)

cond <- BP_longterm %>% 
  filter(parameter == "Conductivity") %>% 
  pivot_wider(names_from = parameter, values_from = result)

hardness <- BP_longterm %>% 
  filter(parameter == "Hardness (total)") %>% 
  pivot_wider(names_from = parameter, values_from = result)

turb <- BP_longterm %>% 
  filter(parameter == "Turbidity") %>% 
  pivot_wider(names_from = parameter, values_from = result)

temp <- BP_longterm %>% 
  filter(parameter == "Temperature") %>% 
  pivot_wider(names_from = parameter, values_from = result)

BP_wide <- full_join(doc, suva) %>% 
  full_join(., alk) %>% 
  full_join(., chla) %>%
  full_join(., sulph) %>%
  full_join(., orthoP) %>%
  full_join(., totalP) %>%
  full_join(., ccolour) %>%
  full_join(., cond) %>%
  full_join(., hardness) %>%
  full_join(., turb) %>%
  full_join(., temp) %>% 
  full_join(., pH) %>% 
  rename(alkalinity = `Alkalinity (total)`,
         chl_a = `Chlorophyll a`,
         sulphate = Sulphate,
         SRP = `Phosphate (ortho)`,
         TP = `Phosphate (total)`,
         colour = `Colour (Apparent)`,
         conductivity = Conductivity,
         hardness = `Hardness (total)`,
         turbidity = Turbidity,
         temperature = Temperature,
         date_ymd = datetime_ymd.hms) %>% 
  select(date_ymd, DOC, pH, SUVA, conductivity, sulphate, chl_a, TP, SRP,
         colour, turbidity, alkalinity, hardness, temperature) %>% 
  mutate(year = year(date_ymd),
         DOY = as.numeric(format(date_ymd, '%j'))) %>% 
  select(date_ymd, year, DOY, everything()) 
  # select(-c(alkalinity, hardness, SUVA, turbidity, temperature)) 

write_csv(BP_wide, "./anthony_masters/plsc-837/data/bp_longterm.csv")

BP_wide <- BP_wide %>% filter(date_ymd >= "1983-05-01")

BP_wide %>% 
  select(-c(year, DOY)) %>%
  summarise(DOC_mean = mean(DOC, na.rm = TRUE),
            DOC_sd = sd(DOC, na.rm = TRUE),
            DOC_min = min(DOC, na.rm = TRUE),
            DOC_max = max(DOC, na.rm = TRUE))
