library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)
library(zoo)

# ignore warnings
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-ammonia-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-nitrate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-organic-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-TP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-SRP.R")

mg_conc_lab <- expression(paste("Analyte concentration (mg or µg L"^-1*")"))
ug_conc_lab <- expression(paste("Analyte concentration (µg L"^-1*")"))

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
NH3_lab <- expression(paste("NH"[3]*" (mg N L"^-1*")")) 
NO3_lab <- expression(paste("NO"[3]*" (mg L"^-1*")")) 

TP_lab <- expression(paste("TP (µg L"^-1*")")) 
SRP_lab <- expression(paste("SRP (µg L"^-1*")")) 

bp_doc

bp_ammonia_clean <- bp_ammonia_monthly()
bp_DOC_clean <- bp_DOC_monthly()
bp_nitrate_clean <- bp_nitrate_monthly()
bp_organicN_clean <- bp_organicN_monthly()
bp_sulphate_clean <- bp_sulphate_monthly() 
bp_TP_clean <- bp_TP_monthly()
bp_SRP_clean <- bp_SRP_monthly()

bp_DOC_weekly <- DOC_complete_1990_2019()

analytes_wide <- bp_DOC_clean %>% 
  right_join(bp_ammonia_clean) %>% 
  right_join(bp_nitrate_clean) %>% 
  right_join(bp_organicN_clean) %>% 
  right_join(bp_sulphate_clean) %>% 
  right_join(bp_TP_clean) %>% 
  right_join(bp_SRP_clean)

parms <- tibble::tribble(
  
  ~parameter, ~Analyte,
  "DOC_mg.L",    "Dissolved organic carbon",
  "NH3_mg.L",    "Ammonium-N",   
  "NO3_mg.L",    "Nitrate-N",
  "orgN_mg.L",   "Dissolved organic nitrogen",
  "SO4_mg.L",    "Sulphate",
  "TP_ug.L",     "Total phosphorus",
  "SRP_ug.L",    "Soluble reactive phosphorus"
  
)

fct_order <- c("Dissolved organic carbon", 
               "Sulphate",
               "Total phosphorus",
               "Soluble reactive phosphorus",
               "Ammonium-N",
               "Nitrate-N",
               "Dissolved organic nitrogen")

analytes_long <- analytes_wide %>% 
  pivot_longer(cols = -c(date_ymd, year, month), 
               names_to = "parameter", values_to = "result") %>% 
  full_join(parms) %>% 
  mutate(Analyte = fct_relevel(Analyte, fct_order))

p_analytes <- analytes_long %>% 
  ggplot(aes(date_ymd, result, col = Analyte)) + 
  facet_wrap(~ Analyte, scales = "free", ncol = 1) +
  geom_line(size = 1.25) + 
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(x = "Year", y = mg_conc_lab) +
  scale_color_viridis_d(option = "viridis")

ggsave("./R_bpwtp/outputs/20220601-p_analytes.png", p_analytes, dpi = 300, width = 7.5, height = 8.38)

# 898 796


# option = c(magma, inferno, plasma, viridis)


analytes_long %>% 
  filter(parameter %in% c("DOC_mg.L", "SO4_mg.L")) %>% 
  group_by(parameter) %>% 
  mutate(ma_5year = rollmean(result, k = 60, fill = NA)) %>% 
  filter(!is.na(ma_5year)) %>% 
  ggplot(aes(date_ymd, ma_5year)) + 
  facet_wrap(~ Analyte, ncol = 1, scales = "free_y") +
  geom_line(size = 1) +
  geom_line(data = analytes_long %>% filter(parameter %in% c("DOC_mg.L", "SO4_mg.L")),
            aes(date_ymd, result), size = 1, col = 'steelblue', alpha = 1/2) + 
  labs(x = "Year", y = mg_conc_lab, 
       subtitle = "5-year moving averages of DOC and sulphate concentrations")
  
  
analytes_long %>% 
  filter(parameter %in% c("DOC_mg.L")) %>% 
  # group_by(parameter) %>% 
  mutate(ma_5year = rollmean(result, k = 60, fill = NA))


bp_DOC_weekly %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, nrow = 3) + 
  # geom_point() +
  geom_line(col = "steelblue", size = 1) +
  labs(y = DOC_lab, x = "Day of year",
       subtitle = "Weekly DOC concentrations")

bp_DOC_clean %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, nrow = 3) + 
  # geom_point() +
  geom_line(col = "steelblue", size = 1) +
  labs(y = DOC_lab, x = "Day of year",
       subtitle = "Monthly DOC concentrations")


bp_DOC_weekly1 %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, nrow = 3) + 
  geom_hline(yintercept = 5.5, col = "red", size = 0.75, alpha = 3/5) +
  geom_hline(yintercept = 7.8, col = "red", size = 0.75, alpha = 3/5) +
  geom_hline(yintercept = median(bp_DOC_weekly$DOC_mg.L), size = 0.75, alpha = 3/5) + 
  geom_line(col = "steelblue", size = 1) +
  labs(x = "Day of Year", y = DOC_lab)

bp_DOC_clean %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, nrow = 3) + 
  geom_hline(yintercept = 5.495, col = "red", size = 0.75, alpha = 3/5) +
  geom_hline(yintercept = 7.861, col = "red", size = 0.75, alpha = 3/5) +
  geom_hline(yintercept = median(bp_DOC_clean$DOC_mg.L), size = 0.75, alpha = 3/5) + 
  geom_line(col = "steelblue", size = 1) +
  labs(x = "Day of Year", y = DOC_lab)

bp_DOC_clean %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.5) + 
  scale_color_viridis_c(option = 'plasma')

bp_DOC_weekly %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.5) + 
  scale_color_viridis_c(option = 'plasma')


bp_DOC_weekly %>% 
  mutate(ptrend = ifelse(year %in% c(1990:1999), 1,
                         ifelse(year %in% c(2000:2009), 2, 3))) %>% 
  group_by(ptrend) %>% 
  summarise(pmedian = median(DOC_mg.L),
            pmean = mean(DOC_mg.L),
            psd = sd(DOC_mg.L),
            pmin = min(DOC_mg.L),
            pmax = max(DOC_mg.L),
            pn = n()) %>% 
  mutate(prange = pmax - pmin) %>% 
  select(ptrend:pmax, prange, pn)



