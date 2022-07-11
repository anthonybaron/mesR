library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(rstatix)

theme_set(theme_bw(base_size = 14) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
eems <- bp_doc_eems()

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
SUVA_lab <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))
BA_lab <- expression(paste("Freshness index (", italic(β), ":", italic(α), ")"))
S_lab <- "<i>S</i><sub>275–295</sub>"
HIX_lab <- "Humification index"
FI_lab <- "Fluorescence index"
dist_lab <- "Distance from Buffalo Pound Lake inflow (km)"


# Are inflow sites statistically different? -------------------------------

inflow <- eems %>% filter(grepl("Inflow", site_code_long))

inflow %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) + 
  # facet_wrap(~ Year) +
  geom_boxplot()

inflow %>% group_by(site_code_long) %>% summarise(n = n())

iaov <- anova_test(DOC_mg.L ~ site_code_long, data = inflow)
# F(2, 52) = 0.126, p = 0.882 // inflow sites are not significantly different 


# Are causeway upstream sites statistically different?  -------------------

us_causeway <- eems %>% filter(grepl("Upstream Causeway", site_code_long)) 

us_causeway %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) + 
  # facet_wrap(~ Year) + 
  geom_boxplot()

us_causeway %>% group_by(site_code_long) %>% summarise(n = n())

ccc <- us_causeway %>% filter(site_code_long == "Upstream Causeway Centre") %>% select(DOC_mg.L)
cce <- us_causeway %>% filter(site_code_long == "Upstream Causeway East") %>% select(DOC_mg.L)

t.test(ccc, cce, alternative = "two.sided", var.equal = FALSE)
# upstream causeway sites are not significantly different

lake_inflow <- inflow %>%
  group_by(date_ymd) %>% 
  summarise(across(TDN_mg.L:ext_coeff_m, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(site_name = "1.5 km below Qu'Appelle R. Inflow Centre",
         site_code_long = as.factor("Lake Inflow"),
         site_abbr = as.factor("LI"),
         Year = as.factor(year(date_ymd)), 
         Month = month(date_ymd, label = TRUE, abbr = TRUE), 
         DOY = yday(date_ymd), 
         latitude = 50.722927,
         longitude = -105.605669,
         distHaversine_km = 0) %>% 
  select(site_name, site_code_long, site_abbr, date_ymd, Year:distHaversine_km, everything())

upstream_causeway <- us_causeway %>% 
  group_by(date_ymd) %>% 
  summarise(across(TDN_mg.L:ext_coeff_m, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(site_name = "Upstream Causeway",
         site_code_long = as.factor("Upstream Causeway"),
         site_abbr = as.factor("CU"),
         Year = as.factor(year(date_ymd)), 
         Month = month(date_ymd, label = TRUE, abbr = TRUE), 
         DOY = yday(date_ymd), 
         latitude = 50.722927, 
         longitude = -105.605669,
         distHaversine_km = 2.18) %>% 
  select(site_name, site_code_long, site_abbr, date_ymd, Year:distHaversine_km, everything())


eems <- subset(eems, !grepl("Inflow", site_code_long))
eems <- subset(eems, !grepl("Upstream Causeway", site_code_long))
eems <- eems %>% bind_rows(lake_inflow) %>% bind_rows(upstream_causeway) %>% arrange(date_ymd)

site_codes <- tribble(
  ~site_code_long,            ~site_abbr,  
  "Lake Inflow",              "LI",
  "Upstream Causeway",        "CU",
  "Below Causeway",           "CB",
  "Opposite South Lake",      "SL",
  "Opposite Sun Valley",      "SV",
  "Opposite Parkview",        "PK", 
  "WTP Intake",               "TP",
  "Above Outlet",             "AO"  
)

site_codes_c <- site_codes[["site_code_long"]]
site_abbrs_c <- site_codes[["site_abbr"]]

eems <- eems %>% 
  mutate(site_code_long = forcats::fct_relevel(site_code_long, site_codes_c),
         site_abbr = forcats::fct_relevel(site_abbr, site_abbrs_c))
         
eems %>% 
  group_by(site_abbr, distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ggplot(aes(distHaversine_km, DOC_mg.L, col = site_abbr)) + 
  geom_point() + 
  scale_color_viridis_d(end = 0.8)


# Seasonal ANOVA ----------------------------------------------------------

ee_spring <- eems %>% filter(Month %in% c("Mar", "Apr", "May", "June")) # n = 61
ee_summer <- eems %>% filter(Month %in% c("Jul", "Aug", "Sep")) # n = 78

ee_seasons <- eems %>% mutate(Season = ifelse(Month %in% c("Mar", "Apr", "May", "June"), "Spring", "Summer"))
ee_seasons %>% 
  group_by(Season, distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>%  
  ggplot(aes(distHaversine_km, DOC_mg.L, col = Season)) + 
  # geom_line(size = 1) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 2) +  
  geom_smooth(method = 'lm', se = F, alpha = 1/4) + 
  lims(x = c(0, 30)) +
  labs(x = dist_lab, y = DOC_lab)

spr <- ee_seasons %>% filter(Season == "Spring") %>% select(DOC_mg.L)
sum <- ee_seasons %>% filter(Season == "Summer") %>% select(DOC_mg.L)

t.test(spr, sum, alternative = "two.sided", var.equal = FALSE)
# Spring and summer DOC concentrations are not significantly different

mean_spr <- ee_seasons %>% 
  filter(Season == "Spring") %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

mean_sum <- ee_seasons %>% 
  filter(Season == "Summer") %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

mspr <- lm(DOC_mg.L ~ distHaversine_km, data = mean_spr)
summary(mspr)
# R2 = 0.7232, p = 0.004608 // y = 0.014014x + 5.328478

msum <- lm(DOC_mg.L ~ distHaversine_km, data = mean_sum)
summary(msum)
# R2 = 0.9575, p = 1.536e-05 // y = 0.051766x + 4.835060


# Group sites -------------------------------------------------------------

lake_order <- c("Inflow", "Causeway", "Mid-lake", "Outlet")

eems <- eems %>% 
  mutate(lake_section = ifelse(site_num %in% c(1:3), "Inflow",
                               ifelse(site_num %in% c(4:6), "Causeway",
                                      ifelse(site_num %in% c(7:9), "Mid-lake", "Outlet"))),
         lake_section = factor(lake_section),
         lake_section = forcats::fct_relevel(lake_section, lake_order)) %>% 
  select(site_num, lake_section, everything())

eems %>%
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(site_code_long, DOC_mg.L, fill = lake_section)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot(alpha = 3/4) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = NULL, y = DOC_lab)

eems %>%
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(site_code_long, DOC_mg.L, fill = lake_section)) +
  # facet_wrap(~ Year, ncol = 4) +
  geom_boxplot(alpha = 9/10) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = NULL, y = DOC_lab)

eems %>%
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(lake_section, DOC_mg.L, fill = lake_section)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot(alpha = 9/10) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = NULL, y = DOC_lab)

eems %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(site_num, site_code_long, Year) %>% 
  summarise(obs_n = n()) %>% 
  mutate(lake_section = ifelse(site_num %in% c(1:3), "Inflow",
                               ifelse(site_num %in% c(4:6), "Causeway",
                                      ifelse(site_num %in% c(7:9), "Mid-lake", "Outlet"))),
         lake_section = factor(lake_section),
         lake_section = forcats::fct_relevel(lake_section, lake_order)) %>% 
  ggplot(aes(site_code_long, obs_n, fill = lake_section)) +
  facet_wrap(~ Year, nrow = 1) + 
  geom_col(alpha = 9/10) + 
  coord_flip() + 
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = NULL, y = "Number of observations")



# Summary stats -----------------------------------------------------------

# // DOC ------------------------------------------------------------------
eems %>% 
  # group_by(site_code_long, Year) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

eems %>% 
  filter(site_num %in% c(1:6)) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

eems %>% 
  filter(site_num %in% c(7:11)) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

eems %>% 
  filter(site_num %in% c(10)) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

eems %>% 
  filter(Month %in% c("Mar", "Apr", "May", "Jun")) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

eems %>% 
  filter(Month %in% c("Jul", "Aug", "Sep")) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

abbr_levs <- c('IE','IC','IW','CC','CE','CB','SL','SV','PK','TP','AO')

eems <- eems %>% mutate(site_abbr = ifelse(site_num == 1, "IE",
                                           ifelse(site_num == 2, "IC", 
                                                  ifelse(site_num == 3, "IW",
                                                         ifelse(site_num == 4, "CC",
                                                                ifelse(site_num == 5, "CE",
                                                                       ifelse(site_num == 6, "CB",
                                                                              ifelse(site_num == 7, "SL",
                                                                                     ifelse(site_num == 8, "SV",
                                                                                            ifelse(site_num == 9, "PK",
                                                                                                   ifelse(site_num == 10, "TP",
                                                                                                          ifelse(site_num == 11, "AO", site_abbr))))))))))),
                        site_abbr = as.factor(site_abbr),
                        site_abbr = fct_relevel(site_abbr, abbr_levs))


pppp <- eems %>% 
  rename(`Day of year` = DOY) %>% 
  ggplot(aes(site_num, DOC_mg.L, col = `Day of year`, group = `Day of year`)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom") +
  labs(x = "Site", y = DOC_lab)

eems %>% 
  rename(`Day of year` = DOY) %>% 
  ggplot(aes(site_abbr, DOC_mg.L, col = `Day of year`, group = `Day of year`)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  # scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
  #                    labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom") +
  labs(x = "Site", y = DOC_lab)

doc_mean <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE))

mmdoc <- lm(doc_mean$DOC_mean ~ doc_mean$distHaversine_km)
summary(mmdoc)
# R2 = 0.94, p = 4.5e-7

p_lm_doc <- doc_mean %>% 
  ggplot(aes(distHaversine_km, DOC_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  lims(x = c(0, 30)) +
  labs(x = NULL, y = DOC_lab)

(p_lm_doc + p_lm_suva) / (p_lm_s + p_lm_fi) / (p_lm_hix + p_lm_ba)


# // SUVA -----------------------------------------------------------------
eems %>% 
  # group_by(site_code_long, Year) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>% 
  filter(site_num %in% c(1:6)) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>% 
  filter(site_num %in% c(7:11)) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>% 
  filter(site_num %in% c(10)) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>% 
  filter(Month %in% c("Mar", "Apr", "May", "Jun")) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>% 
  filter(Month %in% c("Jul", "Aug", "Sep")) %>% 
  get_summary_stats(SUVA, type = "common")

eems %>%
  ggplot(aes(site_num, SUVA, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c()

eems %>% 
  mutate(DOY = as.factor(DOY)) %>% 
  ggplot(aes(DOY, SUVA)) + 
  facet_wrap(~ Year, scales = "free_x") + 
  geom_boxplot()


suva_mean <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(suva_mean = mean(SUVA, na.rm = TRUE))

mmsuva <- lm(suva_mean$suva_mean ~ suva_mean$distHaversine_km)
summary(mmsuva)
# R2 = 0.98, p = 1.56e-9

p_lm_suva <- suva_mean %>% 
  ggplot(aes(distHaversine_km, suva_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') + 
  lims(x = c(0, 30)) + 
  labs(x = NULL, y = SUVA_lab)


# // S275to295 ------------------------------------------------------------
eems %>% get_summary_stats(S275to295, type = 'common')

eems %>% 
  filter(site_num %in% c(1:6)) %>% 
  get_summary_stats(S275to295, type = "common")

eems %>% 
  filter(site_num %in% c(7:11)) %>% 
  get_summary_stats(S275to295, type = "common")

eems %>% 
  group_by(Month) %>% 
  get_summary_stats(S275to295, type = "common") 

eems %>% 
  group_by(site_code_long) %>% 
  get_summary_stats(S275to295, type = "common") 

eems %>% 
  group_by(site_code_long, Year) %>% 
  get_summary_stats(S275to295, type = "common") %>% 
  arrange(Year) %>% 
  filter(Year == 2019)

eems %>% 
  # group_by(site_code_long, date_ymd) %>% 
  # group_by(site_code_long, Year) %>% 
  # get_summary_stats(S275to295, type = "common") %>% 
  ggplot(aes(site_num, S275to295, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_point() +
  geom_line() +
  lims(x = c(0, 12))
# In all years S increased from inflow sites and peaked at the outlet site, 
# with average values reaching 0.025 in 2017, 2018, and 2019, and 0.023 in 2016.
# The highest mean S values were observed in 2019 where they ranged from 
# 0.023—0.025. 

eems %>% 
  ggplot(aes(site_num, S275to295, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c() +
  labs(y = S_lab) +
  theme(axis.title.y = ggtext::element_markdown())

s_mean <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(S_mean = mean(S275to295, na.rm = TRUE))

mms <- lm(s_mean$S_mean ~ s_mean$distHaversine_km)
summary(mms)
# R2 = 0.96, p = 4.75e-8

p_lm_s <- s_mean %>% 
  ggplot(aes(distHaversine_km, S_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  lims(x = c(0, 30)) +
  labs(x = NULL, y = S_lab) +
  theme(axis.title.y = ggtext::element_markdown())
  
eems %>% 
  mutate(DOY = as.factor(DOY)) %>% 
  ggplot(aes(DOY, S275to295)) + 
  facet_wrap(~ Year, scales = "free_x") + 
  geom_boxplot()



# // Fluorescence Index ---------------------------------------------------
eems %>%
  filter(FI < 1.69) %>%
  get_summary_stats(FI, type = 'common')
# mean = 1.56 ± 0.033 (1.48—1.66), N = 211

eems %>% 
  filter(FI < 1.69) %>% 
  filter(site_num %in% c(1:6)) %>% 
  get_summary_stats(FI, type = "common")
# mean = 1.54 ± 0.027 (1.48—1.61), N = 112

eems %>% 
  filter(FI < 1.69) %>% 
  filter(site_num %in% c(7:11)) %>% 
  get_summary_stats(FI, type = "common")
# mean = 1.58 ± 0.028 (1.51—1.66), N = 99

eems %>% 
  filter(FI < 1.69) %>% 
  group_by(Month) %>% 
  get_summary_stats(FI, type = "common")
# Obvious seasonal differences in FI values were absent; mean FI was highest in
# March and September (1.57), lowest in June (1.55), and 1.56 in April, May,
# July, and August.

eems %>% 
  filter(FI < 1.69) %>% 
  group_by(site_code_long) %>% 
  get_summary_stats(FI, type = "common")
# While seasonal patterns were not evident, mean FI values increased linearly
# with increasing distance from the lake inflow 1.54 at the five sites above the
# causeway to 1.59 at WTP Intake and 1.60 at Above Outlet (R2 = 0.94, p =
# 4.9e-7, Figure nD). Hansen et al. (2016) report increasing FI values with 
# microbial processing for plant and algae leachates that underwent an 111 day
# incubation; however the FI values in this study are lower than their FI values
# for peat soils (1.6-1.9). 

eems %>% 
  filter(FI < 1.69) %>% 
  group_by(site_code_long, Year) %>% 
  get_summary_stats(FI, type = "common") %>% filter(Year == 2019)
  
eems %>% 
  filter(FI < 1.69) %>% 
  group_by(Year) %>% 
  get_summary_stats(FI, type = "common")

eems %>% 
  ggplot(aes(site_num, FI, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c()

fi_mean <- eems %>% 
  filter(FI < 1.69) %>% 
  group_by(distHaversine_km) %>% 
  summarise(FI_mean = mean(FI, na.rm = TRUE))

mmfi <- lm(fi_mean$FI_mean ~ fi_mean$distHaversine_km)
summary(mmfi)
# R2 = 0.94, p = 4.9e-7

p_lm_fi <- fi_mean %>% 
  ggplot(aes(distHaversine_km, FI_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  lims(x = c(0, 30)) +
  labs(x = NULL, y = FI_lab)


# // Humification Index ---------------------------------------------------

eems %>% 
  ggplot(aes(site_num, HIX_Ohno, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c()

hix_mean <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(HIX_mean = mean(HIX_Ohno, na.rm = TRUE))

mmhix <- lm(hix_mean$HIX_mean ~ hix_mean$distHaversine_km)
summary(mmhix)
# R2 = 0.88, p = 1.08e-5

p_lm_hix <- hix_mean %>% 
  ggplot(aes(distHaversine_km, HIX_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') + 
  lims(x = c(0, 30)) +
  labs(x = dist_lab, y = HIX_lab)


# // Freshness Index (β:α) ------------------------------------------------

eems %>% 
  ggplot(aes(site_num, BA, col = DOY, group = DOY)) + 
  facet_wrap(~ Year) +
  geom_line(size = 0.75) +
  geom_point(col = "white", size = 2) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_viridis_c()

ba_mean <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(BA_mean = mean(BA, na.rm = TRUE))

mmba <- lm(ba_mean$BA_mean ~ ba_mean$distHaversine_km)
summary(mmba)
# R2 = 0.94, p = 7.54e-7

p_lm_ba <- ba_mean %>% 
  ggplot(aes(distHaversine_km, BA_mean)) + 
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') + 
  lims(x = c(0, 30)) +
  labs(x = dist_lab, y = BA_lab)


# // C:T ratio ------------------------------------------------------------

eems %>% 
  filter(!Year == 2019) %>% 
  ggplot(aes(site_num, CT_ratio, col = DOY, group = DOY)) + 
  # facet_wrap(~ Year, ncol = 1, scale = "free_y") + 
  geom_point() +
  geom_line() +
  lims(x = c(0, 12))


# // A:T ratio ------------------------------------------------------------

eems %>%
  filter(!Year == 2019) %>% 
  ggplot(aes(site_num, AT_ratio, col = DOY, group = DOY)) + 
  # facet_wrap(~ Year, ncol = 1, scale = "free_y") + 
  geom_point() +
  geom_line() +
  lims(x = c(0, 12))


# // C:A ratio ------------------------------------------------------------

eems %>%
  filter(!Year == 2019) %>% 
  ggplot(aes(site_num, CA_ratio, col = DOY, group = DOY)) + 
  # facet_wrap(~ Year, ncol = 1, scale = "free_y") + 
  geom_point() +
  geom_line() +
  lims(x = c(0, 12))


# // C:M ratio ------------------------------------------------------------

eems %>%
  filter(!Year == 2019) %>% 
  ggplot(aes(site_num, CM_ratio, col = DOY, group = DOY)) + 
  # facet_wrap(~ Year, ncol = 1, scale = "free_y") + 
  geom_point() +
  geom_line() +
  lims(x = c(0, 12))



# Some plots --------------------------------------------------------------

eems %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(Month, DOC_mg.L)) + 
  facet_wrap(~ Year) + 
  geom_boxplot()

eems %>% 
  filter(!is.na(SUVA)) %>% 
  ggplot(aes(Month, SUVA)) + 
  facet_wrap(~ Year) + 
  geom_boxplot()

eems %>% 
  filter(!is.na(S275to295)) %>% 
  ggplot(aes(Month, S275to295)) + 
  facet_wrap(~ Year) + 
  geom_boxplot()


eems %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(site_code_long, Year) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Year, values_from = n) %>% 
  ungroup() %>% 
  summarise(sum17 = sum(`2017`), 
            sum18 = sum(`2018`))

eems %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(site_code_long, Year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(n, site_code_long)) + 
  facet_wrap(~ Year, ncol = 1) + 
  geom_col(alpha = 3/4) +
  scale_y_discrete(limits = rev) +
  theme_bw(base_size = 11) +
  labs(y = NULL, x = "Number of DOC samples")

dd <- tribble(
  ~ydays,
  yday("2022-03-01"), # 60
  yday("2022-04-01"), # 91
  yday("2022-05-01"), # 121
  yday("2022-06-01"), # 152
  yday("2022-07-01"), # 182
  yday("2022-08-01"), # 213
  yday("2022-09-01"), # 244
  yday("2022-10-01")  # 274
)

tibble(date_ymd = unique(eems$date_ymd),
       doy = yday(date_ymd),
       year = year(date_ymd),
       val = ifelse(year == 2016, "2016",
                    ifelse(year == 2017, "2017",
                           ifelse(year == 2018, "2018", "2019")))) %>% 
  mutate(val = factor(val),
         year = factor(year)) %>% 
  ggplot(aes(doy, val, col = year)) + 
  geom_vline(xintercept = dd$ydays, lty = 2) + 
  geom_point(size = 3) + 
  geom_line(size = 1) +
  scale_y_discrete(limits = rev) +
  xlim(c(1, 366)) +
  theme(legend.position = 'none') +
  labs(x = "Day of year", y = NULL)


eems %>% 
  pivot_longer(cols = c(TDN_mg.L:ext_coeff_m),
               names_to = "parameter",
               values_to = "result") %>% 
  group_by(parameter) %>% 
  filter(!is.na(result)) %>% 
  summarize(n = n(),
            min = min(result),
            max = max(result),
            q1 = quantile(result, 0.25),
            q3 = quantile(result, 0.75),
            iqr = IQR(result),
            median = median(result),
            mean = mean(result),
            sd = sd(result),
            se = sd / sqrt(n),
            ci = mean - (1.96*sd))

summary_all_eems <- eems %>% 
  select(-c(site_num:distHaversine_km)) %>% 
  get_summary_stats(type = 'full')

summary_select_eems <- eems %>% 
  select(-c(site_num:distHaversine_km)) %>%
  select(-c(A280, A350, A440, S350to400)) %>% 
  get_summary_stats(type = 'full') 

write_csv(summary_select_eems, "./R_EEMs/outputs/data/eems-summary-stats.csv")

summary_select_eems %>% filter(variable == "S275to295") 


# Correlation matrix ------------------------------------------------------

dat<- matrix(rnorm(50), nrow=10, ncol=5)
set.seed (877)
naInd<- sample(1:length(dat), 10)
dat[naInd]<- NA
colnames(dat)<- paste("col", 1:ncol(dat), sep="")
rownames(dat)<- paste("row", 1:nrow(dat), sep="")
dat
cor(dat)
as.data.frame(na.omit(dat))
cor(na.omit(dat))

myCorDat<- mycor(dat, method="pearson", na.action=na.omit)


mycor <- function(x, ...) {
  
  r<- apply(x, 2, function(j) {
    apply(x, 2, function(i) {
      as.numeric(cor.test(i, j, ...)$estimate)
    })
  })
  P<- apply(x, 2, function(j){
    apply(x, 2, function(i){
      as.numeric(cor.test(i, j, ...)$p.value)
    })
  })
  out <- c()
  out$P <- P
  out$r <- r
  return(out) 
  
}

ee <- eems %>% 
  select(-c(site_num:distHaversine_km)) %>%
  select(-c(A280, A350, A440, S350to400, SR, HIX, Fmax)) %>% 
  mutate(spA = PeakA_RU / DOC_mg.L,
         spB = PeakB_RU / DOC_mg.L,
         spC = PeakC_RU / DOC_mg.L,
         spD = PeakD_RU / DOC_mg.L,
         spE = PeakE_RU / DOC_mg.L,
         spM = PeakM_RU / DOC_mg.L,
         spN = PeakN_RU / DOC_mg.L,
         spP = PeakP_RU / DOC_mg.L,
         spT = PeakT_RU / DOC_mg.L)

eem <- as.matrix(ee)

eem_corr <- Hmisc::rcorr(eem, type = "pearson")

corrplot::corrplot(corr = eem_corr$r, p.mat = eem_corr$P,
                   type = "full", 
                   insig = "pch",
                   sig.level = 0.05,
                   pch.cex = 0.9)

View(eem_corr)

eem_corr_rvals <- as_tibble(eem_corr[["r"]])




# ANOVAs ------------------------------------------------------------------

# https://www.datanovia.com/en/lessons/anova-in-r/
# https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/


# Basic one-way ANOVA -----------------------------------------------------

eems %>% 
  ggplot(aes(x = DOC_mg.L)) +
  geom_histogram(binwidth = 0.2, col = "grey70") + 
  facet_wrap(~ lake_section) +
  labs(x = DOC_lab)

library(tidyverse)
library(ggpubr)
library(rstatix)

eems %>% 
  group_by(site_code_long, Year) %>% 
  get_summary_stats(DOC_mg.L, type = "mean_sd")

eems %>% 
  select(site_code_long, Year, DOC_mg.L) %>% 
  group_by(site_code_long, Year) %>% 
  identify_outliers(DOC_mg.L)

model <- lm(DOC_mg.L ~ site_code_long, data = eems)
ggqqplot(residuals(model))
shapiro_test(residuals(model))

eems %>% 
  select(site_code_long, Year, DOC_mg.L) %>% 
  group_by(site_code_long, Year) %>% 
  shapiro_test(DOC_mg.L) %>% View()

ggqqplot(eems, "DOC_mg.L", facet.by = "site_code_long")

plot(model, 1)

eems %>% levene_test(DOC_mg.L ~ site_code_long)

prep_rm <- function(df = eems, year = year) {
  
  df <- df %>% 
    filter(Year == year) %>% 
    select(lake_section, site_code_long, date_ymd, DOC_mg.L) %>% 
    mutate(date_ymd = factor(date_ymd)) %>% 
    distinct()
  
}

eems16 <- prep_rm(eems, 2016)
eems17 <- prep_rm(eems, 2017)
eems18 <- prep_rm(eems, 2018) 
eems19 <- prep_rm(eems, 2019)

res_aov16 <- eems16 %>% anova_test(DOC_mg.L ~ site_code_long)
res_aov17 <- eems17 %>% anova_test(DOC_mg.L ~ site_code_long)
res_aov18 <- eems18 %>% anova_test(DOC_mg.L ~ site_code_long)
res_aov19 <- eems19 %>% anova_test(DOC_mg.L ~ site_code_long)

res_aov16 # F(10, 42) = 1.469, p = 0.185, ges = 0.26
res_aov17 # F(10, 50) = 2.903, p = 0.006, ges = 0.37
res_aov18 # F(10, 46) = 2.675, p = 0.011, ges = 0.37
res_aov19 # F(10, 55) = 1.718, p = 0.1,   ges = 0.24

pwc17 <- eems17 %>% tukey_hsd(DOC_mg.L ~ site_code_long)
pwc18 <- eems18 %>% tukey_hsd(DOC_mg.L ~ site_code_long)

pwc17 
pwc18 


# Repeated measures one-way ANOVA -----------------------------------------

### 2016 ###

eems16 %>% 
  group_by(site_code_long) %>% 
  get_summary_stats(type = "mean_sd")

# no extreme outliers
eems16 %>% 
  group_by(site_code_long) %>% 
  identify_outliers(DOC_mg.L)

# not normal; try Friedman Test 
eems16 %>% 
  group_by(site_code_long) %>% 
  shapiro_test(DOC_mg.L)

rmaov16 <- anova_test(data = eems16, dv = DOC_mg.L, wid = site_code_long, within = date_ymd)

get_anova_table(rmaov16)
# F(1.25, 11.21) = 10.122, p = 0.006, generalized effect size (ges) = 0.364

# post-hoc Bonferroni -- doesn't work with a missing observation 
pwc16 <- eems16 %>% 
  pairwise_t_test(DOC_mg.L ~ date_ymd, paired = TRUE, p.adjust.method = "bonferroni")


### 2017 ###

eems17 %>% 
  group_by(date_ymd) %>% 
  get_summary_stats(type = "mean_sd")

# no extreme outliers
eems17 %>% 
  group_by(date_ymd) %>% 
  identify_outliers(DOC_mg.L)

# not normal; try Friedman Test 
eems17 %>% 
  group_by(date_ymd) %>% 
  shapiro_test(DOC_mg.L)

rmaov17 <- anova_test(data = eems17, dv = DOC_mg.L, wid = site_code_long, within = date_ymd)

get_anova_table(rmaov17)
# F(5, 25) = 3.268, p = 0.021, generalized effect size (ges) = 0.262

# post-hoc Bonferroni -- doesn't work with too few observations or missing values
pwc17 <- eems17 %>% 
  pairwise_t_test(DOC_mg.L ~ date_ymd, paired = TRUE, p.adjust.method = "bonferroni")

### 2018 ###

eems18 %>% 
  group_by(date_ymd) %>% 
  get_summary_stats(type = "mean_sd")

# one extreme outliers
eems18 %>% 
  group_by(date_ymd) %>% 
  identify_outliers(DOC_mg.L)

# not normal; try Friedman Test 
eems18 %>% 
  group_by(date_ymd) %>% 
  shapiro_test(DOC_mg.L)

rmaov18 <- anova_test(data = eems18, dv = DOC_mg.L, wid = site_code_long, within = date_ymd)

get_anova_table(rmaov18)
# F(5, 20) = 0.676, p = 0.647, generalized effect size (ges) = 0.101

# post-hoc Bonferroni -- doesn't work when groups have different lengths
pwc18 <- eems18 %>% 
  pairwise_t_test(DOC_mg.L ~ date_ymd, paired = TRUE, p.adjust.method = "bonferroni")


### 2019 ###

eems19 %>% 
  group_by(site_code_long) %>% 
  get_summary_stats(type = "mean_sd")

# one extreme outlier
eems19 %>% 
  group_by(site_code_long) %>% 
  identify_outliers(DOC_mg.L)

# normality met
eems19 %>% 
  group_by(site_code_long) %>% 
  shapiro_test(DOC_mg.L)

rmaov19 <- anova_test(data = eems19, dv = DOC_mg.L, wid = date_ymd, within = site_code_long)

get_anova_table(rmaov19)
# F(2.52, 25.25) = 10.293, p = 0.000245, generalized effect size (ges) = 0.386

# post-hoc Bonferroni -- doesn't work when groups have different lengths
pwc19 <- eems19 %>% 
  pairwise_t_test(DOC_mg.L ~ site_code_long, paired = TRUE, p.adjust.method = "bonferroni")

bxp19 <- ggboxplot(eems19, x = "site_code_long", y = "DOC_mg.L", add = "point")

pwc19 <- pwc19 %>% add_xy_position(x = "date_ymd")

bxp19 + 
  stat_pvalue_manual(pwc19, hide.ns = FALSE) + 
  labs(subtitle = get_test_label(rmaov19, detail = TRUE),
       caption = get_pwc_label(pwc19),
       x = NULL, 
       y = DOC_lab) +
  scale_x_discrete(limits = rev) +
  coord_flip() 


# Friedman Test -----------------------------------------------------------

### 2016 ###

eems16 %>% 
  group_by(date_ymd) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

fried16 <- eems16 %>% friedman_test(DOC_mg.L ~ date_ymd | site_code_long)

### 2017 ###

eems17 %>% 
  group_by(date_ymd) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

### 2018 ###

eems18 %>% 
  group_by(date_ymd) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

### 2019 ###

eems19 %>% 
  group_by(site_code_long) %>% 
  get_summary_stats(DOC_mg.L, type = "common")

fried19 <- eems19 %>% friedman_test(DOC_mg.L ~ site_code_long | date_ymd)
# n = 6 sites, X^2(10) = 21.7, p = 0.0166

effried19 <- eems19 %>% friedman_effsize(DOC_mg.L ~ site_code_long | date_ymd)
# Kendall's W coefficient = 0.362 (moderate effect)

# Wilcoxon signed-rank test for multiple pariwise comparisons
wpwc19 <- eems19 %>% wilcox_test(DOC_mg.L ~ site_code_long, paired = TRUE, p.adjust.method = "BY")

wpwc19 <- wpwc19 %>% add_xy_position(x = "site_code_long")

bxp19 +
  stat_pvalue_manual(wpwc19, hide.ns = TRUE) + 
  labs(subtitle = get_test_label(fried19, detailed = TRUE),
       caption = get_pwc_label(wpwc19),
       x = NULL,
       y = DOC_lab) +
  scale_x_discrete(limits = rev) +
  coord_flip() 



# Kruskal-Wallis Test -----------------------------------------------------

# The effect size (eta^2) is based on the Kruskal-Wallis H statistic.
# eta^2[H] = (H - k + 1)/(n - k)
# where H = stat, k = number of groups, n = N.
# The effect size indicates the percentage of variance in the dependent var 
# explained by the independent var.
# 0.01 to < 0.06 = small effect
# 0.06 to < 0.14 = moderate effect
# >= 0.14 = large effect

eems16 %>% kruskal_test(DOC_mg.L ~ site_code_long)
# n = 32, H(10) = 13.4, p = 0.203 -- not significant
eems16 %>% kruskal_effsize(DOC_mg.L ~ site_code_long)
# eta^2[H] = 0.161 (large effect)

eems17 %>% kruskal_test(DOC_mg.L ~ site_code_long)
# n = 61, H(10) = 23.8, p = 0.00822 -- significant at 0.99
eems17 %>% kruskal_effsize(DOC_mg.L ~ site_code_long)
# eta^2[H] = 0.275 (large effect)

eems18 %>% kruskal_test(DOC_mg.L ~ site_code_long)
# n = 57, H(10) = 20.6, 0.0243 -- significant at 0.95
eems18 %>% kruskal_effsize(DOC_mg.L ~ site_code_long)
# eta^2[H] = 0.230 (large effect)

eems19 %>% kruskal_test(DOC_mg.L ~ site_code_long)
# n = 66, H(10) = 7.99, p = 0.175 -- not significant
eems19 %>% kruskal_effsize(DOC_mg.L ~ site_code_long)
# eta^2[H] = 0.0719 (moderate effect)

# Multiple pairwise comparisons 
# Use Dunn's test to identify which groups are different.
# Can also use Wolcoxon's test to calculate pairwise comparisons between
# groups levels with corrections for multiple testing. 
# Compared to Wilcoxon's test, Dunn's test takes into account the rankings used
# by the Kruskwal-Wallis test and adjusts for ties. 

# Dunn Test
pwc_kw17 <- eems17 %>% dunn_test(DOC_mg.L ~ site_code_long, p.adjust.method = "BY")
pwc_kw18 <- eems18 %>% dunn_test(DOC_mg.L ~ site_code_long, p.adjust.method = "BY")

pwc_kw17 %>% arrange(p.adj)
pwc_kw18 %>% arrange(p.adj)

# Wilcoxon Test
pwc2_kw17 <- eems17 %>% wilcox_test(DOC_mg.L ~ site_code_long, p.adjust.method = "BY")
pwc2_kw18 <- eems18 %>% wilcox_test(DOC_mg.L ~ site_code_long, p.adjust.method = "BY")

pwc2_kw17 %>% arrange(p.adj)
pwc2_kw18 %>% arrange(p.adj)



# Kruskal-Wallis with lake sections ---------------------------------------

### Summary stats

eems16 %>% group_by(lake_section) %>% get_summary_stats(DOC_mg.L, type = "common")
# lake_section variable     n   min   max median   iqr  mean    sd    se    ci
# <fct>        <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 Inflow     DOC_mg.L     9  4.09  6.58   6.20 2.33   5.66 1.12  0.374 0.863
# 2 Causeway   DOC_mg.L     9  4.80  7.12   6.24 1.04   6.09 0.734 0.245 0.564
# 3 Mid-lake   DOC_mg.L     8  6.25  7.12   6.74 0.185  6.72 0.254 0.09  0.213
# 4 Outlet     DOC_mg.L     6  6.51  6.97   6.65 0.087  6.70 0.157 0.064 0.164

eems17 %>% group_by(lake_section) %>% get_summary_stats(DOC_mg.L, type = "common")
# lake_section variable     n   min   max median   iqr  mean    sd    se    ci
# <fct>        <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 Inflow     DOC_mg.L    15  4.55  7.97   5.18 0.784  5.52 1.08  0.279 0.599
# 2 Causeway   DOC_mg.L    17  4.86  7.82   5.50 1.33   5.67 0.795 0.193 0.409
# 3 Mid-lake   DOC_mg.L    18  4.85  6.95   6.32 0.967  6.12 0.612 0.144 0.304
# 4 Outlet     DOC_mg.L    11  5.74  8.13   7.02 0.422  7.02 0.596 0.18  0.4

eems18 %>% group_by(lake_section) %>% get_summary_stats(DOC_mg.L, type = "common")
# lake_section variable     n   min   max median   iqr  mean    sd    se    ci
# <fct>        <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 Inflow     DOC_mg.L    13  3.86  5.66   4.29 0.549  4.47 0.552 0.153 0.334
# 2 Causeway   DOC_mg.L    15  3.92  5.30   4.43 0.484  4.49 0.426 0.11  0.236
# 3 Mid-lake   DOC_mg.L    17  4.33  5.43   4.97 0.343  4.93 0.325 0.079 0.167
# 4 Outlet     DOC_mg.L    12  4.23  6.50   5.29 0.839  5.24 0.696 0.201 0.442

eems19 %>% group_by(lake_section) %>% get_summary_stats(DOC_mg.L, type = "common")
# lake_section variable     n   min   max median   iqr  mean    sd    se    ci
# <fct>        <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 Inflow     DOC_mg.L    18   4     5.9   4.6   1.42  4.83 0.706 0.166 0.351
# 2 Causeway   DOC_mg.L    18   3.7   5     4.6   0.4   4.52 0.389 0.092 0.193
# 3 Mid-lake   DOC_mg.L    18   4.2   6.3   4.95  0.35  5.03 0.517 0.122 0.257
# 4 Outlet     DOC_mg.L    12   4.2   6.4   5.35  0.45  5.31 0.62  0.179 0.394


### Outliers

eems16 %>% group_by(lake_section) %>% identify_outliers(DOC_mg.L) # no extreme outliers
eems17 %>% group_by(lake_section) %>% identify_outliers(DOC_mg.L) # one extreme outlier
eems18 %>% group_by(lake_section) %>% identify_outliers(DOC_mg.L) # no extreme outliers
eems19 %>% group_by(lake_section) %>% identify_outliers(DOC_mg.L) # one extreme outliers


### Normality

m16 <- lm(DOC_mg.L ~ lake_section, data = eems16)
ggqqplot(residuals(m16)) # not great on bottom tail
shapiro_test(residuals(m16)) # p = 0.0229 (normality not true)

m17 <- lm(DOC_mg.L ~ lake_section, data = eems17)
ggqqplot(residuals(m17)) # not great on top tail
shapiro_test(residuals(m17)) # p = 0.00155 (normality not true)

m18 <- lm(DOC_mg.L ~ lake_section, data = eems18)
ggqqplot(residuals(m18)) # not great on top tail
shapiro_test(residuals(m18)) # p = 0.0721 (~ normal)

m19 <- lm(DOC_mg.L ~ lake_section, data = eems19)
ggqqplot(residuals(m19)) # looks decent
shapiro_test(residuals(m19)) # p = 0.260 (~ normal)


### Homogeneity of variance
eems16 %>% levene_test(DOC_mg.L ~ lake_section) # p = 0.0460, close to equal variance
eems17 %>% levene_test(DOC_mg.L ~ lake_section) # p = 0.574, equal variance
eems18 %>% levene_test(DOC_mg.L ~ lake_section) # p = 0.164, equal variance
eems19 %>% levene_test(DOC_mg.L ~ lake_section) # p = 0.0741, equal variance


### Assumptions summary

# 2016 --- no extreme outliers, data not normal and heteroscedastic
# 2017 --- one extreme outlier, data not normal but have equal variance
# 2018 --- no extreme outliers, data approach normality and have equal variance
# 2019 --- one extreme outlier, data approach normality and have equal variance

# Because 2016, 2017, and 2019 do not meet one or more of the assumptions of the 
# one-way ANOVA, and because group sizes are unequal, I will instead use the
# non-parametric Kruskwal-Wallis test. 

### Kruskal-Wallis tests

eems16 %>% kruskal_test(DOC_mg.L ~ lake_section)
# n = 32, H(3) = 12.1, p = 0.00701 -- significant at 0.99
eems16 %>% kruskal_effsize(DOC_mg.L ~ lake_section)
# eta^2[H] = 0.325 (large effect)

eems17 %>% kruskal_test(DOC_mg.L ~ lake_section)
# n = 61, H(3) = 21.4, p = 0.0000876 -- significant at 0.9999
eems17 %>% kruskal_effsize(DOC_mg.L ~ lake_section)
# eta^2[H] = 0.323 (large effect)

eems18 %>% kruskal_test(DOC_mg.L ~ lake_section)
# n = 57, H(3) = 16.6, p = 0.000868 -- significant at 0.999
eems18 %>% kruskal_effsize(DOC_mg.L ~ lake_section)
# eta^2[H] = 0.256 (large effect)

eems19 %>% kruskal_test(DOC_mg.L ~ lake_section)
# n = 66, H(3) = 12.7, p = 0.00532 -- significant at 0.99
eems19 %>% kruskal_effsize(DOC_mg.L ~ lake_section)
# eta^2[H] = 0.157 (large effect)

### Multiple pairwise comparisons

# Use Dunn's test to identify which groups are different.
# Can also use Wolcoxon's test to calculate pairwise comparisons between groups 
# levels with corrections for multiple testing. 
# Compared to Wilcoxon's test, Dunn's test takes into account the rankings used
# by the Kruskwal-Wallis test and adjusts for ties. 

## Dunn Test
pwc3_kw16 <- eems16 %>% dunn_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc3_kw17 <- eems17 %>% dunn_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc3_kw18 <- eems18 %>% dunn_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc3_kw19 <- eems19 %>% dunn_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")

pwc3_kw16 %>% arrange(p.adj)
# Inflow and Mid-lake sig diff: p.adj = 0.0357

pwc3_kw17 %>% arrange(p.adj)
# Inflow and Outlet sig diff: p.adj = 0.000307
# Causeway and Outlet sig diff: p.adj = 0.00110

pwc3_kw18 %>% arrange(p.adj)
# Inflow and Outlet sig diff: p.adj = 0.0173
# Causeway and Outlet sig diff: p.adj = 0.0173
# Inflow and Mid-lake sig diff: p.adj = 0.0353

pwc3_kw19 %>% arrange(p.adj)
# Causeway and Outlet sig diff: p.adj = 0.0169

## Wilcoxon Test
pwc4_kw16 <- eems16 %>% wilcox_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc4_kw17 <- eems17 %>% wilcox_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc4_kw18 <- eems18 %>% wilcox_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")
pwc4_kw19 <- eems19 %>% wilcox_test(DOC_mg.L ~ lake_section, p.adjust.method = "BY")

pwc4_kw16 %>% arrange(p.adj)
# Inflow and Mid-lake sig diff: p.adj = 0.027
# Inflow nad Outlet sig diff: p.adj = 0.027

pwc4_kw17 %>% arrange(p.adj)
# Causeway and Outlet sig diff: p.adj = 0.000917
# Mid-lake and Outlet sig diff: p.adj = 0.001
# Inflow and Outlet sig diff: p.adj = 0.01
# Inflow and Mid-lake sig diff: p.adj = 0.049

pwc4_kw18 %>% arrange(p.adj)
# Inflow and Mid-lake sig diff: p.adj = 0.022
# Inflow nad Outlet sig diff: p.adj = 0.022
# Causeway and Mid-lake sig diff: p.adj = 0.022
# Causeway and Outlet sig diff: p.adj = 0.022

pwc4_kw19 %>% arrange(p.adj)
# Causeway and Mid-lake sig diff: p.adj = 0.017
# Causeway and Outlet sig diff: p.adj = 0.017

### Plot pairwise comparisons

# 2016
pwc3_kw16 <- pwc3_kw16 %>% add_xy_position(x = "lake_section") 
p_kwd16 <- eems16 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc3_kw16, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis test Dunn pairwise comparisons (2016)")

pwc4_kw16 <- pwc4_kw16 %>% add_xy_position(x = "lake_section") 
p_kww16 <- eems16 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc4_kw16, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Wilcoxon pairwise comparisons (2016)")

# 2017
pwc3_kw17 <- pwc3_kw17 %>% add_xy_position(x = "lake_section") 
p_kwd17 <- eems17 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc3_kw17, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Dunn pairwise comparisons (2017)")

pwc4_kw17 <- pwc4_kw17 %>% add_xy_position(x = "lake_section") 
p_kww17 <- eems17 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc4_kw17, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Wilcoxon pairwise comparisons (2017)")

# 2018
pwc3_kw18 <- pwc3_kw18 %>% add_xy_position(x = "lake_section") 
p_kwd18 <- eems18 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc3_kw18, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Dunn pairwise comparisons (2018)")

pwc4_kw18 <- pwc4_kw18 %>% add_xy_position(x = "lake_section") 
p_kww18 <- eems18 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc4_kw18, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Wilcoxon pairwise comparisons (2018)")

# 2019
pwc3_kw19 <- pwc3_kw19 %>% add_xy_position(x = "lake_section") 
p_kwd19 <- eems19 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc3_kw19, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Dunn pairwise comparisons (2019)")

pwc4_kw19 <- pwc4_kw19 %>% add_xy_position(x = "lake_section") 
p_kww19 <- eems19 %>% 
  ggboxplot("lake_section", "DOC_mg.L") +
  stat_pvalue_manual(pwc4_kw19, hide.ns = TRUE) + 
  labs(x = "Lake section", y = DOC_lab,
       title = "Kruskwall-Wallis Test with Wilcoxon pairwise comparisons (2019)")


p_kwd16 + p_kwd17 + p_kwd18 + p_kwd19
p_kww16 + p_kww17 + p_kww18 + p_kww19


eems %>% 
  ggplot(aes(date_ymd, chla_ug.L)) + 
  geom_point()

eems %>% 
  select(-Month) %>% 
  filter(!is.na(chla_ug.L)) %>% 
  rename(Month = date_ymd) %>% 
  ggplot(aes(lake_section, chla_ug.L)) +
  geom_boxplot() +
  geom_point(aes(col = Month)) +
  labs(x = "Lake section",
       y = "Chl a (µg/L)",
       subtitle = "2019 Chl a concentrations")

eems %>% 
  select(-Month) %>% 
  filter(!is.na(ext_coeff_m)) %>% 
  rename(Month = date_ymd) %>% 
  ggplot(aes(lake_section, ext_coeff_m)) +
  geom_boxplot() +
  geom_point(aes(col = Month)) +
  labs(x = "Lake section",
       y = "Extinction coefficient (m)",
       subtitle = "2019 extinction coefficients")

eems %>% 
  select(-Month) %>% 
  filter(!is.na(secchi_depth_m)) %>% 
  rename(Month = date_ymd) %>% 
  ggplot(aes(lake_section, secchi_depth_m)) +
  geom_boxplot() +
  geom_point(aes(col = Month)) +
  labs(x = "Lake section",
       y = "Secchi depth (m)",
       subtitle = "2019 Secchi depths")

eems %>% 
  select(-Month) %>% 
  filter(!is.na(turb_field_NTU)) %>% 
  rename(Month = date_ymd) %>% 
  ggplot(aes(lake_section, turb_field_NTU)) +
  geom_boxplot() +
  geom_point(aes(col = Month)) +
  labs(x = "Lake section",
       y = "Field turbidity (NTU)",
       subtitle = "2019 field turbidity")

eems %>% 
  select(-Month) %>% 
  filter(!is.na(turb_lab_NTU)) %>% 
  rename(Month = date_ymd) %>% 
  ggplot(aes(lake_section, turb_lab_NTU)) +
  geom_boxplot() +
  geom_point(aes(col = Month)) +
  labs(x = "Lake section",
       y = "Lab turbidity (NTU)",
       subtitle = "2019 lab turbidity")


# Linear models with distance ---------------------------------------------



eems %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, DOC_mg.L)) + 
  facet_wrap(~ Year) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = DOC_lab)

dd_means <- eems %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup()

dd_means_year <- eems %>% 
  group_by(distHaversine_km, Year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup()

dd_means16 <- dd_means_year %>% filter(Year == "2016")
dd_means17 <- dd_means_year %>% filter(Year == "2017")
dd_means18 <- dd_means_year %>% filter(Year == "2018")
dd_means19 <- dd_means_year %>% filter(Year == "2019")

mm1 <- lm(dd_means$DOC_mg.L ~ dd_means$distHaversine_km)
summary(mm1) # R2 = 0.9484, p = 5.7777e-06 // y = 0.038293x + 5.010753
dd_means %>% ggplot(aes(distHaversine_km, DOC_mg.L)) + geom_point() + geom_smooth(method = 'lm')

mm16 <- lm(dd_means16$DOC_mg.L ~ dd_means16$distHaversine_km)
summary(mm16) # R2 = 0.4911, p = 0.02131 // y = 0.03241x + 6.03693

mm17 <- lm(dd_means17$DOC_mg.L ~ dd_means17$distHaversine_km)
summary(mm17) # R2 = 0.9231, p = 2.361e-05 // y = 0.059415x + 5.474772

mm18 <- lm(dd_means18$DOC_mg.L ~ dd_means18$distHaversine_km)
summary(mm18) # R2 = 0.8711, p = 0.0001468 // y = 0.033329x + 4.436347

mm19 <- lm(dd_means19$DOC_mg.L ~ dd_means19$distHaversine_km)
summary(mm19) # R2 = 0.7726, p = 0.001113 // y = 0.030489x + 4.573036
