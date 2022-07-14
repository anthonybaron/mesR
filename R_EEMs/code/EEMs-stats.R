library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(rstatix)

theme_set(theme_bw(base_size = 14) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
eems <- bp_doc_eems() %>% mutate(distHaversine_km = distHaversine_km + 1.5)


# Can we average some sites? ----------------------------------------------

# // Inflow sites ---------------------------------------------------------

# Inflow sites (East, Centre, West)
inflow <- eems %>% 
  filter(grepl("Inflow", site_code_long)) %>% 
  select(site_code_long, Year, date_ymd, DOC_mg.L, SUVA, BA, HIX_Ohno)
# Inflow East   = 1.50 km from lake inflow
# Inflow Centre = 1.70 km from lake inflow
# Inflow West   = 1.94 km from lake inflow

p_inflow_doc <- inflow %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = DOC_lab, tag = 'A')

p_inflow_suva <- inflow %>% 
  ggplot(aes(site_code_long, SUVA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(1.76, 2.81)) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = SUVA_lab, tag = 'B')

p_inflow_ba <- inflow %>% 
  ggplot(aes(site_code_long, BA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  # scale_color_viridis_d(end = 0.8) +
  scale_color_viridis_d() +
  lims(y = c(0.70, 0.78)) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = BA_lab, tag = 'C')

p_inflow_hix <- inflow %>% 
  ggplot(aes(site_code_long, HIX_Ohno)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(NA, 0.90)) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = HIX_lab, tag = 'D')

p_anova <- ((p_inflow_doc + p_inflow_suva) / (p_inflow_ba + p_inflow_hix)) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("./R_EEMs/outputs/figures/20220712_p_anova.png", p_anova, w = 9, h = 8.1)

p_inflow_year <- inflow %>% 
  mutate(`Day of year` = DOY) %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) + 
  facet_wrap(~ Year) +
  geom_boxplot() +
  geom_point(aes(col = `Day of year`), size = 4, alpha = 3/4) +
  scale_color_viridis_c(end = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = DOC_lab)

p_inflow + p_inflow_year


inflow %>% 
  pivot_longer(cols = -c(site_code_long, Year, date_ymd),
               names_to = "parameter",
               values_to = "result") %>% 
  pivot_wider(names_from = c(site_code_long, parameter),
              names_sep = "_",
              values_from = result) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.))) %>% 
  mutate(phr = NA) %>% 
  pivot_longer(cols = -phr, names_to = "site_parameter", values_to = "n") %>% 
  select(-phr)

# summary statistics
inflow %>% 
  group_by(site_code_long) %>%
  summarise_if(is.numeric, list(mean = mean, sd = sd, var = var)) 

# check outliers --- no extreme outliers
inflow %>% 
  select(-date_ymd) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  group_by(site_code_long, parameter) %>% 
  identify_outliers(result) # DOC & BA each have one outlier, not extreme

# check normality --- log-transform DOC 
inflow %>% 
  select(-date_ymd) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  group_by(site_code_long, parameter) %>% 
  shapiro_test(result) %>% 
  arrange(p)

inflow %>% 
  select(-date_ymd) %>% 
  mutate(logDOC_mg.L = log(DOC_mg.L)) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  ggpubr::ggqqplot(., "result", facet.by = c("site_code_long", "parameter"))

# check equality of variances --- variances equal
inflow %>% levene_test(DOC_mg.L ~ site_code_long)   # p = 0.984 
inflow %>% levene_test(SUVA ~ site_code_long)     # p = 0.939
inflow %>% levene_test(BA ~ site_code_long)       # p = 0.231
inflow %>% levene_test(HIX_Ohno ~ site_code_long) # p = 0.590


doc_aov <- inflow %>% anova_test(DOC_mg.L ~ site_code_long) # log-trans doesn't change outcome
suva_aov <- inflow %>% anova_test(SUVA ~ site_code_long)
ba_aov <- inflow %>% anova_test(BA ~ site_code_long)
hix_aov <- inflow %>% anova_test(HIX_Ohno ~ site_code_long)

# DOC:  F(2, 52) = 0.126, p = 0.882, ges = 0.005
# SUVA: F(2, 52) = 0.002, p = 0.998, ges = 7.62e-05
# BA:   F(2, 52) = 0.064, p = 0.938, ges = 0.002
# HIX:  F(2, 52) = 0.088, p = 0.916, ges = 0.003 



# // Upstream Causeway sites ----------------------------------------------

# Upstream Causeway sites (Upstream Causeway Centre, Upstream Causeway East)
causeway <- eems %>% 
  filter(grepl("Upstream Causeway", site_code_long)) %>% 
  select(site_code_long, Year, date_ymd, DOC_mg.L, SUVA, BA, HIX_Ohno)
ccc <- us_causeway %>% filter(site_code_long == "Upstream Causeway Centre") %>% select(DOC_mg.L)
cce <- us_causeway %>% filter(site_code_long == "Upstream Causeway East") %>% select(DOC_mg.L)
# Upstream Causeway Centre = 3.68 km from lake inflow
# Upstream Causeway East   = 3.83 km from lake inflow

p_causeway_doc <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom") %>% 
  labs(x = NULL, y = DOC_lab, tag = 'A')

p_causeway_suva <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, SUVA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(1.76, 2.81)) +
  theme(legend.position = "bottom") %>% 
  labs(x = NULL, y = SUVA_lab, tag = 'B')

p_causeway_ba <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, BA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  # scale_color_viridis_d(end = 0.8) +
  scale_color_viridis_d() +
  lims(y = c(0.70, 0.78)) +
  theme(legend.position = "bottom") %>% 
  labs(x = NULL, y = BA_lab, tag = 'C')

p_causeway_hix <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, HIX_Ohno)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(0.77, 0.87)) +
  theme(legend.position = "bottom") %>% 
  labs(x = NULL, y = HIX_lab, tag = 'D')

p_t_test <- ((p_causeway_doc + p_causeway_suva) / (p_causeway_ba + p_causeway_hix)) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("./R_EEMs/outputs/figures/20220712_p_t_test.png", p_t_test, w = 9, h = 8.1)

causeway %>% 
  pivot_longer(cols = -c(site_code_long, Year, date_ymd),
               names_to = "parameter",
               values_to = "result") %>% 
  pivot_wider(names_from = c(site_code_long, parameter),
              names_sep = "_",
              values_from = result) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.))) %>% 
  mutate(phr = NA) %>% 
  pivot_longer(cols = -phr, names_to = "site_parameter", values_to = "n") %>% 
  select(-phr)

# summary statistics
causeway %>% 
  group_by(site_code_long) %>%
  summarise_if(is.numeric, list(mean = mean, sd = sd, var = var)) 

# check outliers --- no extreme outliers
causeway %>% 
  select(-date_ymd) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  group_by(site_code_long, parameter) %>% 
  identify_outliers(result) 
# DOC_mg.L: outlier for East, not extreme
# BA: outlier for both, not extreme
# HIX_Ohno: outlier for Centre, not extreme
# SUVA: outlier for both, not extreme

# check normality --- log-transform DOC 
causeway %>% 
  select(-date_ymd) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  group_by(site_code_long, parameter) %>% 
  shapiro_test(result) %>% 
  arrange(p)

causeway %>% 
  select(-date_ymd) %>% 
  mutate(logDOC_mg.L = log(DOC_mg.L)) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter",
               values_to = "result") %>% 
  mutate(parameter = factor(parameter)) %>% 
  ggpubr::ggqqplot(., "result", facet.by = c("site_code_long", "parameter"))

# check equality of variances --- variances equal
causeway %>% levene_test(DOC_mg.L ~ site_code_long) # p = 0.839 
causeway %>% levene_test(SUVA ~ site_code_long)     # p = 0.650
causeway %>% levene_test(BA ~ site_code_long)       # p = 0.818
causeway %>% levene_test(HIX_Ohno ~ site_code_long) # p = 0.632

causeway_ttest <- causeway %>%
  mutate(Treatment = "placeholder",
         Treatment = factor(Treatment)) %>% 
  group_by(Treatment)

doc_ttest <- causeway_ttest %>% t_test(DOC_mg.L ~ site_code_long) # t(36.1) = -0.0134, p = 0.989, n = 39
suva_ttest <- causeway_ttest %>% t_test(SUVA ~ site_code_long)    # t(36.4) =  0.370, p = 0.713, n = 39
ba_ttest <- causeway_ttest %>% t_test(BA ~ site_code_long)        # t(36.7) = -1.55, p = 0.13, n = 39
hix_ttest <- causeway_ttest %>% t_test(HIX_Ohno ~ site_code_long) # t(35.6) =  0.396, p = 0.694, n = 39

# Data prep ---------------------------------------------------------------

inflow <- eems %>% filter(grepl("Inflow", site_code_long))
us_causeway <- eems %>% filter(grepl("Upstream Causeway", site_code_long)) 
ccc <- us_causeway %>% filter(site_code_long == "Upstream Causeway Centre") %>% select(DOC_mg.L)
cce <- us_causeway %>% filter(site_code_long == "Upstream Causeway East") %>% select(DOC_mg.L)

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


# Parameter labels --------------------------------------------------------

A254_lab <- expression(paste("A"[254]*""))
A280_lab <- expression(paste("A"[280]*""))
A350_lab <- expression(paste("A"[350]*""))
A440_lab <- expression(paste("A"[440]*""))
AT_ratio_lab <- "Peak A:Peak T"
BA_lab <- expression(paste("Freshness Index (", italic(β), ":", italic(α), ")"))
Chla_lab <- expression(paste("Chl", itlaic(a), "concentration (µg L"^-1*")")) 
CA_ratio_lab <- "Peak C:Peak A"
CM_ratio_lab <- "Peak C:Peak M"
CT_ratio_lab <- "Peak C:Peak T"
dist_lab <- "Distance from Buffalo Pound Lake inflow (km)"
DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
extinction_coefficient_lab <- expression(paste("k"[T]*" (m"^-1*")"))
FI_lab <- "Fluorescence Index"
HIX_lab <- "Humification Index"
PeakA_lab <- "Peak A (RU)"
PeakB_lab <- "Peak B (RU)"
PeakC_lab <- "Peak C (RU)"
PeakD_lab <- "Peak D (RU)"
PeakE_lab <- "Peak E (RU)"
PeakM_lab <- "Peak M (RU)"
PeakN_lab <- "Peak N (RU)"
PeakP_lab <- "Peak P (RU)"
PeakT_lab <- "Peak T (RU)"
S_lab <- "<i>S</i><sub>275–295</sub>"
S_lab2 <- "<i>S</i><sub>350–400</sub>"
secchi_lab <- "Secchi depthi (m)"
SUVA_lab <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))
TDN_lab <- expression(paste("TDN concentration (mg L"^-1*")")) 
turb_field_lab <- "Field turbidity (NTU)"
turb_lab_lab <- "Lab turbidity (NTU)"


# Histograms --------------------------------------------------------------

p_histograms <- eems %>% 
  select(A254, AT_ratio, BA, CA_ratio, chla_ug.L, CM_ratio, CT_ratio, DOC_mg.L,
         ext_coeff_m, FI, HIX_Ohno, PeakA_RU, PeakB_RU, PeakC_RU, PeakD_RU,
         PeakE_RU, PeakM_RU, PeakN_RU, PeakP_RU, PeakT_RU, S275to295, 
         secchi_depth_m, SUVA, TDN_mg.L, turb_field_NTU, turb_lab_NTU) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

shapiro_test_res <- eems %>% 
  select(A254, AT_ratio, BA, CA_ratio, chla_ug.L, CM_ratio, CT_ratio, DOC_mg.L,
         ext_coeff_m, FI, HIX_Ohno, PeakA_RU, PeakB_RU, PeakC_RU, PeakD_RU,
         PeakE_RU, PeakM_RU, PeakN_RU, PeakP_RU, PeakT_RU, S275to295, 
         secchi_depth_m, SUVA, TDN_mg.L, turb_field_NTU, turb_lab_NTU) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  group_by(parameter) %>%
  shapiro_test(result) %>% 
  mutate(sig = ifelse(p >= 0.05, "normal", "transform"))

shapiro_join <- shapiro_test_res %>% select(parameter, sig)

eems_long <- eems %>% 
  select(A254, AT_ratio, BA, CA_ratio, chla_ug.L, CM_ratio, CT_ratio, DOC_mg.L,
         ext_coeff_m, FI, HIX_Ohno, PeakA_RU, PeakB_RU, PeakC_RU, PeakD_RU,
         PeakE_RU, PeakM_RU, PeakN_RU, PeakP_RU, PeakT_RU, S275to295, 
         secchi_depth_m, SUVA, TDN_mg.L, turb_field_NTU, turb_lab_NTU) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result))

eems_trans <- eems_long %>%
  left_join(shapiro_join) %>% 
  group_by(parameter) %>% 
  mutate(log_result = ifelse(sig == "transform", log(result), NA),
         sqrt_result = ifelse(sig == "transform", sqrt(result), NA))

eems_log <- eems_trans %>% filter(!is.na(log_result))
eems_sqrt <- eems_trans %>% filter(!is.na(sqrt_result))

p_eems_log <- eems_log %>% 
  ggplot(aes(x = log_result)) +
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

p_eems_sqrt <- eems_trans %>% 
  ggplot(aes(x = sqrt_result)) +
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")
  
shapiro_test_log <- eems_log %>% 
  group_by(parameter) %>% 
  shapiro_test(log_result) %>% 
  mutate(sig = ifelse(p >= 0.05, "normal", "transform"))

shapiro_test_sqrt <- eems_sqrt %>% 
  group_by(parameter) %>% 
  shapiro_test(sqrt_result) %>% 
  mutate(sig = ifelse(p >= 0.05, "normal", "transform"))

log_parms <- shapiro_test_log %>% 
  filter(sig == "normal" & !parameter %in% c("secchi_depth_m", "turb_field_NTU", "ext_coeff_m"))


# Seasonal differences ----------------------------------------------------

ee_spring <- eems %>% filter(Month %in% c("Mar", "Apr", "May", "Jun")) # n = 85
ee_summer <- eems %>% filter(Month %in% c("Jul", "Aug", "Sep")) # n = 78

ee_seasons <- eems %>% 
  mutate(Season = ifelse(Month %in% c("Mar", "Apr", "May", "June"), "Spring", "Summer"),
         Season = factor(Season))

ee_seasons %>% 
  group_by(Season, distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>%  
  ggplot(aes(distHaversine_km, DOC_mg.L, col = Season, shape = Season)) + 
  # geom_line(size = 1) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 3) +  
  geom_smooth(method = 'lm', se = F, alpha = 1/4, size = 1) + 
  lims(x = c(0, 30)) +
  labs(x = dist_lab, y = DOC_lab)

ee_seasons %>% 
  group_by(Season, distHaversine_km, Year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>%  
  ggplot(aes(distHaversine_km, DOC_mg.L, col = Season, shape = Season)) + 
  facet_wrap(~ Year) +
  geom_line(size = 1) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 3) +  
  # geom_smooth(method = 'lm', se = F, alpha = 1/4, size = 1) + 
  lims(x = c(0, 30)) +
  labs(x = dist_lab, y = DOC_lab)

spr <- ee_seasons %>% filter(Season == "Spring") %>% select(DOC_mg.L)
sum <- ee_seasons %>% filter(Season == "Summer") %>% select(DOC_mg.L)

var(spr$DOC_mg.L) # 1.137794
var(sum$DOC_mg.L) # 0.8075726

t.test(spr, sum, alternative = "two.sided", var.equal = FALSE)
# t(109.98) = 0.24014, p = 0.8107
# Spring and summer DOC concentrations are not significantly different

mean_spr <- ee_seasons %>% 
  filter(Season == "Spring") %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

var(mean_spr$DOC_mg.L) # = 0.02756821

mean_sum <- ee_seasons %>% 
  filter(Season == "Summer") %>% 
  group_by(distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

var(mean_sum$DOC_mg.L) # = 0.2977929

mspr <- lm(DOC_mg.L ~ distHaversine_km, data = mean_spr)
summary(mspr)
# R2 = 0.7232, p = 0.004608 // y = 0.014014x + 5.328478
par(mfrow = c(2, 2)); plot(mspr)
gratia::appraise(mspr)

msum <- lm(DOC_mg.L ~ distHaversine_km, data = mean_sum)
summary(msum)
# R2 = 0.9575, p = 1.536e-05 // y = 0.051766x + 4.835060
par(mfrow = c(2, 2)); plot(msum)
gratia::appraise(msum)

## // ## 

ee_seasons_avg <- ee_seasons %>% 
  group_by(Season, distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(logDOC_mg.L = log(DOC_mg.L))


model1 <- glm(DOC_mg.L ~ distHaversine_km * Season, data = ee_seasons_avg)
summary(model1)
anova(model1, test = 'F')

# compare to null model
model_null <- glm(DOC_mg.L ~ 1, data = ee_seasons_avg)
anova(model_null, model1, test = 'F')
# model1 is significantly different from the null model

# check the model assumptions
par(mfrow = c(2,2)); plot(model1)

# check distribution
ee_seasons_avg %>% 
  ggplot(aes(log(DOC_mg.L))) + 
  geom_histogram()

# try log-transformation
model2 <- glm(logDOC_mg.L ~ distHaversine_km * Season, data = ee_seasons_avg)
summary(model2)
anova(model2, test = 'F')

par(mfrow = c(2,2)); plot(model2)

coef(model2)

ee_seasons_avg %>% 
  ggplot(aes(distHaversine_km, logDOC_mg.L, col = Season, shape = Season)) + 
  geom_point(size = 3) +
  lims(x = c(0, 30), y = c(1, 3)) + 
  labs(x = dist_lab, y = DOC_lab)


# all terms significant so no need for model simplication

# the next step in model simplification is to test whether or not Season has a 
# significant effect on DOC concentration when we control for distHaversine_km;
# to do this we use update(), and remove Season:
model3 <- update(model2, ~ . - Season)
summary(model3)
anova(model3, test = 'F')

mmm <- glm(logDOC_mg.L ~ distHaversine_km, data = ee_seasons_avg)
summary(mmm)

anova(model2, model3, test = 'F')

model4 <- glm(formula = logDOC_mg.L ~ distHaversine_km + Season, data = ee_seasons_avg)

## // ##

ee_seasons_avg %>% 
  ggplot(aes(Season, DOC_mg.L)) + 
  geom_boxplot()

ee_seasons_avg %>%
  ungroup() %>%  
  mutate(Season = factor(Season)) %>% 
  t_test(formula = DOC_mg.L ~ Season, p.adjust.method = 'bonferroni', detailed = TRUE)

ggpubr::ggpaired(ee_seasons_avg, x = "Season", y = "DOC_mg.L",
                 order = c("Spring", "Summer"),
                 xlab = "Season", ylab = DOC_lab)

ee_seasons_avg_wide <- ee_seasons_avg %>% 
  select(-logDOC_mg.L) %>% 
  pivot_wider(names_from = Season, values_from = DOC_mg.L) %>% 
  mutate(differences = Spring - Summer)

ee_seasons_avg_wide %>% identify_outliers(differences) # no extreme outliters
ee_seasons_avg_wide %>% shapiro_test(differences) # ~ normal
ggpubr::ggqqplot(ee_seasons_avg_wide, "differences")

t_test_res <- ee_seasons_avg %>%
  t_test(DOC_mg.L ~ Season, paired = TRUE, detailed = TRUE) %>%
  add_significance()
# not significantly different

t_test_res <- t_test_res %>% add_xy_position(x = "Season")

ggpubr::ggpaired(ee_seasons_avg, x = "Season", y = "DOC_mg.L",
                 order = c("Spring", "Summer"),
                 xlab = "Season", ylab = DOC_lab) +
  ggpubr::stat_pvalue_manual(t_test_res) + 
  labs(subtitle = get_test_label(t_test_res, detail = TRUE))






# Seasonal differences (paired sample t-test) -----------------------------

ee_spring <- eems %>% filter(Month %in% c("Mar", "Apr", "May", "Jun")) # n = 85
ee_summer <- eems %>% filter(Month %in% c("Jul", "Aug", "Sep")) # n = 78

ee_seasons <- eems %>% 
  mutate(Season = ifelse(Month %in% c("Mar", "Apr", "May", "Jun"), "Spring", "Summer"),
         Season = factor(Season)) %>% 
  

ee_seasons %>% 
  ggplot(aes(Season, DOC_mg.L)) + 
  facet_wrap(~ Year) + 
  geom_boxplot()

eeavg <- ee_seasons %>%
  mutate(logAT_ratio = log(AT_ratio),
         logCT_ratio = log(CT_ratio),
         logPeakC_RU = log(PeakC_RU),
         logPeakD_RU = log(PeakD_RU),
         logPeakT_RU = log(PeakT_RU)) %>% 
  select(-c(site_name, Month, DOY, latitude, longitude)) %>% 
  pivot_longer(cols = -c(site_code_long, site_abbr, date_ymd, Year, distHaversine_km, Season),
               names_to = "parameter",
               values_to = "result") %>% 
  group_by(Season, site_abbr, parameter) %>% 
  summarise(parameter_mean = mean(result, na.rm = TRUE),
            parameter_sd = sd(result, na.rm = TRUE),
            n = n())

eeavg %>%
  group_by(parameter) %>% 
  t_test(parameter_mean ~ Season, p.adjust.method = 'bonferroni', detailed = TRUE) %>% 
  add_significance() %>% 
  View()

ee_seasons_avg <- ee_seasons %>% 
  group_by(Season, distHaversine_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(logDOC_mg.L = log(DOC_mg.L))

ee_seasons_avg %>%
  ungroup() %>%  
  mutate(Season = factor(Season)) %>% 
  t_test(formula = DOC_mg.L ~ Season, p.adjust.method = 'bonferroni', detailed = TRUE)

ggpubr::ggpaired(ee_seasons_avg, x = "Season", y = "DOC_mg.L",
                 order = c("Spring", "Summer"),
                 xlab = "Season", ylab = DOC_lab)

ee_seasons_avg_wide <- ee_seasons_avg %>% 
  select(-logDOC_mg.L) %>% 
  pivot_wider(names_from = Season, values_from = DOC_mg.L) %>% 
  mutate(differences = Spring - Summer)

ee_seasons_avg_wide %>% identify_outliers(differences) # no extreme outliters
ee_seasons_avg_wide %>% shapiro_test(differences) # ~ normal
ggpubr::ggqqplot(ee_seasons_avg_wide, "differences")

t_test_res <- ee_seasons_avg %>%
  t_test(DOC_mg.L ~ Season, paired = TRUE, detailed = TRUE) %>%
  add_significance()
# not significantly different

t_test_res <- t_test_res %>% add_xy_position(x = "Season")

ggpubr::ggpaired(ee_seasons_avg, x = "Season", y = "DOC_mg.L",
                 order = c("Spring", "Summer"),
                 xlab = "Season", ylab = DOC_lab) +
  ggpubr::stat_pvalue_manual(t_test_res) + 
  labs(subtitle = get_test_label(t_test_res, detail = TRUE))