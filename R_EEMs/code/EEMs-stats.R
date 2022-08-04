library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(rstatix)
library(readr)
library(stringr)
library(ggtext)
library(purrr)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
eems_all <- bp_doc_eems() %>% 
  select(site_name:FI, HIX_Ohno:SR, PeakA_RU:ext_coeff_m, spA:spT) %>% 
  # filter(spB < 0.1) %>% 
  mutate(dist_km = distHaversine_km + 1.5) %>% 
  select(site_name:longitude, dist_km, everything()) %>% 
  select(-distHaversine_km)



# Parameter labels --------------------------------------------------------

A254_lab <- expression(paste("A"[254]*""))
A280_lab <- expression(paste("A"[280]*""))
A350_lab <- expression(paste("A"[350]*""))
A440_lab <- expression(paste("A"[440]*""))
AT_ratio_lab <- "Peak A:Peak T"
BA_lab <- "Freshness index<br>(<i>&beta;<i/>:<i>&alpha;<i/>)"
BA_lab1 <- expression(paste("Freshness index (", italic(β), ":", italic(α), ")"))
Chla_lab <- expression(paste("Chl ", italic(a), " (µg L"^-1*")")) 
CA_ratio_lab <- "Peak C:Peak A"
CM_ratio_lab <- "Peak C:Peak M"
CT_ratio_lab <- "Peak C:Peak T"
dist_lab <- "Distance from Buffalo Pound Lake inflow (km)"
DOC_lab <- "DOC concentration (mg L<sup>–1</sup>)"
# DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
extinction_coefficient_lab <- expression(paste("k"[T]*" (m"^-1*")"))
FI_lab <- "Fluorescence index<br>(FI)"
HIX_lab <- "Humification index<br>(HIX)"
PeakA_lab <- "Peak A (RU)"
PeakB_lab <- "Peak B (RU)"
PeakC_lab <- "Peak C (RU)"
PeakD_lab <- "Peak D (RU)"
PeakE_lab <- "Peak E (RU)"
PeakM_lab <- "Peak M (RU)"
PeakN_lab <- "Peak N (RU)"
PeakP_lab <- "Peak P (RU)"
PeakT_lab <- "Peak T (RU)"
S_lab <- "<i>S</i><sub>275–295</sub><br>(nm<sup>–1</sup>)"
S_lab2 <- "<i>S</i><sub>350–400</sub> (nm<sup>–1</sup>)"
secchi_lab <- "Secchi depth (m)"
SUVA_lab <- "SUVA<sub>254</sub><br>(L mg-C<sup>–1</sup> m<sup>–1</sup>)"
SUVA_lab1 <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))
TDN_lab <- "TDN concentration<br>(mg L<sup>–1</sup>)"
# TDN_lab <- expression(paste("TDN concentration (mg L"^-1*")")) 
turb_field_lab <- expression(paste("Turbidity"[field]*" (NTU)"))
turb_lab_lab <- expression(paste("Turbidity"[lab]*" (NTU)"))




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
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold"),
        axis.title.y = element_markdown()) +
  labs(x = NULL, y = DOC_lab, tag = 'a')

p_inflow_suva <- inflow %>% 
  ggplot(aes(site_code_long, SUVA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(1.76, 2.81)) +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  labs(x = NULL, y = SUVA_lab1, tag = 'b')

p_inflow_ba <- inflow %>% 
  ggplot(aes(site_code_long, BA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  # scale_color_viridis_d(end = 0.8) +
  scale_color_viridis_d() +
  lims(y = c(0.70, 0.78)) +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  labs(x = NULL, y = BA_lab1, tag = 'c')

p_inflow_hix <- inflow %>% 
  ggplot(aes(site_code_long, HIX_Ohno)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(NA, 0.90)) +
  theme(legend.position = "bottom", 
        plot.tag = element_text(face = "bold")) +
  labs(x = NULL, y = "Humification index (HIX)", tag = 'd')

p_anova <- ((p_inflow_doc + p_inflow_suva) / (p_inflow_ba + p_inflow_hix)) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("./R_EEMs/outputs/figures/20220801_p_anova.png", p_anova, w = 9, h = 8.1)

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
  theme(legend.position = "bottom", 
        plot.tag = element_text(face = "bold"),
        axis.title.y = element_markdown()) +
  labs(x = NULL, y = DOC_lab, tag = 'a')

p_causeway_suva <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, SUVA)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(1.76, 2.81)) +
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) %>% 
  labs(x = NULL, y = SUVA_lab1, tag = 'b')

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
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) +
  labs(x = NULL, y = BA_lab1, tag = 'c')

p_causeway_hix <- causeway %>% 
  mutate(site_code_long = ifelse(site_code_long == "Upstream Causeway Centre", 
                                 "Upstream Causeway\nCentre", 
                                 "Upstream Causeway\nEast")) %>% 
  ggplot(aes(site_code_long, HIX_Ohno)) + 
  geom_boxplot() +
  geom_point(aes(col = Year), size = 3, alpha = 2/3) +
  scale_color_viridis_d() +
  lims(y = c(0.77, 0.87)) +
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) +
  labs(x = NULL, y = "Humification index (HIX)", tag = 'd')

p_t_test <- ((p_causeway_doc + p_causeway_suva) / (p_causeway_ba + p_causeway_hix)) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("./R_EEMs/outputs/figures/20220801_p_t_test.png", p_t_test, w = 8, h = 6.5)

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

inflow <- eems_all %>% filter(grepl("Inflow", site_code_long))
us_causeway <- eems_all %>% filter(grepl("Upstream Causeway", site_code_long)) 
# ccc <- us_causeway %>% filter(site_code_long == "Upstream Causeway Centre") %>% select(DOC_mg.L)
# cce <- us_causeway %>% filter(site_code_long == "Upstream Causeway East") %>% select(DOC_mg.L)

lake_inflow <- inflow %>%
  group_by(date_ymd) %>% 
  summarise(across(TDN_mg.L:spT, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(site_name = "1.5 km below Qu'Appelle R. Inflow Centre",
         site_code_long = as.factor("Lake Inflow"),
         site_abbr = as.factor("LI"),
         Year = as.factor(year(date_ymd)), 
         Month = month(date_ymd, label = TRUE, abbr = TRUE), 
         DOY = yday(date_ymd), 
         latitude = 50.722927,
         longitude = -105.605669,
         dist_km = 1.5) %>% 
  select(site_name, site_code_long, site_abbr, date_ymd, Year:dist_km, everything())

upstream_causeway <- us_causeway %>% 
  group_by(date_ymd) %>% 
  summarise(across(TDN_mg.L:spT, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(site_name = "Upstream Causeway",
         site_code_long = as.factor("Upstream Causeway"),
         site_abbr = as.factor("CU"),
         Year = as.factor(year(date_ymd)), 
         Month = month(date_ymd, label = TRUE, abbr = TRUE), 
         DOY = yday(date_ymd), 
         latitude = 50.722927, 
         longitude = -105.605669,
         dist_km = 3.68) %>% 
  select(site_name, site_code_long, site_abbr, date_ymd, Year:dist_km, everything())


eems_select <- subset(eems_all, !grepl("Inflow", site_code_long))
eems_select <- subset(eems_select, !grepl("Upstream Causeway", site_code_long))
eems <- eems_select %>% bind_rows(lake_inflow) %>% bind_rows(upstream_causeway) %>% arrange(date_ymd)

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


# Histograms --------------------------------------------------------------

eems_long <- eems %>% 
  select(-c(site_name:dist_km, distHaversine_km)) %>% 
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) 

p_histograms <- eems_long %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

shapiro_test_res <- eems_long %>% 
  group_by(parameter) %>%
  shapiro_test(result) %>% 
  mutate(sig = ifelse(p >= 0.1, "normal", "transform"))

normal_parms <- shapiro_test_res %>% 
  select(parameter, sig) %>% 
  filter(sig == "normal") %>% 
  mutate(transformation = "none")
    

shapiro_join <- shapiro_test_res %>% select(parameter, sig)

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
  mutate(sig = ifelse(p >= 0.1, "normal", "transform"))

log_parms <- shapiro_test_log %>% 
  select(parameter, sig) %>% 
  filter(sig == "normal") %>% 
  mutate(transformation = "log")

log_parms_nn <- shapiro_test_log %>% 
  select(parameter, sig) %>% 
  filter(sig == "transform") %>% 
  mutate(transformation = "DNW")

shapiro_test_sqrt <- eems_sqrt %>% 
  group_by(parameter) %>% 
  shapiro_test(sqrt_result) %>% 
  mutate(sig = ifelse(p >= 0.1, "normal", "transform"))

sqrt_parms <- shapiro_test_sqrt %>% 
  select(parameter, sig) %>% 
  filter(sig == "normal") %>% 
  mutate(transformation = "sqrt")

sqrt_parms_nn <- shapiro_test_sqrt %>% 
  select(parameter, sig) %>% 
  filter(sig == "transform") %>% 
  mutate(transformation = "DNW")

bind_rows(normal_parms, log_parms, sqrt_parms, log_parms_nn, sqrt_parms_nn) 

# Plot again to see how distributions look

# normal parameters
eems_long %>% 
  filter(parameter %in% c("FI", "PeakN_RU", "S350to400", "SUVA", "TDN_mg.L")) %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

# log-transformed parameters
eems_long %>% 
  filter(parameter %in% c("AT_ratio", "CT_ratio", "PeakA_RU", "ext_coeff_m", "PeakB_RU",
                          "PeakC_RU", "PeakD_RU", "PeakM_RU",
                          "PeakT_RU", "secchi_depth_m", "SR", "turb_field_NTU")) %>% 
  ggplot(aes(x = log(result))) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

# parameters that don't benefit from log or sqrt transformation

# BA looks like it's close enough
eems_long %>% 
  filter(parameter %in% c("A254", "A280", "A350", "A440", "BA", "CA_ratio", 
                          "chla_ug.L", "CM_ratio", "DOC_mg.L", "HIX_Ohno", 
                          "S275to295", "turb_lab_NTU")) %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

# everything looks decent after log-transformation even with significant 
# Shapiro test results
eems_long %>% 
  filter(parameter %in% c("A254", "A280", "A350", "A440", "BA", "CA_ratio", 
                          "chla_ug.L", "CM_ratio", "DOC_mg.L", "HIX_Ohno", 
                          "S275to295", "turb_lab_NTU")) %>% 
  ggplot(aes(x = log(result))) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")

# sqrt doesn't look better than log-transformation
eems_long %>% 
  filter(parameter %in% c("A254", "A280", "A350", "A440", "BA", "CA_ratio", 
                          "chla_ug.L", "CM_ratio", "DOC_mg.L", "HIX_Ohno", 
                          "S275to295", "turb_lab_NTU")) %>% 
  ggplot(aes(x = sqrt(result))) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")



# Transform necessary parameters ------------------------------------------

eems_long_trans <- eems_long %>% 
  mutate(transformation = ifelse(parameter %in% c("FI", "PeakA_RU", "PeakN_RU", "S275to295", "SUVA", "TDN_mg.L"), "none", "log"),
         result = ifelse(transformation == "log", log(result), result),
         parameter = ifelse(transformation == "log", paste0("log_", parameter), parameter))

eems_long_trans %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  labs(x = "Value", y = "Count") +
  facet_wrap(~ parameter, scales = "free", ncol = 5)

log_parms_c <- eems_long_trans %>% filter(grepl("log", parameter)) %>% select(parameter) %>% distinct()


# Summary stats -----------------------------------------------------------

# overall
eems_summary_stats <- eems_long %>% 
  group_by(parameter) %>% 
  get_summary_stats(type = "full")

write_csv(eems_summary_stats, "./R_EEMs/outputs/data/20220718_eems-summary-stats.csv")


# by site
eems_long_site <- eems %>% 
  select(-c(site_name, site_code_long, date_ymd:distHaversine_km, distHaversine_m, site_num, HIX, Fmax,
            TSS_mg.L, source, site_altname, PeakP_RU, dist_km)) %>% 
  select(!contains("percent")) %>% 
  pivot_longer(cols = -site_abbr, names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) 

eems_summary_stats_site <- eems_long_site %>% 
  group_by(site_abbr, parameter) %>% 
  get_summary_stats(type = "full") 

eems_dist <- eems %>% select(site_abbr, dist_km) %>% distinct()

eems_summary_stats_site <- eems_summary_stats_site %>% left_join(eems_dist) %>%
  select(site_abbr, parameter, dist_km, everything())

eems_summary_stats_site %>% select(-c(variable, q1, q3, iqr, mad, se, ci)) 

# by year
eems_long_year
eems %>% 
  select(-c(site_name, site_code_long, Month:dist_km)) %>% 
  pivot_longer(cols = -c(site_abbr, date_ymd, Year), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  filter(parameter == "DOC_mg.L") %>% 
  group_by(Year) %>% 
  summarise(n = n())



# Seasonal differences ----------------------------------------------------

ee_spring <- eems %>% filter(Month %in% c("Mar", "Apr", "May", "Jun")) # n = 87
ee_summer <- eems %>% filter(Month %in% c("Jul", "Aug", "Sep")) # n = 78

ee_seasons <- eems %>% 
  mutate(Season = ifelse(Month %in% c("Mar", "Apr", "May", "June"), "Spring", "Summer"),
         Season = factor(Season))

p_doc_season <- ee_seasons %>% 
  group_by(Season, dist_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>%  
  ggplot(aes(dist_km, DOC_mg.L, col = Season, shape = Season)) + 
  # geom_line(size = 1) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 3) +  
  geom_smooth(method = 'lm', se = F, alpha = 1/4, size = 1) + 
  lims(x = c(0, 30)) +
  theme(axis.title.y = element_markdown(), legend.position = "bottom") +
  labs(x = dist_lab, y = DOC_lab)

p_doc_season_year <- ee_seasons %>% 
  group_by(Season, dist_km, Year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>%  
  ggplot(aes(dist_km, DOC_mg.L, col = Season, shape = Season)) + 
  facet_wrap(~ Year) +
  geom_line(size = 1) +
  geom_point(size = 4, col = "white") +
  geom_point(size = 3) +  
  # geom_smooth(method = 'lm', se = F, alpha = 1/4, size = 1) + 
  lims(x = c(0, 30)) +
  theme(axis.title.y = element_markdown(), legend.position = "bottom") +
  labs(x = dist_lab, y = DOC_lab)

(p_doc_season + p_doc_season_year) 

spr <- ee_seasons %>% filter(Season == "Spring") %>% select(DOC_mg.L)
sum <- ee_seasons %>% filter(Season == "Summer") %>% select(DOC_mg.L)

var(spr$DOC_mg.L) # 1.137794
var(sum$DOC_mg.L) # 0.8075726

t.test(spr, sum, alternative = "two.sided", var.equal = FALSE)
# t(109.98) = 0.24014, p = 0.8107
# Spring and summer DOC concentrations are not significantly different

mean_spr <- ee_seasons %>% 
  filter(Season == "Spring") %>% 
  group_by(dist_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

var(mean_spr$DOC_mg.L) # = 0.02756821

mean_sum <- ee_seasons %>% 
  filter(Season == "Summer") %>% 
  group_by(dist_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) 

var(mean_sum$DOC_mg.L) # = 0.2977929

mspr <- lm(DOC_mg.L ~ dist_km, data = mean_spr)
summary(mspr)
# R2 = 0.7232, p = 0.004608 // y = 0.014014x + 5.328478
par(mfrow = c(2, 2)); plot(mspr)
gratia::appraise(mspr)

msum <- lm(DOC_mg.L ~ dist_km, data = mean_sum)
summary(msum)
# R2 = 0.9575, p = 1.536e-05 // y = 0.051766x + 4.835060
par(mfrow = c(2, 2)); plot(msum)
gratia::appraise(msum)

## // ## 

ee_seasons_avg <- ee_seasons %>% 
  group_by(Season, dist_km) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(logDOC_mg.L = log(DOC_mg.L))


model1 <- glm(DOC_mg.L ~ dist_km * Season, data = ee_seasons_avg)
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
model2 <- glm(logDOC_mg.L ~ dist_km * Season, data = ee_seasons_avg)
summary(model2)
anova(model2, test = 'F')

par(mfrow = c(2,2)); plot(model2)

coef(model2)

ee_seasons_avg %>% 
  ggplot(aes(dist_km, logDOC_mg.L, col = Season, shape = Season)) + 
  geom_point(size = 3) +
  lims(x = c(0, 30), y = c(1, 3)) + 
  labs(x = dist_lab, y = DOC_lab)


# all terms significant so no need for model simplication

# the next step in model simplification is to test whether or not Season has a 
# significant effect on DOC concentration when we control for dist_km;
# to do this we use update(), and remove Season:
model3 <- update(model2, ~ . - Season)
summary(model3)
anova(model3, test = 'F')

mmm <- glm(logDOC_mg.L ~ dist_km, data = ee_seasons_avg)
summary(mmm)

anova(model2, model3, test = 'F')

model4 <- glm(formula = logDOC_mg.L ~ dist_km + Season, data = ee_seasons_avg)

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

ee_spring <- eems %>% filter(Month %in% c("Mar", "Apr", "May", "Jun")) # n = 87
ee_summer <- eems %>% filter(Month %in% c("Jul", "Aug", "Sep")) # n = 78

ee_seasons <- eems %>% 
  mutate(Season = ifelse(Month %in% c("Mar", "Apr", "May", "Jun"), "Spring", "Summer"),
         Season = factor(Season)) 
  
ee_seasons %>% 
  filter(!is.na(chla_ug.L)) %>% 
  group_by(Season) %>% 
  summarise(mean = mean(chla_ug.L, na.rm = TRUE),
            sd = sd(chla_ug.L, na.rm = TRUE),
            median = median(chla_ug.L, na.rm = TRUE),
            n = n())


eeavg <- ee_seasons %>%
  mutate(logAT_ratio = log(AT_ratio),
         logCT_ratio = log(CT_ratio),
         logPeakC_RU = log(PeakC_RU),
         logPeakD_RU = log(PeakD_RU),
         logPeakT_RU = log(PeakT_RU),
         log_turb_field = log(turb_field_NTU),
         log_turb_lab = log(turb_lab_NTU)) %>% 
  select(-c(site_name, Month, DOY, latitude, longitude, source, site_altname)) %>% 
  pivot_longer(cols = -c(site_code_long, site_abbr, date_ymd, Year, dist_km, Season),
               names_to = "parameter",
               values_to = "result") %>% 
  group_by(Season, site_abbr, parameter) %>% 
  summarise(parameter_mean = mean(result, na.rm = TRUE),
            parameter_sd = sd(result, na.rm = TRUE),
            n = n())

seasonal_summary <- eeavg %>% 
  select(-c(parameter_sd, n)) %>% 
  group_by(Season, parameter) %>% 
  get_summary_stats(type = "full")

ttest_res <- eeavg %>%
  group_by(parameter) %>% 
  t_test(parameter_mean ~ Season, p.adjust.method = 'bonferroni', detailed = TRUE) %>% 
  add_significance() %>% 
  select(-c(.y., ))

write_csv(ttest_res, "./R_EEMs/outputs/data/eems-seasonal-ttest.csv")


ttest_res %>% filter(grepl("sp", parameter)) %>% View()

# Lab turbidity effect size
eeavg %>% 
  filter(parameter == "log_turb_lab") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "log_turb_lab") %>% 
  summarise(sd = sd(parameter_mean))

(1.92 - 1.15) / 0.728 # 1.057692

# Field turbidity effect size
eeavg %>% 
  filter(parameter == "log_turb_field") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "log_turb_field") %>% 
  summarise(sd = sd(parameter_mean))

(1.94 - 1.22) / 0.768 # 0.9375

# Chl a effect size
eeavg %>% 
  filter(parameter == "chla_ug.L") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "chla_ug.L") %>% 
  summarise(sd = sd(parameter_mean))

(32 - 11.7) / 17.7 # 1.146893

# kT effect size
eeavg %>% 
  filter(parameter == "ext_coeff_m") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "ext_coeff_m") %>% 
  summarise(sd = sd(parameter_mean))

(1.68 - 1.16) / 0.527 # 0.9867173

# TDN effect size
eeavg %>% 
  filter(parameter == "TDN_mg.L") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "TDN_mg.L") %>% 
  summarise(sd = sd(parameter_mean))

(1.68 - 1.16) / 0.527 # 0.9867173

# spE effect size
eeavg %>% 
  filter(parameter == "spE") %>% 
  group_by(Season) %>% 
  summarise(mean = mean(parameter_mean, na.rm = TRUE))

eeavg %>% 
  ungroup() %>% 
  filter(parameter == "spE") %>% 
  summarise(sd = sd(parameter_sd, na.rm = TRUE))

(0.0113 - 0.00970) / 0.000352 # 4.545455


ee_seasons_avg <- ee_seasons %>% 
  group_by(Season, dist_km) %>% 
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



# Linear models / correlations with distance ------------------------------
log_parms_cc <- log_parms_c %>% mutate(parameter = str_remove(parameter, "log_")) 
log_parms_cc <- log_parms_cc[["parameter"]]

eems_trans_wide <- eems %>% 
  select(-c(site_name, latitude, longitude, HIX, Fmax, source, site_num, site_altname, distHaversine_km, distHaversine_m)) %>% 
  select(-contains("percent")) %>% 
  mutate(across(c(log_parms_cc), ~ log(.x)))
  
  
tttl <- eems_trans_wide %>% 
  group_by(dist_km) %>% 
  summarise(log_TDN_mg.L = mean(log_TDN_mg.L, na.rm = TRUE)) 
ttt <- eems %>% 
  group_by(dist_km) %>% 
  summarise(TDN_mg.L = mean(TDN_mg.L, na.rm = TRUE)) 

summary(lm(log_TDN_mg.L ~ dist_km, data = tttl))
summary(lm(TDN_mg.L ~ dist_km, data = ttt))


eems_mean_long <- eems_trans_wide %>% 
  select(-c(site_code_long, date_ymd, Year, Month, DOY)) %>% 
  pivot_longer(cols = -c(site_abbr, dist_km),
               names_to = "parameter", 
               values_to = "result") %>% 
  group_by(site_abbr, dist_km, parameter) %>% 
  summarise(result_mean = mean(result, na.rm = TRUE)) 


eems_mean_long %>% 
  ggplot(aes(dist_km, result_mean, col = site_abbr)) + 
  facet_wrap(~ parameter, scales = "free_y") + 
  geom_smooth(method = 'loess', se = F, col = "grey70", size = 1) +
  geom_point(size = 2) + 
  scale_color_viridis_d(option = "virids", end = 0.8) + 
  theme(legend.position = "bottom")

lmfit1 <- eems_mean_long %>% 
  split(.$parameter) %>% 
  map(~ lm(result_mean ~ dist_km, data = .)) %>% 
  # map(summary) %>% 
  map_dfr(~ broom::tidy(.), .id = 'source') %>% 
  mutate(p.value = round(p.value, 5)) %>% 
  filter(!term == "(Intercept)" & !source == "TSS_mg.L") %>% 
  rename(parameter = source) %>% 
  arrange(p.value)
 
lmfit2 <- eems_mean_long %>% 
  split(.$parameter) %>% 
  map(~ lm(result_mean ~ dist_km, data = .)) %>% 
  map(summary) %>%
  map_dfr("r.squared") %>% 
  pivot_longer(cols = -TSS_mg.L, names_to = "parameter", values_to = "r_squared") %>% 
  select(-TSS_mg.L)
  
lm_res <- lmfit1 %>% left_join(lmfit2) 

lm_res_65 <- filter(lm_res, r_squared >= 0.65)
lm_res_65c <- lm_res_65[["parameter"]]

eems_mean_long %>%
  filter(parameter %in% lm_res_65c & !grepl("Peak", parameter)) %>% 
  ggplot(aes(dist_km, result_mean, col = site_abbr)) + 
  facet_wrap(~ parameter, scales = "free_y") + 
  geom_smooth(method = 'lm', se = F, col = "grey70", size = 1) +
  geom_point(size = 2) + 
  scale_color_viridis_d(option = "virids", end = 0.8) + 
  theme(legend.position = "bottom")


# Correlations between specific fluorescence peaks ------------------------

# // Average by year ------------------------------------------------------

# humic-like peaks (spA, spC, spM, spD, spE)
humic_peaks <- eems %>%
  select(site_code_long, site_abbr, dist_km, Year, spA, spC, spM, spD, spE) %>% 
  group_by(site_code_long, site_abbr, dist_km, Year) %>% 
  summarise(across(spA:spE, ~ mean(.x, na.rm = TRUE)))

# fresh-like peaks (spB, spT, spN)
fresh_peaks <- eems %>% 
  select(site_name:dist_km, spB, spT, spN, Year) %>% 
  group_by(site_code_long, site_abbr, dist_km, Year) %>% 
  summarise(across(spB:spN, ~ mean(.x, na.rm = TRUE)))

# humic correlations
AC <- lm(spA ~ spC, data = humic_peaks)
AM <- lm(spA ~ spM, data = humic_peaks)
AD <- lm(spA ~ spD, data = humic_peaks)
AE <- lm(spA ~ spE, data = humic_peaks)
CM <- lm(spC ~ spM, data = humic_peaks)
CD <- lm(spC ~ spD, data = humic_peaks)
CE <- lm(spC ~ spE, data = humic_peaks)
MD <- lm(spM ~ spD, data = humic_peaks)
ME <- lm(spM ~ spE, data = humic_peaks)
DE <- lm(spD ~ spE, data = humic_peaks)

# fresh correlations
BT <- lm(spB ~ spT, data = fresh_peaks)
BN <- lm(spB ~ spN, data = fresh_peaks)
TN <- lm(spT ~ spN, data = fresh_peaks)

# humic summaries (R2 range: 0.89—0.99, p < 0.000001)
summary(AC) # R2 = 0.9592, p < 2.2e-16
summary(AM) # R2 = 0.9945, p < 2.2e-16
summary(AD) # R2 = 0.9260, p < 2.2e-16
summary(AE) # R2 = 0.8872, p = 5.658e-16
summary(CM) # R2 = 0.9520, p < 2.2e-16
summary(CD) # R2 = 0.9581, p < 2.2e-16
summary(CE) # R2 = 0.9287, p < 2.2e-16
summary(MD) # R2 = 0.9315, p < 2.2e-16
summary(ME) # R2 = 0.8928, p = 2.653e-16
summary(DE) # R2 = 0.9711, p < 2.2e-16

# fresh summaries (R2 range: 0.33—0.72, p < 0.001)
summary(BT) # R2 = 0.6390, p = 2.48e-08
summary(BN) # R2 = 0.3327, p = 0.0003268
summary(TN) # R2 = 0.7153, p = 6.697e-10

# humic plots
par(mfrow = c(2, 5))

plot(humic_peaks$spA, humic_peaks$spC)
plot(humic_peaks$spA, humic_peaks$spM)
plot(humic_peaks$spA, humic_peaks$spD)
plot(humic_peaks$spA, humic_peaks$spE)
plot(humic_peaks$spC, humic_peaks$spM)
plot(humic_peaks$spC, humic_peaks$spD)
plot(humic_peaks$spC, humic_peaks$spE)
plot(humic_peaks$spM, humic_peaks$spD)
plot(humic_peaks$spM, humic_peaks$spE)
plot(humic_peaks$spD, humic_peaks$spE)

# fresh plots
par(mfrow = c(1, 3))

plot(fresh_peaks$spB, fresh_peaks$spT)
plot(fresh_peaks$spB, fresh_peaks$spN)
plot(fresh_peaks$spT, fresh_peaks$spN)


# // Average overall ------------------------------------------------------

# humic-like peaks (spA, spC, spM, spD, spE)
humic_peaks <- eems %>%
  select(site_code_long, site_abbr, dist_km, spA, spC, spM, spD, spE) %>% 
  group_by(site_code_long, site_abbr, dist_km) %>% 
  summarise(across(spA:spE, ~ mean(.x, na.rm = TRUE)))

# fresh-like peaks (spB, spT, spN)
fresh_peaks <- eems %>% 
  select(site_name:dist_km, spB, spT, spN) %>% 
  group_by(site_code_long, site_abbr, dist_km) %>% 
  summarise(across(spB:spN, ~ mean(.x, na.rm = TRUE)))

# humic correlations
AC <- lm(spA ~ spC, data = humic_peaks)
AM <- lm(spA ~ spM, data = humic_peaks)
AD <- lm(spA ~ spD, data = humic_peaks)
AE <- lm(spA ~ spE, data = humic_peaks)
CM <- lm(spC ~ spM, data = humic_peaks)
CD <- lm(spC ~ spD, data = humic_peaks)
CE <- lm(spC ~ spE, data = humic_peaks)
MD <- lm(spM ~ spD, data = humic_peaks)
ME <- lm(spM ~ spE, data = humic_peaks)
DE <- lm(spD ~ spE, data = humic_peaks)

# fresh correlations
BT <- lm(spB ~ spT, data = fresh_peaks)
BN <- lm(spB ~ spN, data = fresh_peaks)
TN <- lm(spT ~ spN, data = fresh_peaks)

# humic summaries (R2 range: 0.93—0.99, p < 0.0001)
summary(AC) # R2 = 0.9893, p = 2.413e-07
summary(AM) # R2 = 0.9982, p = 1.113e-09
summary(AD) # R2 = 0.9785, p = 1.966e-06
summary(AE) # R2 = 0.9345, p = 5.643e-05
summary(CM) # R2 = 0.9958, p = 1.501e-08
summary(CD) # R2 = 0.9928, p = 7.465e-08
summary(CE) # R2 = 0.9660, p = 7.842e-06
summary(MD) # R2 = 0.9879, p = 3.519e-07
summary(ME) # R2 = 0.9523, p = 2.174e-05
summary(DE) # R2 = 0.9852, p = 6.458e-07

# fresh summaries (R2 range: 0.29—0.72, only TN significant)
summary(BT) # R2 = 0.2942, p = 0.09509
summary(BN) # R2 = 0.3139, p = 0.08626
summary(TN) # R2 = 0.7218, p = 0.004683

# humic plots
par(mfrow = c(2, 5))

plot(humic_peaks$spA, humic_peaks$spC)
plot(humic_peaks$spA, humic_peaks$spM)
plot(humic_peaks$spA, humic_peaks$spD)
plot(humic_peaks$spA, humic_peaks$spE)
plot(humic_peaks$spC, humic_peaks$spM)
plot(humic_peaks$spC, humic_peaks$spD)
plot(humic_peaks$spC, humic_peaks$spE)
plot(humic_peaks$spM, humic_peaks$spD)
plot(humic_peaks$spM, humic_peaks$spE)
plot(humic_peaks$spD, humic_peaks$spE)

# fresh plots
par(mfrow = c(1, 3))

plot(fresh_peaks$spB, fresh_peaks$spT)
plot(fresh_peaks$spB, fresh_peaks$spN)
plot(fresh_peaks$spT, fresh_peaks$spN)


# Correlations between DOC and distance -----------------------------------

# // by year --------------------------------------------------------------

eemean_year <- eems %>% 
  select(site_code_long, site_abbr, Year, dist_km, TDN_mg.L:spT) %>%
  pivot_longer(cols = -c(site_code_long:dist_km),
               names_to = "parameter",
               values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  group_by(site_code_long, site_abbr, dist_km, Year, parameter) %>% 
  summarise(result_mean = mean(result),
            result_sd = sd(result)) %>% 
  mutate(lower = result_mean - result_sd,
         upper = result_mean + result_sd) %>% 
  ungroup() 
  
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
par(mfrow = c(1, 1)); pie(rep(1, 8), col = colorBlindBlack8)

cbc <- c("#000000", "#009E73", "#CC79A7", "#0072B2")
parms <- c("DOC_mg.L", "TDN_mg.L", "SUVA", "FI", "HIX_Ohno", "BA", "S275to295")
parms_new <- c("DOC concentration", "TDN concentration", "SUVA254", "S275to295", "FI", "HIX", "BA")

p_lm_means_year <- eemean_year %>% 
  filter(parameter %in% parms) %>% 
  mutate(parameter = 
           case_when(
             parameter == "DOC_mg.L" ~ "DOC concentration",
             parameter == "TDN_mg.L" ~ "TDN concentration",
             parameter == "SUVA" ~ "SUVA254",
             parameter == "HIX_Ohno" ~ "HIX",
             TRUE ~ as.character(parameter)
           ),
         parameter = factor(parameter, levels = parms_new)) %>% 
  ggplot(aes(dist_km, result_mean, col = Year, shape = Year)) + 
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  xlim(c(0, 30)) +
  # geom_errorbar(aes(ymin = lower, ymax = upper, col = Year), width = 0.33, alpha = 3/4) +
  geom_point(aes(col = Year), size = 3, alpha = 3/4) + 
  geom_smooth(aes(col = Year), method = 'lm', se = F) +
  scale_color_manual(values = cbc) +
  theme(legend.position = "bottom") +
  labs(x = dist_lab, y = "Replicate average value")

eemean_year %>% 
  ggplot(aes(dist_km, result_mean, col = Year, shape = Year)) + 
  facet_wrap(~ parameter, scales = "free_y") +
  xlim(c(0, 30)) +
  # geom_errorbar(aes(ymin = lower, ymax = upper, col = Year), width = 0.33, alpha = 3/4) +
  geom_point(aes(col = Year), size = 1.5, alpha = 1/2) + 
  geom_smooth(aes(col = Year), method = 'lm', se = F, size = 0.75) +
  scale_color_manual(values = cbc) +
  theme(legend.position = "bottom")

eemean <- eems %>% 
  select(site_code_long, site_abbr, dist_km, DOC_mg.L) %>%
  group_by(site_code_long, site_abbr, dist_km) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            DOC_sd = sd(DOC_mg.L, na.rm = TRUE)) %>% 
  mutate(lower = DOC_mean - DOC_sd,
         upper = DOC_mean + DOC_sd) %>% 
  ungroup()
  





# // overall --------------------------------------------------------------

eemean <- eems %>% 
  select(site_code_long, site_abbr, dist_km, TDN_mg.L:spT) %>%
  group_by(site_code_long, site_abbr, dist_km) %>% 
  pivot_longer(cols = -c(site_code_long:dist_km),
               names_to = "parameter",
               values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  group_by(site_code_long, site_abbr, dist_km, parameter) %>% 
  summarise(result_mean = mean(result),
            result_sd = sd(result)) %>% 
  mutate(lower = result_mean - result_sd,
         upper = result_mean + result_sd) %>% 
  ungroup() 


parms <- c("DOC_mg.L", "SUVA", "BA", "FI", "HIX_Ohno", "S275to295", "TDN_mg.L")

p_lm_means <- eemean %>% 
  filter(parameter %in% parms) %>% 
  mutate(parameter = 
           case_when(
             parameter == "DOC_mg.L" ~ "DOC concentration",
             parameter == "TDN_mg.L" ~ "TDN concentration",
             parameter == "SUVA" ~ "SUVA254",
             parameter == "HIX_Ohno" ~ "HIX",
             TRUE ~ as.character(parameter)
           ),
         parameter = factor(parameter, levels = parms_new)) %>% 
  ggplot(aes(dist_km, result_mean)) + 
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  xlim(c(0, 30)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.75, alpha = 3/4) +
  geom_point(size = 3, col = "black", alpha = 3/4) + 
  geom_smooth(method = 'lm', se = F, col = "#CC79A7") +
  labs(x = dist_lab, y = "Replicate average value")

DOC_mean <- filter(eemean, parameter == "DOC_mg.L")
TDN_mean <- filter(eemean, parameter == "TDN_mg.L")
SUVA_mean <- filter(eemean, parameter == "SUVA")
S275to295_mean<- filter(eemean, parameter == "S275to295")
FI_mean <- filter(eemean, parameter == "FI")
HIX_mean <- filter(eemean, parameter == "HIX_Ohno")
BA_mean <- filter(eemean, parameter == "BA")

DOC_lm <- lm(DOC_mean$result_mean ~ DOC_mean$dist_km)
TDN_lm <- lm(TDN_mean$result_mean ~ TDN_mean$dist_km)
SUVA_lm <- lm(SUVA_mean$result_mean ~ SUVA_mean$dist_km)
S275to295_lm <- lm(S275to295_mean$result_mean ~ S275to295_mean$dist_km)
FI_lm <- lm(FI_mean$result_mean ~ FI_mean$dist_km)
HIX_lm <- lm(HIX_mean$result_mean ~ HIX_mean$dist_km)
BA_lm <- lm(BA_mean$result_mean ~ HIX_mean$dist_km)

summary(DOC_lm) # DOC_mg.L = 0.037819*dist_km + 4.976361, R2 = 0.941, p = 4.115e-05
summary(TDN_lm) # TDN_mg.L = 0.0047270*dist_km + 0.3383023, R2 = 0.9467, p = 3.039e-05
summary(SUVA_lm) # SUVA = —0.01848*dist_km + 2.29563, R2 = 0.9674, p = 6.897e-06
summary(S275to295_lm) # S275to295 = 0.00008951*dist_km + 0.02205, R2 = 0.9823, p = 1.106e-06
summary(FI_lm) # FI = 0.0018961*dist_km + 1.5434070, R2 = 0.9535, p = 2.008e-05
summary(HIX_lm) # HIX = —0.0010165*dist_km + 0.8395875, R2 = 0.8682, p = 0.0004708
summary(BA_lm) # BA = 0.001961*dist_km + 0.745886, R2 = 0.9353, p = 5.453e-05

p_lm_means_year
p_lm_means

p_outname <- "./R_EEMs/outputs/figures/"
dd <- "20220803_"

ggsave(paste0(p_outname, dd, "p_lm_means_year.png"), p_lm_means_year, w = 8, h = 9)
ggsave(paste0(p_outname, dd, "p_lm_means.png"), p_lm_means, w = 8, h = 9)
