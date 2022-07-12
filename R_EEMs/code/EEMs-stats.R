library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(rstatix)

theme_set(theme_bw(base_size = 14) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
eems <- bp_doc_eems() %>% mutate(distHaversine_km = distHaversine_km + 1.5)


# Can we average some sites? ----------------------------------------------

### Inflow sites (East, Centre, West)
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

doc_aov <- inflow %>% anova_test(DOC_mg.L ~ site_code_long) # log-trans doesn't change outcome
suva_aov <- inflow %>% anova_test(SUVA ~ site_code_long)
ba_aov <- inflow %>% anova_test(BA ~ site_code_long)
hix_aov <- inflow %>% anova_test(HIX_Ohno ~ site_code_long)

# DOC:  F(2, 52) = 0.126, p = 0.882, ges = 0.005
# SUVA: F(2, 52) = 0.002, p = 0.998, ges = 7.62e-05
# BA:   F(2, 52) = 0.064, p = 0.938, ges = 0.002
# HIX:  F(2, 52) = 0.088, p = 0.916, ges = 0.003 



### Upstream Causeway sites (Upstream Causeway Centre, Upstream Causeway East)
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

