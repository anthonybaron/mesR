library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
eems <- bp_doc_eems() %>% 
  select(site_name:FI, HIX_Ohno:SR, PeakA_RU:ext_coeff_m, spA:spT) # %>% 
  # filter(spB < 0.1)


# Data prep ---------------------------------------------------------------

inflow <- eems %>% filter(grepl("Inflow", site_code_long))
us_causeway <- eems %>% filter(grepl("Upstream Causeway", site_code_long)) 
ccc <- us_causeway %>% filter(site_code_long == "Upstream Causeway Centre") %>% select(DOC_mg.L)
cce <- us_causeway %>% filter(site_code_long == "Upstream Causeway East") %>% select(DOC_mg.L)

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
         distHaversine_km = 0) %>% 
  select(site_name, site_code_long, site_abbr, date_ymd, Year:distHaversine_km, everything())

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

eems$site_abbr1 <- case_when(
  eems$site_abbr == "LI" ~ "B1.7",
  eems$site_abbr == "CU" ~ "B3.8",
  eems$site_abbr == "CB" ~ "B5.2",
  eems$site_abbr == "SL" ~ "B9.0",
  eems$site_abbr == "SV" ~ "B13.0",
  eems$site_abbr == "PK" ~ "B19.8",
  eems$site_abbr == "TP" ~ "B25.2",
  eems$site_abbr == "AO" ~ "B29.1",
)
eems <- select(eems, site_name:site_abbr, site_abbr1, everything())
eems <- rename(eems, dist_km = distHaversine_km)
eems$dist_km <- eems$dist_km + 1.5
eems$dist_km <- case_when(
  eems$site_abbr1 == "B1.7" ~ 1.71,
  eems$site_abbr1 == "B3.8" ~ 3.75,
  TRUE ~ as.numeric(eems$dist_km)
)
eems$site_abbr1 <- factor(eems$site_abbr1, 
                          levels = c("B1.7", "B3.8", "B5.2", "B9.0", 
                                     "B13.0", "B19.8", "B25.2", "B29.1"))

# Parameter labels --------------------------------------------------------

A254_lab <- expression(paste(italic("A"), ""[254]*""))
A280_lab <- expression(paste(italic("A"), ""[280]*""))
A350_lab <- expression(paste(italic("A"), ""[350]*""))
A440_lab <- expression(paste(italic("A"), ""[440]*""))
AT_ratio_lab <- "Peak A:Peak T"
BA_lab <- "Freshness index<br>(&beta;:&alpha;)"
BA_lab1 <- expression(paste("Freshness Index (β:α)"))
Chla_lab <- expression(paste("Chl ", italic(a), " (µg L"^-1*")")) 
CA_ratio_lab <- "Peak C:Peak A"
CM_ratio_lab <- "Peak C:Peak M"
CT_ratio_lab <- "Peak C:Peak T"
dist_lab <- "Distance from Buffalo Pound Lake inflow (km)"
DOC_lab <- "DOC concentration<br>(mg L<sup>–1</sup>)"
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
spectral_ratio_lab <- "<i>S</i><sub>R</sub>"
secchi_lab <- "Secchi depth (m)"
SUVA_lab <- "SUVA<sub>254</sub><br>(L mg-C<sup>–1</sup> m<sup>–1</sup>)"
SUVA_lab1 <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))
TDN_lab <- "TDN concentration<br>(mg L<sup>–1</sup>)"
# TDN_lab <- expression(paste("TDN concentration (mg L"^-1*")")) 
turb_field_lab <- expression(paste("Turbidity"[field]*" (NTU)"))
turb_lab_lab <- expression(paste("Turbidity"[lab]*" (NTU)"))
spA_lab <- "spA<br>(ex260/em450)"
spB_lab <- "spB<br>(ex275/em304)"
spC_lab <- "spC<br>(ex340/em440)"
spD_lab <- "spD<br>(ex390/em510)"
spE_lab <- "spE<br>(ex290/em400)" 
spM_lab <- "spM<br>(ex300/em390)"
spN_lab <- "spN<br>(ex280/em370)"
spT_lab <- "spT<br>(ex275/em340)"



# Histograms --------------------------------------------------------------

p_histograms <- eems %>% 
  select(A254, AT_ratio, BA, CA_ratio, chla_ug.L, CM_ratio, CT_ratio, DOC_mg.L,
         ext_coeff_m, FI, HIX_Ohno, PeakA_RU, PeakB_RU, PeakC_RU, PeakD_RU,
         PeakE_RU, PeakM_RU, PeakN_RU, PeakP_RU, PeakT_RU, S275to295, 
         secchi_depth_m, SUVA, TDN_mg.L, turb_field_NTU, turb_lab_NTU) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  ggplot(aes(x = result)) + 
  facet_wrap(~ parameter, scales = "free") +
  geom_histogram() + 
  labs(x = "Values", y = "Count")



# Preliminary plots -------------------------------------------------------

# HIX <0.6 may be sign of issues, filter >0.6
eems %>% 
  ggplot(aes(BA, HIX_Ohno, colour = Year)) +
  facet_wrap(~ Year, nrow = 1) +
  geom_point(size = 3, alpha = 3/4) +
  theme(legend.position = "bottom") +
  labs(x = BA_lab,
       y = HIX_lab)

eems %>% 
  group_by(site_code_long, Year) %>% 
  rename(Site = site_code_long) %>% 
  summarise(mean_BA = mean(BA, na.rm = TRUE),
            mean_HIX = mean(HIX_Ohno, na.rm = TRUE)) %>% 
  ggplot(aes(mean_BA, mean_HIX, colour = Year)) +
  # facet_wrap(~ Year, nrow = 1) +
  geom_point(size = 3, alpha = 3/4) +
  theme(legend.position = "bottom") +
  labs(x = BA_lab,
       y = HIX_lab)


# Absorbance 
eems %>% 
  # filter(A254 < 20) %>% 
  ggplot(aes(DOC_mg.L, A254, colour = Year)) + 
  facet_wrap(~ Year, nrow = 1) +
  geom_point(size = 3, alpha = 3/4) +
  theme(legend.position = "bottom") +
  labs(x = DOC_lab, y = A254_lab)

# 3 instruments used, Iowa State, Brockport, UVM
bp_select_sites %>% 
  # filter(A254 > 5, A254 < 15, !is.na(date_ymd)) %>%
  ggplot(aes(A254, A280, colour = Year)) + 
  # geom_smooth(method = 'lm', se = F) +
  geom_point(size = 3, alpha = 3/4) +
  facet_wrap(~ Year, nrow = 1) +
  theme(legend.position = "bottom") +
  labs(x = A254_lab, y = A280_lab)

bp_select_sites %>% 
  # filter(!is.na(date_ymd), !Year == "2015", S275to295 < 0.04) %>%
  ggplot(aes(DOC_mg.L, S275to295, colour = Year)) + 
  facet_wrap(~ Year, nrow = 1) +
  # geom_boxplot() + 
  geom_point(size = 3, alpha = 3/4) +
  lims(y = c(0.015, 0.030)) +
  theme(legend.position = "bottom") +
  labs(x = DOC_lab, y = S_lab) + 
  theme(axis.title.y = ggtext::element_markdown())

bp_select_sites %>% 
  # filter(S275to295 < 0.04) %>% 
  ggplot(aes(HIX_Ohno, S275to295, col = Year)) +
  facet_wrap(~ Year, nrow = 1) +
  geom_point(size = 3, alpha = 3/4) +
  theme(legend.position = "bottom") +
  labs(x = HIX_lab, y = S_lab) + 
  theme(axis.title.y = ggtext::element_markdown())

bp_select_sites %>% 
  # filter(S275to295 < 0.04) %>% 
  ggplot(aes(S350to400, S275to295, col = Year)) +
  facet_wrap(~ Year, nrow = 2) +
  geom_abline(intercept = 0, lty = 2, alpha = 3/4) +
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0.015, 0.030),
       y = c(0.015, 0.030)) +
  theme(legend.position = "bottom") +
  labs(x = S_lab2, y = S_lab) + 
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())

bp_select_sites %>% 
  ggplot(aes(HIX_Ohno, SUVA, col = Year)) +
  facet_wrap(~ Year, nrow = 2) +
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(NA, 0.90)) +
  theme(legend.position = "bottom") +
  labs(x = HIX_lab, y = SUVA_lab) 

bp_select_sites %>% 
  ggplot(aes(BA, FI, col = Year, shape = Year)) +
  # facet_wrap(~ Year, nrow = 2) +
  geom_point(size = 3, alpha = 3/4) +
  # lims(x = c(NA, 0.90)) +
  theme(legend.position = "bottom") +
  labs(x = BA_lab, y = FI_lab) 



# Parameter plots ---------------------------------------------------------

plot_mean_sd <- function(parm = "", parm_lab = "") { 
  
  df <- eems %>% 
    # select(-c(Fmax, TSS_mg.L, source, site_altname, PeakA_percent:PeakT_percent,
    #           site_num, distHaversine_m)) %>% 
    pivot_longer(cols = c(TDN_mg.L:spT), 
                 names_to = "parameter", 
                 values_to = "result") %>% 
    mutate(parameter = factor(parameter),
           Site = site_code_long) %>% 
    group_by(Site, site_abbr1, Year, dist_km, parameter) %>% 
    summarise(mean_parameter = mean(result, na.rm = TRUE),
              sd_parameter = sd(result, na.rm = TRUE)) %>% 
    mutate(lower = mean_parameter - sd_parameter,
           upper = mean_parameter + sd_parameter) %>% 
    ungroup() 
    
  p_mean_sd <- df %>% 
    filter(parameter == parm & !is.na(mean_parameter)) %>%
    # filter(parameter == parm) %>%
    ggplot(aes(dist_km, mean_parameter)) + 
    facet_wrap(~ Year, nrow = 1) +
    xlim(c(0, 30)) +
    geom_line() +
    geom_errorbar(aes(ymin = lower, ymax = upper, col = site_abbr1), width = 0.33) +
    geom_point(aes(col = site_abbr1), size = 3) + 
    scale_color_viridis_d(begin = 0, end = 0.8) +
    theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) +
    guides(col = guide_legend(nrow = 1)) + 
    labs(x = dist_lab, y = parm_lab, col = "Site")
    
  return(p_mean_sd)
  
}



p_outname <- "./R_EEMs/outputs/figures/"
dd <- "20220926_"

# Water quality plots

# // Water quality --------------------------------------------------------

### Turbditiy, Chl a, extinction coefficient, Secchi depth (main text)
p_m_sd_fieldturb <- plot_mean_sd(parm = "turb_field_NTU", parm_lab = "Turbidity (NTU)") + ylim(c(NA, 100)) + labs(x = NULL, tag = 'a')
# p_m_sd_labturb <- plot_mean_sd(parm = "turb_lab_NTU", parm_lab = turb_lab_lab) + labs(x = NULL, tag = 'b') # not needed
p_m_sd_extcoeff <- plot_mean_sd(parm = "ext_coeff_m", parm_lab = extinction_coefficient_lab) + ylim(c(0, NA)) + labs(x = NULL, tag = 'b')
p_m_sd_secchi <- plot_mean_sd(parm = "secchi_depth_m", parm_lab = secchi_lab) + scale_y_reverse() + labs(x = NULL, tag = 'c')
p_m_sd_Chla <- plot_mean_sd(parm = "chla_ug.L", parm_lab = Chla_lab) + labs(tag = 'd')

p_wq <- (p_m_sd_fieldturb / p_m_sd_extcoeff / p_m_sd_secchi / p_m_sd_Chla) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_wq.png"), p_wq, w = 8, h = 9)

#

### TDN and DOC (main text)
p_m_sd_TDN <- plot_mean_sd(parm = "TDN_mg.L", parm_lab = TDN_lab) + ylim(c(0.2, 0.6)) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'a')
p_m_sd_DOC <- plot_mean_sd(parm = "DOC_mg.L", parm_lab = DOC_lab) + theme(axis.title.y = element_markdown()) + labs(tag = 'b')

p_tdn_doc <- (p_m_sd_TDN / p_m_sd_DOC) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_tdn_doc.png"), p_tdn_doc, w = 8, h = 5.5)


# // Absorbance and fluorescence ------------------------------------------

### Absorbance (Appendix)
p_m_sd_A254 <- plot_mean_sd(parm = "A254", parm_lab = A254_lab) + ylim(c(7, NA)) + labs(x = NULL, tag = 'a') + theme(axis.text.x = element_blank())
p_m_sd_A280 <- plot_mean_sd(parm = "A280", parm_lab = A280_lab) + ylim(c(NA, 12)) + labs(x = NULL, tag = 'b') + theme(axis.text.x = element_blank())
p_m_sd_A350 <- plot_mean_sd(parm = "A350", parm_lab = A350_lab) + labs(x = NULL, tag = 'c') + theme(axis.text.x = element_blank())
p_m_sd_A440 <- plot_mean_sd(parm = "A440", parm_lab = A440_lab) + labs(tag = 'd')

p_abs <- (p_m_sd_A254 / p_m_sd_A280 / p_m_sd_A350 / p_m_sd_A440) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_abs.png"), p_abs, w = 7.2, h = 7.4)

#

# Absorbance and fluorescence indexes and spectral slope (275—295 nm) (main text)
p_m_sd_SUVA <- plot_mean_sd(parm = "SUVA", parm_lab = SUVA_lab) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'a')
p_m_sd_S275to295 <- plot_mean_sd(parm = "S275to295", parm_lab = S_lab) + theme(axis.title.y = element_markdown()) + ylim(c(0.018, 0.026)) + labs(x = NULL, tag = 'b')
p_m_sd_FI <- plot_mean_sd(parm = "FI", parm_lab = FI_lab) + ylim(c(1.50, 1.65)) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'c')
p_m_sd_HIX <- plot_mean_sd(parm = "HIX_Ohno", parm_lab = HIX_lab) + ylim(c(NA, 0.88)) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'd')
p_m_sd_BA <- plot_mean_sd(parm = "BA", parm_lab = BA_lab) + ylim(c(0.69, 0.84)) + theme(axis.title.y = element_markdown()) + labs(tag = 'e')

p_abs_flor <- (p_m_sd_SUVA / p_m_sd_S275to295 / p_m_sd_FI / p_m_sd_HIX / p_m_sd_BA) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_abs_flor.png"), p_abs_flor, w = 8, h = 10)

# 

# spectral slope (350—400 nm) and spectral slope ratio (Appendix)
p_m_sd_S350to400 <- plot_mean_sd(parm = "S350to400", parm_lab = S_lab2) + theme(axis.title.y = element_markdown()) + ylim(c(NA, 0.022)) + labs(x = NULL, tag = 'a')
p_m_sd_SR <- plot_mean_sd(parm = "SR", parm_lab = spectral_ratio_lab) + theme(axis.title.y = element_markdown()) + labs(tag = 'b') 

p_slopes <- (p_m_sd_S350to400 / p_m_sd_SR) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_slopes.png"), p_slopes, w = 8, h = 5)



# // Spectral peaks -------------------------------------------------------

### Humic-like peaks (A, C, M, D, E)
p_m_sd_PeakA <- plot_mean_sd(parm = "PeakA_RU", parm_lab = PeakA_lab) + ylim(c(0.6, 1.5)) + labs(x = NULL, tag = 'a')
p_m_sd_PeakC <- plot_mean_sd(parm = "PeakC_RU", parm_lab = PeakC_lab) + ylim(c(0.35, NA)) + labs(x = NULL, tag = 'b')
p_m_sd_PeakM <- plot_mean_sd(parm = "PeakM_RU", parm_lab = PeakM_lab) + labs(x = NULL, tag = 'c')
p_m_sd_PeakD <- plot_mean_sd(parm = "PeakD_RU", parm_lab = PeakD_lab) + labs(tag = 'd')
p_m_sd_PeakE <- plot_mean_sd(parm = "PeakE_RU", parm_lab = PeakE_lab) + labs(x = NULL, tag = 'e')

p_humic_peaks <- (p_m_sd_PeakA / p_m_sd_PeakC / p_m_sd_PeakM / p_m_sd_PeakD / p_m_sd_PeakE) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_humic_peaks.png"), p_humic_peaks, w = 8, h = 10)

### Specific humic-like peaks (spA, spC, spM, spD, spE)
p_m_sd_spA <- plot_mean_sd(parm = "spA", parm_lab = spA_lab) + ylim(c(0.10, NA)) +  theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'a')
p_m_sd_spC <- plot_mean_sd(parm = "spC", parm_lab = spC_lab) + ylim(c(0.06, NA)) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'b')
p_m_sd_spM <- plot_mean_sd(parm = "spM", parm_lab = spM_lab) + ylim(c(0.06, 0.16)) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'c')
p_m_sd_spD <- plot_mean_sd(parm = "spD", parm_lab = spD_lab) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'd')
p_m_sd_spE <- plot_mean_sd(parm = "spE", parm_lab = spE_lab) + ylim(c(NA, 0.016)) + theme(axis.title.y = element_markdown()) + labs(tag = 'e')

p_humic_sp_peaks <- (p_m_sd_spA / p_m_sd_spC / p_m_sd_spM / p_m_sd_spD / p_m_sd_spE) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_humic_sp_peaks.png"), p_humic_sp_peaks, w = 8, h = 10)

#

### Fresh-like peaks (B, T, N)
p_m_sd_PeakB <- plot_mean_sd(parm = "PeakB_RU", parm_lab = PeakB_lab) + ylim(c(0.1, NA)) + labs(x = NULL, tag = 'a')
p_m_sd_PeakT <- plot_mean_sd(parm = "PeakT_RU", parm_lab = PeakT_lab) + ylim(c(NA, 0.45)) + labs(x = NULL, tag = 'b')
p_m_sd_PeakN <- plot_mean_sd(parm = "PeakN_RU", parm_lab = PeakN_lab) + ylim(c(NA, 0.45)) + labs(tag = 'c')

p_fresh_peaks <- (p_m_sd_PeakB / p_m_sd_PeakT / p_m_sd_PeakN) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_fresh_peaks.png"), p_fresh_peaks, w = 8, h = 6.5)

### Specific fresh-like peaks (spB, spT, spN)
p_m_sd_spB <- plot_mean_sd(parm = "spB", parm_lab = spB_lab) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'a')
p_m_sd_spT <- plot_mean_sd(parm = "spT", parm_lab = spT_lab) + theme(axis.title.y = element_markdown()) + labs(x = NULL, tag = 'b')
p_m_sd_spN <- plot_mean_sd(parm = "spN", parm_lab = spN_lab) + ylim(c(0.05, 0.10)) + theme(axis.title.y = element_markdown()) + labs(tag = 'c')

p_fresh_sp_peaks <- (p_m_sd_spB / p_m_sd_spT / p_m_sd_spN) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_fresh_sp_peaks.png"), p_fresh_sp_peaks, w = 8, h = 6.5)


#

### Peak ratios (CT, AT, CA, CM)
p_m_sd_CTratio <- plot_mean_sd(parm = "CT_ratio", parm_lab = CT_ratio_lab) + labs(x = NULL, tag = 'a')
p_m_sd_ATratio <- plot_mean_sd(parm = "AT_ratio", parm_lab = AT_ratio_lab) + labs(x = NULL, tag = 'b')
p_m_sd_CAratio <- plot_mean_sd(parm = "CA_ratio", parm_lab = CA_ratio_lab) + labs(x = NULL, tag = 'c')
p_m_sd_CMratio <- plot_mean_sd(parm = "CM_ratio", parm_lab = CM_ratio_lab) + labs(tag = 'd')

p_peak_ratios <- (p_m_sd_CTratio / p_m_sd_ATratio / p_m_sd_CAratio / p_m_sd_CMratio) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(p_outname, dd, "p_peak_ratios.png"), p_peak_ratios, w = 8, h = 8.7)




eems %>% 
  # filter(turb_field_NTU < 80) %>%
  ggplot(aes(turb_field_NTU, turb_lab_NTU)) + 
  geom_point() + 
  geom_abline(intercept = c(0, 0))

eems %>% 
  select(site_code_long, turb_field_NTU, turb_lab_NTU) %>% 
  pivot_longer(cols = -site_code_long, names_to = "parameter", values_to = "result") %>% View()
  ggplot(aes(parameter, result)) + 
  geom_boxplot() + 
  labs(x = NULL, y = "Turbidity (NTU)")
