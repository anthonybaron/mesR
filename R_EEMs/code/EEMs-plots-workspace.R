library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
bp_select_sites <- bp_doc_eems()

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
TDN_lab <- expression(paste("TDN concentration (mg L"^-1*")")) 
Chla_lab <- expression(paste("Chl a concentration (µg L"^-1*")")) 

p_outname <- "./R_EEMs/outputs/figures/"
dd <- "20220706_"


# Data prep ---------------------------------------------------------------

eems_dates <- function(df = bp_select_sites) {
  
  df <- df %>% select(date_ymd, Year, doy = DOY, DOC_mg.L)
  
  return(df)
  
}


# Histograms --------------------------------------------------------------

bp_select_sites %>% 
  mutate(Year = as.numeric(Year)) %>% 
  select(Year, distHaversine_km:ext_coeff_m) %>% 
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "result") %>% 
  filter(!is.na(result)) %>% 
  ggplot(aes(x = result)) + 
  geom_histogram() + 
  facet_wrap(~ parameter, scales = "free")




  # Dissolved organic carbon (DOC) ------------------------------------------
p_DOC_doy <- bp_select_sites %>%
  filter(!is.na(DOC_mg.L)) %>%
  ggplot(aes(DOY, DOC_mg.L, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(size = 1.5) +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = DOC_lab,
       subtitle = "DOC concentration")

# ggsave("./R_EEMs/outputs/figures/p_DOC_doy_20220529.png", p_DOC_doy, w = 8.78, h = 5.95)

p_DOC_box <- bp_select_sites %>%
  rename(`Day of year` = DOY) %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(site_code_long, DOC_mg.L)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot(alpha = 9/10) +
  geom_point(aes(col = `Day of year`), size = 2) +
  scale_color_viridis_c(end = 0.8, option = 'virids') + 
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = DOC_lab)

# 1440 x 795
# ggsave("./R_EEMs/outputs/figures/p_DOC_box_20220529.png", p_DOC_box, w = 14.4, h = 7.95)


# bp_select_sites %>% 
#   filter(!is.na(DOC_mg.L)) %>% 
#   ggplot(aes(distHaversine_m, DOC_mg.L, group = site_code_long, fill = site_code_long)) + 
#   facet_wrap(~ Year, ncol = 4) + 
#   geom_boxplot() + 
#   theme(legend.position = 'bottom') +
#   labs(x = "Distance (m)", y = DOC_lab)

p_dist_doc_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_DOC = mean(DOC_mg.L), 
            sd_DOC = sd(DOC_mg.L)) %>% 
  mutate(upper = mean_DOC + sd_DOC,
         lower = mean_DOC - sd_DOC) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_DOC)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = DOC_lab)

ggsave(paste0(p_outname, dd, "DOC_distance.png"), p_dist_doc_facet, w = 7.2, h = 5.3)

abbr_levs <- c('IE','IC','IW','CC','CE','CB','SL','SV','PK','TP','AO')

bp_select_sites <- bp_select_sites %>% 
  mutate(site_abbr = ifelse(site_num == 1, "IE",
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

bp_select_sites %>% 
  rename(Site = site_abbr) %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_DOC = mean(DOC_mg.L), 
            sd_DOC = sd(DOC_mg.L)) %>% 
  mutate(upper = mean_DOC + sd_DOC,
         lower = mean_DOC - sd_DOC) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_DOC)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = DOC_lab)


p_dist_doc_mean <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  group_by(Site, distHaversine_km) %>% 
  summarise(mean_DOC = mean(DOC_mg.L), 
            sd_DOC = sd(DOC_mg.L)) %>% 
  mutate(upper = mean_DOC + sd_DOC,
         lower = mean_DOC - sd_DOC) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_DOC)) + 
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  ggpubr::theme_classic2(base_size = 14) + 
  theme(legend.position = 'none') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = DOC_lab)

p_dist_doc_mean_legend <- p_dist_doc_mean + theme(legend.position = "bottom")

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

shared_legend <- extract_legend(p_dist_doc_mean_legend)

p_combined <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p_dist_doc_mean, p_dist_doc_facet, ncol = 2),
                                      shared_legend, nrow = 2, heights = c(9, 2))



pp1 <- p_dist_doc_mean + p_dist_doc_facet & theme(legend.position = 'bottom')
pp2 <- pp1 + plot_layout(guides = "collect")

p_combined <- (p_dist_doc_mean + p_dist_doc_facet) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

combined <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
doc_weekly_raw <- DOC_complete_1990_2019()
doc_weekly <- doc_weekly_raw %>% 
  filter(year %in% c(2016:2019)) %>% 
  mutate(rownum = 1:length(date_ymd),
         eems_season = ifelse(month %in% 5:9, "yes", "no"))

doc_weekly %>% 
  ggplot(aes(rownum, DOC_mg.L)) + 
  geom_line(size = 1, col = "steelblue") +
  scale_x_continuous(labels = c("2016", "2017", "2018", "2019", "2020")) +
  labs(x = "Year", y = DOC_lab,
       subtitle = "Water treatment plant DOC concentration") 

p_DOC_weekly <- doc_weekly %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line(size = 1, col = "white") +
  geom_vline(xintercept = as.numeric(as.Date("2016-05-01")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2016-09-30")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2017-05-01")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2017-09-30")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-01")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2018-09-30")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-01")), col = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-09-30")), col = "steelblue", size = 1) +
  geom_line(size = 1, col = "black") +
  labs(x = "Year", y = DOC_lab,
       subtitle = "Water treatment plant DOC concentration. Between blue lines is\nMay to September of each year") 

p_DOC_weekly + p_DOC_doy


# Absorbance at 254 nm (A254) ---------------------------------------------
bp_select_sites %>%
  rename(Site = site_name) %>% 
  filter(!is.na(A254) & !Year == 2015& A254 < 25) %>% 
  ggplot(aes(DOY, A254, colour = Year)) +
  facet_wrap(~ Site, ncol = 3) +
  geom_line() +
  geom_point() +
  
  theme(legend.position = "bottom") +
  labs(x = "Day of year", 
       subtitle = "Absorbance at 254 nm")

bp_select_sites %>% 
  rename(Site = site_name) %>% 
  filter(!Year == 2015 & A254 < 25) %>% 
  ggplot(aes(DOC_mg.L, A254, col = Site)) + 
  facet_wrap(~ Year, nrow = 1) +
  geom_point() + 
  # 
  theme(legend.position = "bottom") +
  labs(x = DOC_lab, y = "Absorbance at 254 nm")

p_A254_doy <- bp_select_sites %>%
  filter(!is.na(A254)) %>%
  ggplot(aes(DOY, A254, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(size = 1.5) +
  # 
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = expression(paste("A"[254]*"")),
       subtitle = "A254")

# ggsave("./R_EEMs/outputs/figures/p_A254_doy_20220529.png", p_A254_doy, w = 8.78, h = 5.95)


p_A254_box <- bp_select_sites %>%
  filter(!Year == 2015 & A254 < 25) %>% 
  ggplot(aes(site_code_long, A254, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  # lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = expression(paste("A"[254]*"")),
       subtitle = "A254: A measure of how much light is absorbed by a sample\nHigher values indicate higher absorption")

# ggsave("./R_EEMs/outputs/figures/p_A254_box_20220523.png", p_A254_box, w = 14.4, h = 7.95)

p_A254_doy + p_A254_box

# DOC-specific ultraviolet absorbance (SUVA) ------------------------------
SUVA_lab <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))

bp_select_sites %>%
  filter(!is.na(SUVA)) %>% 
  ggplot(aes(DOY, SUVA, colour = Year)) +
  facet_wrap(~ site_name, ncol = 5) +
  geom_line() +
  geom_point() +
  
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = SUVA_lab)

bp_select_sites %>% 
  filter(!Year == 2015 & A254 < 25) %>% 
  ggplot(aes(DOC_mg.L, SUVA, col = site_name)) + 
  facet_wrap(~ Year, nrow = 1) + 
  geom_point() + 
  # 
  theme(legend.position = "bottom") 

p_SUVA_doy <- bp_select_sites %>%
  filter(!Year == 2015 & A254 < 25) %>% 
  ggplot(aes(DOY, SUVA, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(size = 1.5) +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = SUVA_lab,
       subtitle = "SUVA")

p_SUVA_box <- bp_select_sites %>%
  filter(!Year == 2015 & A254 < 25) %>% 
  ggplot(aes(site_code_long, SUVA, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  # lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = SUVA_lab,
       subtitle = "Higher values associated with higher aromatic DOC content")

# subtitle = "SUVA: Absorption coefficient at 254 nm divided by\nDOC concentration. Higher values associated with higher\naromatic DOC content")

# ggsave("./R_EEMs/outputs/figures/p_SUVA_box_20220523.png", p_SUVA_box, w = 14.4, h = 7.95)

p_SUVA_doy + p_SUVA_box


p_dist_suva_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(SUVA)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_SUVA = mean(SUVA), 
            sd_SUVA = sd(SUVA)) %>% 
  mutate(upper = mean_SUVA + sd_SUVA,
         lower = mean_SUVA - sd_SUVA) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_SUVA)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = SUVA_lab)

ggsave(paste0(p_outname, dd, "SUVA_distance.png"), p_dist_suva_facet, w = 7.2, h = 5.3)

# Total dissolved nitrogen (TDN) ------------------------------------------
p_TDN_box <- bp_select_sites %>%
  filter(!is.na(TDN_mg.L)) %>% 
  ggplot(aes(site_code_long, TDN_mg.L, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = TDN_lab,
       subtitle = "TDN concentration")

p_TDN_doy <- bp_select_sites %>%
  filter(!is.na(DOC_mg.L) & !is.na(TDN_mg.L)) %>% 
  ggplot(aes(DOY, TDN_mg.L, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 2) +
  geom_line() +
  geom_point(size = 1.5) +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = TDN_lab,
       subtitle = "TDN concentration")

p_TDN_doy + p_TDN_box

bp_select_sites %>% 
  filter(!is.na(TDN_mg.L) & !is.na(DOC_mg.L)) %>% 
  ggplot(aes(DOC_mg.L, TDN_mg.L, col = Year)) +
  facet_wrap(~ site_code_long) +
  geom_point() + 
  theme(legend.position = "bottom") 

bp_select_sites %>% 
  filter(!is.na(DOC_mg.L) & !is.na(TDN_mg.L)) %>% 
  mutate(`DOC:TDN` = DOC_mg.L / TDN_mg.L) %>% 
  ggplot(aes(site_code_long, `DOC:TDN`, fill = Year)) + 
  facet_wrap(~ Year, ncol = 2) + 
  geom_boxplot() + 
  coord_flip()

bp_select_sites %>% 
  filter(Year %in% c(2017:2018)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, col = site_code_long)) + 
  facet_wrap(~ Year, ncol = 2) + 
  geom_point() +
  theme(legend.position = 'none')

bp_select_sites %>% 
  filter(Year %in% c(2017:2018)) %>% 
  ggplot(aes(yday(date_ymd), TDN_mg.L, col = site_code_long)) + 
  facet_wrap(~ Year, ncol = 2) + 
  geom_point() +
  theme(legend.position = 'bottom')

bp_select_sites %>% 
  filter(!is.na(DOC_mg.L) & !is.na(TDN_mg.L)) %>% 
  mutate(`DOC:TDN` = DOC_mg.L / TDN_mg.L) %>% 
  ggplot(aes(yday(date_ymd), `DOC:TDN`, col = site_code_long)) + 
  facet_wrap(~ Year, ncol = 2) + 
  geom_point() +
  theme(legend.position = 'bottom')

  



# Spectral slope (275–295 nm) ---------------------------------------------

S275to295_lab <- expression(paste(italic("S"), "[275-295]*"))

p_spectral_slope_doy <- bp_select_sites %>%
  filter(!is.na(S275to295) & S275to295 >= 0.018) %>% 
  ggplot(aes(DOY, S275to295, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  theme(legend.position = "bottom") +
  lims(y = c(0.0175, 0.0275)) +
  labs(x = "Day of year", y = expression(paste("S"[275-295]*"")),
       subtitle = "Spectral slope (275–295 nm)")

p_spectral_slope_box <- bp_select_sites %>%
  filter(!is.na(S275to295) & S275to295 >= 0.018) %>% 
  ggplot(aes(site_code_long, S275to295, fill = Year)) +
  facet_wrap(~ Year, ncol = 5) +
  geom_boxplot() +
  coord_flip() +
  lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = NULL, y = "Spectral slope (275–295 nm)",
       subtitle = "Spectral slope (275–295 nm): Higher S values indicate low molecular weight\ncompounds and/or decreasing aromaticity")

p_spectral_slope_doy + p_spectral_slope_box

# ggsave("./R_EEMs/outputs/figures/p_spectral_slope_box_20220523.png", p_spectral_slope_box, w = 14.4, h = 7.95)



p_dist_spectral_slope_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(S275to295)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_S275to295 = mean(S275to295), 
            sd_S275to295 = sd(S275to295)) %>% 
  mutate(upper = mean_S275to295 + sd_S275to295,
         lower = mean_S275to295 - sd_S275to295) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_S275to295)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Spectral slope (275–295 nm)")

ggsave(paste0(p_outname, dd, "S275to295_distance.png"), p_dist_spectral_slope_facet, w = 7.2, h = 5.3)

# Freshness Index (β:α ratio) ---------------------------------------------

BA_lab <- expression(paste("Freshness index (", italic(β), ":", italic(α), ")"))

p_freshness_index_doy <- bp_select_sites %>%
  filter(!is.na(BA) & !Year == 2015) %>%
  ggplot(aes(DOY, BA, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_hline(yintercept = 0.80, col = "grey30") +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "Freshness Index",
       subtitle = "Freshness index (β:α ratio)")

p_freshness_index_box <- bp_select_sites %>%
  filter(!is.na(BA) & !Year == 2015) %>%
  ggplot(aes(site_code_long, BA, fill = Year)) +
  facet_wrap(~ Year, ncol = 5) +
  geom_hline(yintercept = 0.80, lwd = 1, lty = 4, col = "grey45") +
  geom_boxplot() +
  # geom_point() +
  coord_flip() +
  # lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = c(0.70, 0.80)) +
  theme(legend.position = 'bottom') +
  # theme(legend.position = "bottom",
  #       axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = NULL, y = expression(paste("Freshness Index (", italic("β"), ":", italic("α"), ")")),
       subtitle = "Higher values indicate more recently produced 'fresh' DOM\nValues < 0.80 indicate DOM pool is more degraded than fresh")

# "Freshness Index: Indicator of new 'fresh-like' DOM\nHigher values indicate more fresh DOM\nValues < 0.80 indicate DOM pool is more degraded than fresh"

# ggsave("./R_EEMs/outputs/figures/p_freshness_index_box_20220523.png", p_freshness_index_box, w = 14.4, h = 7.95)

p_freshness_index_doy + p_freshness_index_box

# The Freshness Index (also called β:α ratio) provides an indication of DOM
# degradation state. DOM is more degraded and broken down as the freshness index
# decreases. However, Freshness Index values below 0.80 tend to suggest DOM
# that's more degraded than being freshly produced. In general, the Freshness
# Index suggests the inputs of freshly produced DOM increase from upstream to
# downstream. 
# Indicator of recently produced fresh-like DOM; higher values
# indicate greater proportion of fresh DOM.

p_dist_BA_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(BA)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_BA = mean(BA), 
            sd_BA = sd(BA)) %>% 
  mutate(upper = mean_BA + sd_BA,
         lower = mean_BA - sd_BA) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_BA)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  # ylim(c(0.68, NA)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = BA_lab)

ggsave(paste0(p_outname, dd, "BA_distance.png"), p_dist_BA_facet, w = 7.2, h = 5.3)


# Humification Index (HIX) ------------------------------------------------
bp_select_sites %>%
  filter(!is.na(HIX_Ohno) & !Year == 2015) %>%
  mutate(HIX_Ohno = HIX_Ohno / 10) %>% 
  summarise(meanHIX = mean(HIX_Ohno),
            medianHIX = median(HIX_Ohno))

p_HIX_doy <- bp_select_sites %>%
  filter(!is.na(HIX_Ohno) & !Year == 2015) %>%
  ggplot(aes(DOY, HIX_Ohno, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  lims(y = c(0.7, 0.9)) +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "Humification index",
       subtitle = "Humification index")

p_HIX_box <- bp_select_sites %>%
  filter(!is.na(HIX_Ohno) & !Year == 2015) %>%
  ggplot(aes(site_code_long, HIX_Ohno, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  lims(y = c(0.7, 0.9)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  # scale_y_continuous(breaks = c(0.70, 0.80, 0.90), labels = c(0.70, 0.80, 0.90)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Humification Index",
       subtitle = 'Values closer to 1 indicate more humic (terrestrial) DOM')

# subtitle = 'Humification index: Values closer to 1 indicate higher proportion\nof humic materials')

# ggsave("./R_EEMs/outputs/figures/p_HIX_box_20220523.png", p_HIX_box, w = 14.4, h = 7.95)

p_HIX_doy + p_HIX_box

# The Humification Index (HIX) provides an indication of the humic content of
# the DOM. HIX is a fractional value. A HIX of 1 indicates a sample is 100%
# humic substances Indicator of humic substance content or extent of DOM
# humification Higher values indicate greater humification

p_dist_HIX_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(HIX_Ohno)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_HIX = mean(HIX_Ohno), 
            sd_HIX = sd(HIX_Ohno)) %>% 
  mutate(upper = mean_HIX + sd_HIX,
         lower = mean_HIX - sd_HIX) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_HIX)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  ylim(c(NA, 0.9)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Humification index")

ggsave(paste0(p_outname, dd, "HIX_distance.png"), p_dist_HIX_facet, w = 7.2, h = 5.3)

# Fluorescence Index (FI) -------------------------------------------------
bp_select_sites %>%
  filter(!is.na(FI) & !Year == 2015 & FI < 1.69) %>%
  summarise(meanFI = mean(FI),
            medianFI = median(FI))

bp_select_sites %>% 
  arrange(desc(FI)) %>% 
  select(FI, everything())

p_FI_doy <- bp_select_sites %>%
  filter(!is.na(FI) & !Year == 2015 & FI < 1.69) %>%
  ggplot(aes(DOY, FI, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "Fluorescence index",
       subtitle = "Fluorescence index")

p_FI_box <- bp_select_sites %>%
  filter(!is.na(FI) & !Year == 2015 & FI < 1.69) %>%
  ggplot(aes(site_code_long, FI, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  # geom_point() +
  coord_flip() +
  # scale_y_continuous(breaks = c(1.30, 1.50, 1.70), labels = c(1.30, 1.50, 1.70)) +
  lims(y = c(1.3, 1.7)) +
  scale_x_discrete(limits = rev) +
  geom_hline(yintercept = 1.3, lwd = 1, lty = 4, col = 'darkgreen', alpha = 1/2) + 
  geom_hline(yintercept = 1.7, lwd = 1, lty = 4, col = 'steelblue', alpha = 1/2) + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = NULL, y = "Fluorescence Index",
       subtitle = "Terrestrial DOM ~1.3 and microbial (algal) DOM ~1.7\nUsed with caution as FI can change independent of source")

# subtitle = "Fluorescence index: Indicator of DOM source (terrestrial ~1.3 and\nmicrobial ~1.7) but should be used with caution as FI can change\nindependent of source")

# ggsave("./R_EEMs/outputs/figures/p_FI_box_20220523.png", p_FI_box, w = 14.4, h = 7.95)

p_FI_doy + p_FI_box

# The Fluorescence Index (FI) was developed to provide an indication of DOM
# source (terrestrial ~ 1.3 and microbial ~ 1.7). FI, however, can change
# independent of source and caution should be used if applying FI as the sole
# indicator of source. Generally, FI was lowest in the Qu'appelle River and
# similar across Buffalo Pound. There is some evidence that FI increases from
# upstream to downstream, suggesting as with SR and Freshness Index, that
# internal production of DOM occurs as water flows through Buffalo Pound.
# 
# Identifies relative proportions of terrestrial and microbial sources to the
# DOM pool

p_dist_FI_facet <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(FI)) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_FI = mean(FI), 
            sd_FI = sd(FI)) %>% 
  mutate(upper = mean_FI + sd_FI,
         lower = mean_FI - sd_FI) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_FI)) + 
  facet_wrap(~ Year, nrow = 2) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(end = 0.8) +
  # theme_classic2() +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Fluorescence index")

ggsave(paste0(p_outname, dd, "FI_distance.png"), p_dist_FI_facet, w = 7.2, h = 5.3)

# Spectral slope ratio (SR) -----------------------------------------------
p_SR_doy <- bp_select_sites %>%
  filter(!is.na(SR) & !Year == 2015) %>%
  ggplot(aes(DOY, SR, colour = Year)) +
  facet_wrap(~ site_code_long, ncol = 3) +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "Spectral slope ratio",
       subtitle = "Spectral slope ratio")

p_SR_box <- bp_select_sites %>%
  filter(!is.na(SR) & !Year == 2015) %>%
  ggplot(aes(site_code_long, SR, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  # geom_point() +
  coord_flip() +
  # lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Spectral slope ratio",
       subtitle = "The spectral slope ratio (SR) is an indicator  of DOM size\nDOM decreases in size with increasing SR\nSR negatively correlated with DOM molecular weight\nHigher SR = lower DOM molecular weight/smaller DOM molecules")

# The Spectral Slope Ratio (SR) provides an indication of dissolved organic
# matter (DOM) size. DOM decreases in size with increasing SR. The clearest
# pattern is that Pelican Lake has smaller sized DOM than Buffalo Pound. The
# Qu'appelle River tended to have similar sized DOM as the upper part of Buffalo
# Pound. SR seems to be higher (smaller sized DOM) as you move toward the
# outlet, but the peak values seem to occur in different locations depending on
# the date. These data would suggest the DOM pool shifts toward more internally
# produced DOM and/or photo/biochemically degraded DOM from the upstream to
# downstream.
# 
# SR negatively correlated with DOM molecular weight
# Higher SR = lower DOM molecular weight / smaller DOM molecules

p_SR_doy + p_SR_box



# Peak ratios -------------------------------------------------------------

# A:T --- Measure of humic-like (recalcitrant) versus fresh-like (labile) fluorescence
# C:A --- Measure of the relative amount of photosensitive humic-like DOM fluorescence
# C:M --- Measure of the amount of diagenetically-altered (blue-shifted) fluorescence
# C:T --- Measure of humic-like (recalcitrant) versus fresh-like (labile) fluorescence

ratios <- bp_select_sites %>% 
  select(site_code_long, date_ymd:longitude, distHaversine_km, DOC_mg.L, 
         PeakA_RU:PeakT_RU, AT_ratio, CA_ratio, CM_ratio, CT_ratio)

# A:T
p_AT <- ratios %>% 
  filter(!is.na(AT_ratio)) %>% 
  ggplot(aes(site_code_long, AT_ratio, fill = Year)) + 
  facet_wrap(~ Year, ncol = 4) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_x_discrete(limits = rev) + 
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "Peak A:Peak T",
       subtitle = "Humic-like (recalcitrant) versus fresh-like (labile) fluorescence")

p_ATd <- ratios %>% 
  filter(!is.na(AT_ratio)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_AT = mean(AT_ratio), 
            sd_AT = sd(AT_ratio),
            n = n()) %>% 
  mutate(upper = mean_AT + sd_AT,
         lower = mean_AT - sd_AT) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_AT)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  theme(legend.position = 'none') +
  xlim(c(0, 30)) +
  labs(x = NULL, y = "Peak A:Peak T", tag = "A)")
       # subtitle = expression(bold("Humic-like (recalcitrant) versus fresh-like (labile) fluorescence")))


# C:A
p_CA <- ratios %>% 
  filter(!is.na(CA_ratio)) %>% 
  ggplot(aes(site_code_long, CA_ratio, fill = Year)) + 
  facet_wrap(~ Year, ncol = 4) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_x_discrete(limits = rev) + 
  theme(legend.position = 'none',
        axis.text.y = element_blank()) +
  labs(x = NULL, y = "Peak C:Peak A",
       subtitle = "Relative amount of photosensitive humic-like DOM fluorescence")

p_CAd <- ratios %>% 
  filter(!is.na(CA_ratio)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_CA = mean(CA_ratio), 
            sd_CA = sd(CA_ratio),
            n = n()) %>% 
  mutate(upper = mean_CA + sd_CA,
         lower = mean_CA - sd_CA) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_CA)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  xlim(c(0, 30)) +
  theme(legend.position = 'none') +
  labs(x = NULL, y = "Peak C:Peak A", tag = "C)")
       # subtitle = expression(bold("Relative amount of photosensitive humic-like DOM fluorescence")))

# C:M
p_CM <- ratios %>% 
  filter(!is.na(CM_ratio)) %>% 
  ggplot(aes(site_code_long, CM_ratio, fill = Year)) + 
  facet_wrap(~ Year, ncol = 4) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_x_discrete(limits = rev) + 
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "Peak C:Peak M",
       subtitle = "Amount of diagenetically-altered (blue-shifted) fluorescence")

p_CMd <- ratios %>% 
  filter(!is.na(CM_ratio)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_CM = mean(CM_ratio), 
            sd_CM = sd(CM_ratio),
            n = n()) %>% 
  mutate(upper = mean_CM + sd_CM,
         lower = mean_CM - sd_CM) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_CM)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  xlim(c(0, 30)) +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Peak C:Peak M", tag = "D)")
       # subtitle = expression(bold("Amount of diagenetically-altered (blue-shifted) fluorescence")))


# C:T
p_CT <- ratios %>% 
  filter(!is.na(CT_ratio)) %>% 
  ggplot(aes(site_code_long, CT_ratio, fill = Year)) + 
  facet_wrap(~ Year, ncol = 4) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_x_discrete(limits = rev) + 
  theme(legend.position = 'bottom',
        axis.text.y = element_blank()) +
  labs(x = NULL, y = "Peak C:Peak T",
       subtitle = "Humic-like (recalcitrant) versus fresh-like (labile) fluorescence")

p_CTd <- ratios %>% 
  filter(!is.na(CT_ratio)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_CT = mean(CT_ratio), 
            sd_CT = sd(CT_ratio),
            n = n()) %>% 
  mutate(upper = mean_CT + sd_CT,
         lower = mean_CT - sd_CT) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_CT)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  xlim(c(0, 30)) +
  theme(legend.position = 'none') +
  labs(x = NULL, y = "Peak C:Peak T", tag = "B)")
       # subtitle = expression(bold("Humic-like (recalcitrant) versus fresh-like (labile) fluorescence")))

# 1
p_ATd / p_CTd

(p_AT + p_CA) / (p_CM + p_CT)

p_ratios <- (p_ATd / p_CTd / p_CAd / p_CMd) 

ggsave(paste0(p_outname, dd, "ratios_distance.png"), p_ratios, w = 7.5, h = 8.9)



# Specific fluorescence at various peaks ----------------------------------

### Humic-like peaks (spA, spC, spM, spD)
p_spA <- ratios %>% 
  mutate(spA = PeakA_RU / DOC_mg.L) %>% 
  filter(!is.na(spA)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_spA = mean(spA), 
            sd_spA = sd(spA),
            n = n()) %>% 
  mutate(upper = mean_spA + sd_spA,
         lower = mean_spA - sd_spA) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_spA)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  xlim(c(0, 30)) +
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "spA")

ratios %>% 
  mutate(spD = PeakD_RU / DOC_mg.L) %>% 
  filter(!is.na(spD)) %>% 
  rename(Site = site_code_long) %>% 
  group_by(Site, distHaversine_km, Year) %>% 
  summarise(mean_spD = mean(spD), 
            sd_spD = sd(spD),
            n = n()) %>% 
  mutate(upper = mean_spD + sd_spD,
         lower = mean_spD - sd_spD) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_spD)) + 
  facet_wrap(~ Year) +
  geom_line() +  
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  xlim(c(0, 30)) +
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "spD")

ratios %>% 
  mutate(spA = PeakA_RU / DOC_mg.L, 
         spD = PeakD_RU / DOC_mg.L) %>% 
  filter(!is.na(spA), !is.na(spD)) %>% 
  ggplot(aes(spA, spD, col = Year, shape = Year)) + 
  geom_point()

### Fresh-like peaks (spB, spT) 

# plot grid.... -----------------------------------------------------------

parms <- c(TDN_mg.L, DOC_mg.L, A254, SUVA, )

bp_grid <- bp_select_sites %>%
  select(site_code_long, Year, TDN_mg.L, DOC_mg.L, S275to295, BA:FI, HIX_Ohno, SUVA) %>% 
  pivot_longer(cols = -c(site_code_long, Year),
               names_to = "parameter", 
               values_to = "result")

bp_grid %>% 
  ggplot(aes(site_code_long, result, fill = Year)) +
  facet_grid(cols = vars(Year), rows = vars(parameter), scales = "free_x") + 
  geom_boxplot() + 
  coord_flip() +
  theme(legend.position = 'bottom')

p <- ggplot(mpg, aes(displ, cty)) + geom_point()

# Use vars() to supply variables from the dataset:
p + facet_grid(rows = vars(drv))
p + facet_grid(cols = vars(cyl))
p + facet_grid(vars(drv), vars(cyl))

bp_select_sites %>%
  filter(!is.na(FI) & !Year == 2015 & FI < 1.69) %>%
  ggplot(aes(site_code_long, FI, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  # geom_point() +
  coord_flip() +
  scale_y_continuous(breaks = c(1.50, 1.60, 1.70), labels = c(1.50, 1.60, 1.70)) +
  # lims(y = c(0.015, 0.030)) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Fluorescence index",
       subtitle = "Fluorescence index: Indicator of DOM source (terrestrial ~1.3 and\nmicrobial ~1.7) but should be used with caution as FI can change\nindependent of source")

# Additional plots wip ----------------------------------------------------

# Peak A, Peak b, Peak C, Peak D, Peak E, Peak M, Peak N plots...
# Peak ratios: C:T and A:T

bp_select_sites %>%
  filter(!is.na(PeakC_RU)) %>% 
  ggplot(aes(site_code_long, PeakC_RU, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom")

bp_select_sites %>%
  filter(!is.na(PeakA_RU)) %>% 
  ggplot(aes(site_code_long, PeakA_RU, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom")

bp_select_sites %>%
  filter(!is.na(PeakT_RU)) %>% 
  ggplot(aes(site_code_long, PeakT_RU, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom")

bp_select_sites %>%
  mutate(CT_ratio = PeakC_RU / PeakT_RU) %>% 
  filter(!is.na(CT_ratio)) %>% 
  ggplot(aes(site_code_long, CT_ratio, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom") + 
  labs(x = NULL, y = "Peak C:Peak T")

bp_select_sites %>%
  mutate(AT_ratio = PeakA_RU / PeakT_RU) %>% 
  filter(!is.na(AT_ratio)) %>% 
  ggplot(aes(site_code_long, AT_ratio, fill = Year)) +
  facet_wrap(~ Year, ncol = 4) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "bottom")

bp_select_sites %>%
  mutate(AT_ratio = PeakA_RU / PeakT_RU,
         CT_ratio = PeakC_RU / PeakT_RU) %>% 
  filter(!is.na(AT_ratio) & !is.na(CT_ratio)) %>% 
  ggplot(aes(AT_ratio, CT_ratio, col = Year)) +
  geom_point()



# Absorbance and fluorescence diagnostic plots  ---------------------------

# bp_select_sites %>% 
#   filter(!is.na(PeakA_percent)) %>% 
#   ggplot(aes(DOY, PeakA_percent, colour = Year)) +
#   facet_wrap(~ site_name, ncol = 3) +
#   geom_line() +
#   geom_point(col = "white", size = 1.5) +
#   geom_point() +
#   theme(legend.position = "bottom") +
#   labs(x = "Day of year")

bp_select_sites %>% 
  filter(!is.na(PeakA_RU)) %>% 
  ggplot(aes(DOY, PeakA_RU, colour = Year)) +
  facet_wrap(~ site_name, ncol = 3) +
  geom_line() +
  geom_point(col = "white", size = 1.5) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year")

bp_select_sites %>%
  filter(!is.na(TDN_mg.L)) %>%
  ggplot(aes(DOY, TDN_mg.L, colour = Year)) +
  facet_wrap(~ site_name, ncol = 3) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = TDN_lab)

bp_select_sites %>%
  filter(!is.na(HIX)) %>%
  ggplot(aes(DOY, HIX, colour = Year)) +
  facet_wrap(~ site_name, ncol = 3) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "Humification Index")

bp_select_sites %>%
  filter(!is.na(BA)) %>%
  ggplot(aes(DOY, BA, colour = Year)) +
  facet_wrap(~ site_name, ncol = 3) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "B:a")




# Where is there missing data? --------------------------------------------

eems <- bp_doc_eems()

# DOC 
eems %>% 
  group_by(Year) %>% 
  filter(is.na(DOC_mg.L))

# lat/long
eems %>% filter(is.na(latitude)) 



# Preliminary plots -------------------------------------------------------

# HIX <0.6 may be sign of issues, filter >0.6
bp_select_sites %>% 
  filter(HIX_Ohno > 0.6 & !is.na(date_ymd)) %>%
  ggplot(aes(BA, HIX_Ohno, colour = Year)) +
  facet_wrap(~ Year, nrow = 1) + 
  geom_point(size = 2.5, alpha = 3/4) +
  
  theme(legend.position = "bottom") +
  labs(x = expression(paste(italic("β"), ":", italic("α"), " ratio")),
       y = "Humification index")


bp_select_sites %>% 
  filter(A254 < 20) %>% 
  ggplot(aes(DOC_mg.L, A254, colour = Year)) + 
  geom_point(size = 2.5, alpha = 3/4) +
  
  theme(legend.position = "bottom") +
  labs(x = DOC_lab, y = "Absorbance at 254 nm")


# 3 instruments used, Iowa State, Brockport, UVM
bp_select_sites %>% 
  filter(A254 > 5, A254 < 15, !is.na(date_ymd)) %>%
  ggplot(aes(A254, A280, colour = Year)) + 
  geom_point(size = 2.5, alpha = 7/8) +
  
  facet_wrap(~ Year, ncol = 5) +
  theme(legend.position = "bottom")

bp_select_sites %>% 
  filter(!is.na(date_ymd), !Year == "2015", S275to295 < 0.04) %>%
  ggplot(aes(DOC_mg.L, S275to295, colour = Year)) + 
  facet_wrap(~ Year, ncol = 5) +
  # geom_boxplot() + 
  geom_point(size = 2.5, alpha = 7/8) +
  lims(x = c(NA, 10),
       y = c(0.015, 0.0265)) +
  
  theme(legend.position = "bottom")

# remove > 0.04
bp_select_sites %>% 
  # filter(S275to295 < 0.04) %>% 
  ggplot(aes(HIX, S275to295, col = site_code_long)) +
  facet_wrap(~ Year, nrow = 1) +
  theme(legend.position = "bottom") +
  geom_point()

bp_select_sites %>% 
  filter(S275to295 < 0.04) %>% 
  ggplot(aes(S350to400, S275to295, colour = Year)) + 
  geom_point(size = 2.5, alpha = 3/4) +
  facet_wrap(~ Year, ncol = 5) +
  
  theme(legend.position = "bottom")


# could be useful to separate tributaries from lake samples
# SUVA normal if <5 and >1 
bp_select_sites %>% 
  filter(HIX_Ohno > 0.6) %>% 
  ggplot(aes(HIX_Ohno, SUVA, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

bp_select_sites %>% 
  filter(HIX > 0.6) %>% 
  ggplot(aes(HIX, SUVA, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

eems %>% 
  filter(Year == 2019) %>% 
  ggplot(aes(date_ymd, A254)) + 
  geom_point() 


eems %>% 
  ggplot(aes(BA, FI, col = Year)) + 
  facet_wrap(~ Year) +
  geom_point() +
  theme(legend.position = 'bottom')



# Mueller et al. 2012 -----------------------------------------------------
# Aquat Geochem (2012) 18:21–44
# DOI 10.1007/s10498-011-9147-y
eems %>% 
  group_by(site_code_long) %>%
  ggplot(aes(DOC_mg.L, SUVA)) + 
  # facet_wrap(~ Year) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_viridis_d() +
  labs(x = DOC_lab, y = SUVA_lab)

eems %>% 
  group_by(site_code_long, Year) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            SUVA_mean = mean(SUVA, na.rm = TRUE)) %>% 
  ggplot(aes(DOC_mean, SUVA_mean)) + 
  facet_wrap(~ Year, scales = "free") +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = SUVA_lab)

eems %>% 
  group_by(site_code_long) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            SUVA_mean = mean(SUVA, na.rm = TRUE)) %>% 
  ggplot(aes(DOC_mean, SUVA_mean)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = SUVA_lab)

pp2 <- eems %>% 
  group_by(site_code_long) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            A254_mean = mean(A254, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(DOC_mean, A254_mean)) + 
  geom_point() +
  ylim(c(7, 19)) +
  geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = "Absorbance")

pp1 <- eems %>% 
  group_by(site_code_long) %>% 
  ggplot(aes(DOC_mg.L, A254, col = site_code_long)) + 
  # facet_wrap(~ site_code_long) + 
  geom_point() +
  # geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = "Absorbance")

pp1 + pp2

eems %>% 
  group_by(site_code_long) %>% 
  ggplot(aes(DOC_mg.L, FI)) + 
  # facet_wrap(~ Year) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = "Fluorescence Index")

eems %>% 
  group_by(site_code_long) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            FI_mean = mean(FI, na.rm = TRUE)) %>% 
  ggplot(aes(DOC_mean, FI_mean)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme(panel.grid = element_blank()) +
  labs(x = DOC_lab, y = "Fluorescence Index")

eems %>% 
  group_by(site_code_long, Year) %>% 
  summarise(DOC_mean = mean(DOC_mg.L, na.rm = TRUE),
            FI_mean = mean(FI, na.rm = TRUE)) %>% 
  ggplot(aes(DOC_mean, FI_mean)) + 
  facet_wrap(~ Year, scales = 'free') +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = DOC_lab, y = "Fluorescence Index")

eems %>% 
  group_by(site_code_long) %>% 
  rename(Site = site_code_long) %>% 
  ggplot(aes(SUVA, FI, col = Site)) + 
  facet_wrap(~ date_ymd) +
  geom_point(size = 2.5, alpha = 3/4) +
  scale_color_viridis_d() +
  # ggpubr::theme_classic2() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank()) +
  labs(x = SUVA_lab, y = "Fluorescence Index")

eems %>% 
  group_by(site_code_long) %>% 
  rename(Site = site_code_long) %>% 
  summarise(SUVA_mean = mean(SUVA, na.rm = TRUE),
            FI_mean = mean(FI, na.rm = TRUE)) %>% 
  ggplot(aes(SUVA_mean, FI_mean)) + 
  # ggpubr::theme_classic2() +
  geom_point(aes(col = Site), size = 3) +
  geom_smooth(method = 'lm', se = FALSE, col = "steelblue") +
  theme(legend.position = "bottom") +
  scale_color_viridis_d() +
  labs(x = SUVA_lab, y = "Fluorescence Index")

eems_mean <- eems %>% 
  group_by(site_code_long) %>% 
  summarise(SUVA_mean = mean(SUVA, na.rm = TRUE),
            FI_mean = mean(FI, na.rm = TRUE),
            DOC_mean = mean(DOC_mg.L, na.rm = TRUE)) 

fi_suva <- lm(FI_mean ~ SUVA_mean, data = eems_mean)
summary(fi_suva) # R^2 = 0.97, p < 0.0001


# Water quality -----------------------------------------------------------


# Turbidity ---------------------------------------------------------------


p_fturb <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(turb_field_NTU)) %>% 
  group_by(Site, distHaversine_km) %>% 
  summarise(mean_fturb = mean(turb_field_NTU), 
            sd_fturb = sd(turb_field_NTU),
            n = n()) %>% 
  mutate(upper = mean_fturb + sd_fturb,
         lower = mean_fturb - sd_fturb) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_fturb)) + 
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  theme_classic2(base_size = 15) + 
  theme(legend.position = 'right') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Field turbidity (NTU)")

p_lturb <- bp_select_sites %>% 
  rename(Site = site_code_long) %>% 
  filter(!is.na(turb_lab_NTU)) %>% 
  group_by(Site, distHaversine_km) %>% 
  summarise(mean_lturb = mean(turb_lab_NTU), 
            sd_lturb = sd(turb_lab_NTU),
            n = n()) %>% 
  mutate(upper = mean_lturb + sd_lturb,
         lower = mean_lturb - sd_lturb) %>% 
  ungroup() %>% 
  ggplot(aes(distHaversine_km, mean_lturb)) + 
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  theme_classic2(base_size = 15) + 
  theme(legend.position = 'right') +
  labs(x = "Distance from Buffalo Pound Lake inflow (km)", y = "Lab turbidity (NTU)")

(p_fturb / p_lturb) + plot_layout(guides = 'collect')

