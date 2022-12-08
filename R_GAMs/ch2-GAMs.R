library(mgcv)
library(gratia)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)


# Read-in data ------------------------------------------------------------
gambp_raw <- readr::read_csv("./R_data-cleaning/bpwtp/data/clean/monthly-ts.csv")
# Note can also source this data, but slow and uses a lot of memory
# source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
# gambp_raw <- wavelet_data(ts_monthly = TRUE)

gambp <- select(gambp_raw, 
                date_ymd, 
                DOC = DOC_mg.L,
                SO4 = SO4_mg.L,
                Chla = chla_ug.L, 
                TP = TP_ug.L, 
                NH4 = NH3_mg.L,
                Dief = SK05JG006_cms)



# For plotting and saving outputs -----------------------------------------
theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))
p_outname <- "./R_GAMs/outputs/figures/"
dd <- format(Sys.time(), "%Y%m%d")

# axis labels
DOC_lab <- expression(paste("DOC concentration (mg L"^-1 * ")"))
SO4_lab <- expression(paste("SO"["4"]^" 2–", " concentration (mg L"^-1*")"))
TP_lab <- expression(paste("TP concentration (µg L"^-1*")")) 
Chla_lab <- expression(paste("Chl ", italic("a"), " concentration (µg L"^-1*")")) 
NH4_lab <- expression(paste("NH"[4]^" +", " concentration (mg L"^-1*")")) 
LD_lab <- expression(paste(italic("Q"), ""[LD]*" (m"^-3*" s"^-1*")"))

# plot titles 
SO4_title <- expression(paste("s(SO"["4"]^" 2–",")"))
TP_title <- expression(paste("s(TP)")) 
Chla_title <- expression(paste("s(Chl ", italic("a"), ")")) 
NH4_title <- expression(paste("s(NH"[4]^" +", ")")) 
LD_title <- expression(paste("s(", italic("Q"), ""[LD]*" )"))



# Check distributions -----------------------------------------------------
# looks like gamma could be good but will use Tweedie
gambp %>%
  select(DOC:Dief) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()



# Best-fitting model and prediction ---------------------------------------
M <- gam(DOC ~ s(SO4) + s(TP) + s(Chla) + s(NH4, k = 3) + s(Dief),
         data = gambp, family = tw, method = "REML", select = TRUE)
summary(M) ### R2 = 0.499, Dev. expl. = 56.3%
# gam.check(M) 

# plot partial effects for each smooth
p_draw2 <- draw(M, ncol = 2) &   
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
# ggsave(paste0(p_outname, dd, "p_draw2.png"), p_draw2, w = 8.5, h = 7.5)

pMa <- draw(M, select = "s(SO4)") + labs(x = SO4_lab, title = SO4_title)
pMb <- draw(M, select = "s(TP)") + labs(x = TP_lab, title = TP_title)
pMc <- draw(M, select = "s(Chla)") + labs(x = Chla_lab, title = Chla_title)
pMd <- draw(M, select = "s(NH4)") + labs(x = NH4_lab, title = NH4_title)
pMe <- draw(M, select = "s(Dief)") + labs(x = LD_lab, title = LD_title)

(pMa + pMb) / (pMc + pMd) / (pMe) & plot_layout(ncol = 2)

# appraise best-fit model
p_appraise2 <- appraise(M, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
# ggsave(paste0(p_outname, dd, "p_appraise2.png"), p_appraise2, w = 7.2, h = 5.5)

# predict DOC concentrations from GAM 
predM <- predict(M, type = 'response', se.fit = TRUE)

predM_df <- as_tibble(predM) %>% rename(DOCpred = fit, DOCpred_se = se.fit)

gambp_pred <- as_tibble(cbind(gambp, predM_df)) %>% mutate(Year = year(date_ymd))
gambp_pred_long <- gambp_pred %>% 
  select(-c(SO4, Chla, TP, NH4, Dief, Year, DOCpred_se)) %>% 
  rename(Observed = DOC, Predicted = DOCpred) %>% 
  pivot_longer(cols = -date_ymd,
               names_to = "fit",
               values_to = "DOC") 
# gambp_pred_long %>%
#   ggplot(aes(date_ymd, DOC, col = fit)) +
#   geom_line(size = 1) +
#   labs(y = DOC_lab, col = NULL, x = 'Year')

# plot prediction with ribbon 
p_predYEAR <- gambp_pred %>% 
  ggplot(aes(date_ymd, DOC)) +
  geom_point(alpha = 7/8, size = 1.5) + 
  geom_line(data = gambp_pred, aes(date_ymd, DOCpred), size = 1, col = "steelblue3") + 
  geom_ribbon(data = gambp_pred,
              aes(ymin = predictM$fit - 1.96 * predictM$se.fit, 
                  ymax = predictM$fit + 1.96 * predictM$se.fit),
              fill = 'steelblue3', alpha = 1/2, size = 4) +
  labs(x = 'Year', y = DOC_lab) 
       # subtitle = "GAM fit: DOC ~ s(Sulfate) + s(TP) + s(NH3) + s(L. Dief. flow)\nR squared = 0.50, Deviance explained = 56%")
# ggsave(paste0(p_outname, dd, "p_GAMpred.png"), p_predYEAR, w = 6, h = 4)

# plot prediction with ribbon and faceted by year 
p_predDOY <- gambp_pred %>% 
  ggplot(aes(yday(date_ymd), DOC)) +
  facet_wrap(~ Year, ncol = 10) +
  geom_point(alpha = 7/8, size = 1.5) + 
  geom_line(data = gambp_pred, aes(yday(date_ymd), DOCpred, group = Year), size = 1, col = "steelblue3") +
  geom_ribbon(data = gambp_pred,
              aes(ymin = predictM$fit - 1.96 * predictM$se.fit, 
                  ymax = predictM$fit + 1.96 * predictM$se.fit),
              fill = 'steelblue3', alpha = 1/2) +
  labs(x = 'Day of year', y = DOC_lab,
       subtitle = "GAM fit: DOC ~ s(Sulfate) + s(TP) + s(NH3) + s(L. Dief. flow)\nR squared = 0.50, Deviance explained = 56%")



# Diefenbaker 18-month lag ------------------------------------------------
dief18 <- gambp %>% 
  mutate(Dief_18mlead = lead(Dief, n = 18)) %>% 
  filter(!is.na(Dief_18mlead)) 

g18 <- gam(DOC ~ s(SO4) + s(TP) + s(Chla) + s(NH4, k = 3) + s(Dief_18mlead),
           data = dief18, family = tw, method = "REML", select = TRUE)
summary(g18) ### R2 = 0.535, Dev. expl. = 59.5%
appraise(g18)
# gam.check(g18) 

p_draw18 <- draw(g18, ncol = 2) &   
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
# ggsave(paste0(p_outname, dd, "p_draw18.png"), p_draw18, w = 8.5, h = 7.5)

p_appraise18 <- appraise(g18, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
# ggsave(paste0(p_outname, dd, "p_appraise18.png"), p_appraise18, w = 7.2, h = 5.5)



# Swap SRP for TP  --------------------------------------------------------
gsrp <- gam(DOC_mg.L ~ s(chla_ug.L) + s(SRP_ug.L) + s(SO4_mg.L) +
              s(NH3_mg.L, k = 3) + s(SK05JG006_cms),
            data = gambp_raw, family = tw, method = "REML", select = TRUE)
summary(gsrp) ### R2 = 0.515, Dev. expl. = 56.9
draw(gsrp)
appraise(gsrp)
