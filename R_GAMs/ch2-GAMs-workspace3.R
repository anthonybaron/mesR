library(mgcv)
library(gratia)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)
# library(tidymv)

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

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))
DOC_lab <- expression(paste("DOC concentration (mg L"^-1 * ")"))
SO4_lab <- expression(paste("SO"["4"]^" 2–", " concentration (mg L"^-1*")"))
SO4_title <- expression(paste("s(SO"["4"]^" 2–",")"))
TP_lab <- expression(paste("TP concentration (µg L"^-1*")")) 
TP_title <- expression(paste("s(TP)")) 
Chla_lab <- expression(paste("Chl ", italic("a"), " concentration (µg L"^-1*")")) 
Chla_title <- expression(paste("s(Chl ", italic("a"), ")")) 
NH4_lab <- expression(paste("NH"[4]^" +", " concentration (mg L"^-1*")")) 
NH4_title <- expression(paste("s(NH"[4]^" +", ")")) 
LD_lab <- expression(paste(italic("Q"), ""[LD]*" (m"^-3*" s"^-1*")"))
LD_title <- expression(paste("s(", italic("Q"), ""[LD]*" )"))

p_outname <- "./R_GAMs/outputs/figures/"
dd <- format(Sys.time(), "%Y%m%d")

# looks like gamma could be good but will use Tweedie
# gambp %>%
#   select(DOC:Dief) %>% 
#   gather() %>% 
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram()

# loggambp <- gambp %>%
#   mutate_at(c("DOC","SO4", "Chla", "TP", "NH4", "Dief"), log) 
# loggambp %>%
#   select(DOC:Dief) %>% 
#   gather() %>% 
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram()

g1 <- gam(DOC ~ s(SO4) + s(Chla) + s(TP) + s(NH4) + s(Dief),
          data = gambp, family = tw, method = "REML", select = TRUE)
summary(g1)
draw(g1)
appraise(g1)
gam.check(g1) # increase k for NH4


g2 <- gam(DOC ~ s(SO4) + s(TP) + s(Chla) + s(NH4, k = 30) + s(Dief),
          data = gambp, family = tw, method = "REML", select = TRUE)
summary(g2)
draw(g2)
appraise(g2)
gam.check(g2) # looks like increasing k does not help NH4...

# lead Dief by 3 months
Dieflead3 <- gambp %>% 
  mutate(Dief_3month = lead(Dief, n = 3)) %>% 
  filter(!is.na(Dief_3month)) # good, n = 357

g3 <- gam(DOC ~ s(SO4) + s(TP) + s(NH4, k = 30) + s(Dief_3month),
          data = Dieflead3, family = tw, method = "REML", select = TRUE)
summary(g3) # lowers dev expl
draw(g3)
appraise(g3)
gam.check(g3) 

# lead Dief by 1 month
Dieflead1 <- gambp %>% 
  mutate(Dief_1month = lead(Dief, n = 1)) %>% 
  filter(!is.na(Dief_1month)) # good, n = 359

g4 <- gam(DOC ~ s(SO4) + s(TP) + s(NH4, k = 30) + s(Dief_1month),
          data = Dieflead1, family = tw, method = "REML", select = TRUE)
summary(g4) # lowers dev expl
draw(g4)
appraise(g4)
gam.check(g4) 

# lead Dief by 2 months
Dieflead2 <- gambp %>% 
  mutate(Dief_2month = lead(Dief, n = 2)) %>% 
  filter(!is.na(Dief_2month)) # good, n = 358

g5 <- gam(DOC ~ s(SO4) + s(TP) + s(NH4, k = 30) + s(Dief_2month),
          data = Dieflead2, family = tw, method = "REML", select = TRUE)
summary(g5) # doesn't help
draw(g5)
appraise(g5)
gam.check(g5) 

# lead Dief by 12 months and SO4 by 3 months
Dieflead12 <- gambp %>% 
  mutate(Dief_12month = lead(Dief, n = 12),
         SO4_3month = lead(SO4, n = 3)) %>% 
  filter(!is.na(Dief_12month)) # good, n = 348

g6 <- gam(DOC ~ s(SO4_3month) + s(TP) + s(NH4, k = 30) + s(Dief_12month),
          data = Dieflead12, family = tw, method = "REML", select = TRUE)
summary(g6) # doesn't help
draw(g6)
appraise(g6)
gam.check(g6) 

# lead Dief by 18 months
Dieflead18 <- gambp %>% 
  mutate(Dief_18month = lead(Dief, n = 18)) %>% 
  filter(!is.na(Dief_18month)) # good, n = 356

g6 <- gam(DOC ~ s(SO4) + s(TP) + s(NH4, k = 30) + s(Dief_18month),
          data = Dieflead4, family = tw, method = "REML", select = TRUE)
summary(g6) # doesn't help
draw(g6)
appraise(g6)
gam.check(g6) 




# Best-fitting model and prediction ---------------------------------------
# g2
M <- gam(DOC ~ s(SO4) + s(TP) + s(Chla) + s(NH4, k = 3) + s(Dief),
         data = gambp, family = tw, method = "REML", select = TRUE)
summary(M)
# gam.check(M) ### R2 = 0.499, Dev. expl. = 56.3%

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



p_appraise2 <- appraise(M, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
# ggsave(paste0(p_outname, dd, "p_appraise2.png"), p_appraise2, w = 7.2, h = 5.5)


predictM <- predict(M, type = 'response', se.fit = TRUE)
# plot(predictM)
predM_df <- as_tibble(predictM) %>% 
  rename(DOCpred = fit, DOCpred_se = se.fit)

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
p_predYEAR <- gambp_pred %>% 
  ggplot(aes(date_ymd, DOC)) +
  geom_point(alpha = 7/8, size = 1.5) + 
  geom_line(data = gambp_pred, aes(date_ymd, DOCpred), size = 1, col = "steelblue3") + 
  geom_ribbon(data = gambp_pred,
              aes(ymin = predictM$fit - 1.96 * predictM$se.fit, 
                  ymax = predictM$fit + 1.96 * predictM$se.fit),
              fill = 'steelblue3', alpha = 1/2, size = 4) +
  labs(x = 'Year', y = DOC_lab,
       subtitle = "GAM fit: DOC ~ s(Sulfate) + s(TP) + s(NH3) + s(L. Dief. flow)\nR squared = 0.50, Deviance explained = 56%")
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


  




# Dief 18-month lag -------------------------------------------------------
dief18 <- gambp %>% 
  mutate(Dief_18mlead = lead(Dief, n = 18)) %>% 
         # SO4_18mlead = lead(SO4, n = 9)) %>%
  filter(!is.na(Dief_18mlead)) 

g18 <- gam(DOC ~ s(SO4) + s(TP) + s(Chla) + s(NH4, k = 3) + s(Dief_18mlead),
           data = dief18, family = tw, method = "REML", select = TRUE)
summary(g18) 
appraise(g18)
gam.check(g18) 

p_draw18 <- draw(g18, ncol = 2) &   
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
ggsave(paste0(p_outname, dd, "p_draw18.png"), p_draw18, w = 8.5, h = 7.5)

p_appraise18 <- appraise(g18, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
ggsave(paste0(p_outname, dd, "p_appraise18.png"), p_appraise18, w = 7.2, h = 5.5)

# compare M and g18
summary(M)
# R2 = 0.499, DE% = 56.3%, -REML = 585.4, n = 360
summary(g18)
# R2 = 0.535, DE% = 59.5%, -REML = 553.6, n = 342





# GAM with all vars -------------------------------------------------------

allgam <- gam(DOC_mg.L ~ s(chla_ug.L) + s(TP_ug.L) + s(SRP_ug.L) + s(SO4_mg.L) +
                s(DON_mg.L) + s(NO3_mg.L) + s(NH3_mg.L) + s(RC_IC_cms) +
                s(SK05JG004_combined_cms) + s(SK05JG006_cms),
              data = gambp_raw, family = tw, method = "REML", select = TRUE)
summary(allgam)
draw(allgam)
gam.check(allgam)
appraise(allgam)


p1 <- gambp_raw %>% 
  ggplot(aes(chla_ug.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$chla_ug.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p2 <- gambp_raw %>% 
  ggplot(aes(TP_ug.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$TP_ug.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p3 <- gambp_raw %>% 
  ggplot(aes(SRP_ug.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$SRP_ug.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p4 <- gambp_raw %>% 
  ggplot(aes(SO4_mg.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$SO4_mg.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p5 <- gambp_raw %>% 
  ggplot(aes(DON_mg.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$DON_mg.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p6 <- gambp_raw %>% 
  ggplot(aes(NO3_mg.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$NO3_mg.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p7 <- gambp_raw %>% 
  ggplot(aes(NH3_mg.L, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$NH3_mg.L), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p8 <- gambp_raw %>% 
  ggplot(aes(SK05JG004_combined_cms, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$SK05JG004_combined_cms), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p9 <- gambp_raw %>% 
  ggplot(aes(SK05JG006_cms, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$SK05JG006_cms), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)
p10 <- gambp_raw %>% 
  ggplot(aes(RC_IC_cms, DOC_mg.L)) + 
  geom_point() + 
  geom_vline(xintercept = mean(gambp_raw$RC_IC_cms), col = "red") +
  geom_hline(yintercept = mean(gambp_raw$DOC_mg.L), col = "red") +
  geom_smooth(method = 'gam', se = TRUE)

(p1+p2+p3+p4+p5)/(p6+p7+p8+p9+p10) + plot_layout(nrow = 2)




gsrp <- gam(DOC_mg.L ~ s(chla_ug.L) + s(SRP_ug.L) + s(SO4_mg.L) +
                s(NH3_mg.L, k = 3) + s(SK05JG006_cms),
              data = gambp_raw, family = tw, method = "REML", select = TRUE)
summary(gsrp)
draw(gsrp)
gam.check(gsrp)
appraise(gsrp)
