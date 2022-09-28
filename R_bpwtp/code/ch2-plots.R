library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(viridis)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")

p_outname <- "./R_wavelet/outputs/figures/"
dd <- "20220928_"

bp_drivers <- wavelet_data()
bp_long <- bp_drivers %>% pivot_longer(cols = -c(date_ymd, year, month), 
                                       names_to = "parameter", values_to = "result")

ffr <- station_flow_monthly()
ff <- select(ffr, date_ymd, Year, Month, SK05JG004_combined_cms, SK05JG006_cms, RC_IC_cms)
ff <- rename(ff, LD = SK05JG006_cms, LC = RC_IC_cms, BP = SK05JG004_combined_cms)
ff_long <- ff %>% pivot_longer(cols = -c(date_ymd, Month, Year),
                               names_to = "station", values_to = "result")

# Water chemistry time series ---------------------------------------------

scales::show_col(viridis(8, option = "C", end = 0.9))
cc <- viridis(8, option = "D", end = 0.9)

p_mon <- function(df = bp_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, parameter == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 0.6, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}

DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 
SO4_lab <- expression(paste("SO"["4"]^" 2–", " (mg L"^-1*")"))
chla_lab <- expression(paste("Chl ", italic("a"), " (µg L"^-1*")")) 
TP_lab <- expression(paste("TP (µg L"^-1*")")) 
SRP_lab <- expression(paste("SRP (µg L"^-1*")")) 
DON_lab <- expression(paste("DON (mg L"^-1*")")) 
NO3_lab <- expression(paste("NO"["3"], "–N (mg L"^-1*")")) 
NH3_lab <- expression(paste("NH"["3"], "–N (mg L"^-1*")")) 

p1 <- p_mon(var = "DOC_mg.L", lc = "grey30", ylab = DOC_lab) + lims(y = c(2.5, NA)) + labs(tag = "a")
p2 <- p_mon(var = "SO4_mg.L", lc = "grey30", ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "b")
p3 <- p_mon(var = "chla_ug.L", lc = "grey30", ylab = chla_lab) + labs(tag = "e")
p4 <- p_mon(var = "TP_ug.L", lc = "grey30", ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "c")
p5 <- p_mon(var = "SRP_ug.L", lc = "grey30", ylab = SRP_lab) + labs(tag = "d")
p6 <- p_mon(var = "DON_mg.L", lc = "grey30", ylab = DON_lab)  + labs(tag = "f")
p7 <- p_mon(var = "NO3_mg.L", lc = "grey30", ylab = NO3_lab) + labs(x = "Year", tag = "g")
p8 <- p_mon(var = "NH3_mg.L", lc = "grey30", ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(x = "Year", tag = "h")

p_all <- (p1 + p2) / (p4 + p5) / (p3 + p6) / (p7 + p8)
ggsave(paste0(p_outname, dd, "p_ts.png"), p_all, w = 8, h = 9)


# Flow time series --------------------------------------------------------

p_ff <- function(df = ff_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, station == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 0.6, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}

BP_lab <- expression(paste(italic("Q"), ""[BP]*" (m"^-3*" s"^-1*")"))
LD_lab <- expression(paste(italic("Q"), ""[LD]*" (m"^-3*" s"^-1*")"))
LC_lab <- expression(paste(italic("Q"), ""[LC]*" (m"^-3*" s"^-1*")"))

p_bp <- p_ff(var = "BP", lc = "steelblue3", ylab = BP_lab) + lims(y = c(NA, 20)) + labs(x = "Year", tag = "c")
p_ld <- p_ff(var = "LD", lc = "grey30", ylab = LD_lab) + labs(tag = "a")
p_lc <- p_ff(var = "LC", lc = "forestgreen", ylab = LC_lab) + labs(tag = "b")

p_flow <- p_ld / p_lc / p_bp
ggsave(paste0(p_outname, dd, "p_flow.png"), p_flow, w = 8, h = 6)

