library(tidyverse)
library(ggplot2)

theme_set(theme_bw(base_size = 18))

timescales <- read_csv("./R_wavelet/data/wavelet-timescale-plots.csv") %>% 
  mutate(relationship = ifelse(relationship == 'Sulphate and L.Dief outflow', 'Sulphate and L.Dief. outflow', relationship))

order <- tribble(
  ~order,
  "DOC and sulphate",
  "DOC and TP",
  "DOC and SRP",
  "DOC and Buffalo Pound inflow",
  "Sulphate and Buffalo Pound inflow",
  "DOC and L.Dief. outflow",
  "Sulphate and L.Dief. outflow",
  "Sulphate and Ridge + Iskwao Creek flows",
  "SRP and Ridge + Iskwao Creek flows")

order_c <- order[["order"]]



timescales %>%  
  mutate(relationship = factor(relationship),
         relationship = forcats::fct_relevel(relationship, order_c)) %>% 
  ggplot(aes(x = ts1_start, xend = ts1_end, y = relationship, yend = relationship)) +
  geom_point(size = 2) +
  geom_point(data = timescales, aes(x = ts1_end, y = relationship), size = 2) +
  geom_point(data = timescales, aes(x = ts2_start, y = relationship), size = 2) +
  geom_point(data = timescales, aes(x = ts2_end, y = relationship), size = 2) +
  geom_segment(size = 1) +
  geom_segment(data = timescales, aes(x = ts2_start, xend = ts2_end, y = relationship, yend = relationship), size = 1) +
  scale_y_discrete(limits = rev) +
  labs(x = "Timescale (months)", y = NULL)

  

  