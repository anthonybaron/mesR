library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")

# Source df with DOC and TOC 
doc_infill_raw <- bp_doc_toc_cc_fn() 

# 21 missing values for DOC
doc_infill_raw %>% filter(is.na(DOC_mg.L)) %>% print(n = Inf)

# Plot where the missing DOC values are
doc_infill_raw %>% 
  mutate(DOC_mg.L = ifelse(is.na(DOC_mg.L), 0, DOC_mg.L)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, col = note)) + 
  facet_wrap(~ year) +
  geom_point(size = 2, alpha = 3/5) +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(x = "Day", y = DOC_lab)

# Function to infill missing DOC with mean of nearest neighbours
doc_infill <- function(df = bp_doc_toc_cc_fn(), target_row = "", target_range = "") { 
  
  # Ideally nearest neighbour will be mean of 3 weeks ahead and 3 weeks behind missing value
  
  df <- df %>% 
    filter(row_num %in% c(target_range)) %>% 
    mutate(DOC_mg.L = ifelse(row_num == target_row, mean(DOC_mg.L, na.rm = TRUE), DOC_mg.L)) %>% 
    filter(row_num == target_row)
  
  return(df)
  
}

# Function to carry out DOC infilling using doc_infill() function
# target_rows and target_ranges selected in imputation-workspace.R
doc_NAs_filled <- function(df = doc_infill(), target_row = "", target_range = "") {
  
  df_filled <- bind_rows(
    doc_infill(target_row = 55, target_range = 52:60),
    doc_infill(target_row = 56, target_range = 53:61),
    doc_infill(target_row = 57, target_range = 54:62),
    doc_infill(target_row = 264, target_range = 261:267),
    doc_infill(target_row = 364, target_range = 361:367),
    doc_infill(target_row = 625, target_range = 622:628),
    doc_infill(target_row = 677, target_range = 674:680),
    doc_infill(target_row = 710, target_range = 707:713),
    doc_infill(target_row = 729, target_range = 726:732),
    doc_infill(target_row = 733, target_range = 720:736),
    doc_infill(target_row = 735, target_range = 732:738),
    doc_infill(target_row = 737, target_range = 734:740),
    doc_infill(target_row = 739, target_range = 736:742),
    doc_infill(target_row = 741, target_range = 738:744),
    doc_infill(target_row = 742, target_range = 739:745),
    doc_infill(target_row = 788, target_range = 785:791),
    doc_infill(target_row = 831, target_range = 828:834),
    doc_infill(target_row = 999, target_range = 996:1002),
    doc_infill(target_row = 1360, target_range = 1357:1363),
    doc_infill(target_row = 1460, target_range = 1457:1463),
    doc_infill(target_row = 1513, target_range = 1510:1516)
  ) 
  
  return(df_filled)
  
}

DOC_complete_1990_2019 <- function(df1 = bp_doc_toc_cc_fn(), df2 = doc_NAs_filled()) {
  
  df3 <- df1 %>%
    full_join(df2) %>% 
    filter(!is.na(DOC_mg.L), date_ymd <= "2019-12-31") %>% 
    arrange(date_ymd) 
  
  return(df3)
  
}
