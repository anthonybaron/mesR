library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")
# source("./R_wavelet/code/imputation.R")

bp_longterm_clean <- clean_bp_longterm() %>% filter(parameter %in% c("DOC", "TOC")) 
bp_masterfile_clean <- clean_bp_masterfile() %>% filter(parameter %in% c("DOC", "TOC"))
bp_historical_clean <- clean_bp_historical() %>% filter(parameter %in% c("DOC", "TOC"))

DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 


# mutate(date_ymd = as.character(date_ymd)) %>% 
#   add_row(date_ymd = "1996-12-30", result = NA) %>%
#   add_row(date_ymd = "2001-12-31", result = NA) %>% 
#   add_row(date_ymd = "2002-12-30", result = NA) %>% 
#   add_row(date_ymd = "2018-01-01", result = NA) %>% 
#   add_row(date_ymd = "2019-01-07", result = NA) 


# Plot DOC and TOC for each file ------------------------------------------

bp_longterm_clean %>% filter(parameter == "DOC", is.na(result)) # 203 NAs
bp_masterfile_clean %>% filter(parameter == "DOC", is.na(result)) # 196 NAs
bp_historical_clean %>% filter(parameter == "DOC", is.na(result)) # 214 NAs

bp_longterm_clean %>% filter(parameter == "TOC", is.na(result)) # 369 NAs
bp_masterfile_clean %>% filter(parameter == "TOC", is.na(result)) # 362 NAs
bp_historical_clean %>% filter(parameter == "TOC", is.na(result)) # 380 NAs

bp_longterm_clean %>% filter(parameter == "DOC", is.na(result))
bp_longterm_clean %>% filter(datasheet == "doc_profile")
bp_longterm_clean %>% filter(datasheet == "RawWater")

bp_longterm_clean %>% 
  filter(datasheet == "doc_profile", parameter == "DOC") %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_point()

bp_longterm_clean %>% 
  filter(datasheet == "RawWater", parameter == "DOC") %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_point(size = 2) + 
  geom_point(data = bp_longterm_clean %>% filter(datasheet == "doc_profile"), aes(yday(date_ymd), result), col = "pink", alpha = 3/4)

bp_longterm_rawwater <- bp_longterm_clean %>% 
  filter(datasheet == "RawWater", parameter == "DOC", date_ymd >= "1998-05-27") %>% 
  # rename(DOC_RawWater = result) %>% 
  select(date_ymd:parameter, result) 

bp_longterm_rawwater %>% filter(is.na(result)) %>% print(n = Inf)




bp_longterm_docprofile <- bp_longterm_clean %>% 
  filter(datasheet == "doc_profile", parameter == "DOC") %>% 
  # rename(DOC_doc_profile = result) %>% 
  select(date_ymd:parameter, result)



bp_longterm_docprofile %>% tail()
bp_longterm_rawwater %>% tail()


bp_longterm_rawwater %>% full_join(bp_longterm_docprofile) %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_point()

bp_longterm_rawwater %>% full_join(bp_longterm_docprofile) %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point()

  
tmp_join <- bp_longterm_rawwater %>% left_join(bp_longterm_docprofile) 
tmp_join2 <- bp_longterm_docprofile %>% left_join(bp_longterm_rawwater)

tmp_join2 %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_line()

tmp_join$date_ymd[duplicated(tmp_join$date_ymd)]



bp_longterm_rawwater %>%
  mutate(result = ifelse(is.na(result), bp_longterm_docprofile$result, result)) %>% 
  distinct(date_ymd)

bp_longterm_rawwater %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_point()











# Raw water DOC concentrations from 1998-05-27 to 2019-12-31
bp_longterm_rawwater <- bp_longterm_clean %>% 
  filter(datasheet == "RawWater", parameter == "DOC", date_ymd >= "1998-05-27") %>% 
  select(date_ymd:parameter, result) 

bp_longterm_rawwater_pre1998 <- bp_longterm_clean %>% 
  filter(datasheet == "RawWater", parameter == "DOC", date_ymd < "1998-05-27") %>% 
  select(date_ymd:parameter, result) 

# DOC profile DOC concentrations from 1998-05-27 to 2019-12-31
bp_longterm_docprofile <- bp_longterm_clean %>% 
  filter(datasheet == "doc_profile", parameter == "DOC") %>% 
  select(date_ymd:parameter, result)


# Missing raw water DOC concentrations from 1998-05-27 to 2019-12-31
bp_longterm_rawwater_NA <- bp_longterm_rawwater %>%
  filter(is.na(result)) %>% 
  mutate(rownum = row_number()) %>% 
  select(rownum, everything()) 

bp_longterm_rawwater_NA  %>% print(n = Inf)

# Raw water DOC concentrations from 1998-05-27 to 2019-12-31 without NAs
bp_longterm_rawwater_CC <- bp_longterm_rawwater %>% filter(!is.na(result)) 

# DOC profile values to replace missing raw water DOC concentrations from 1998-05-27 to 2019-12-31
profile_vals <- tibble::tribble(
  ~rownum, ~result,
  # 2000
  1,       NA,
  
  # 2001
  2,       7.0,
  3,       7.6,
  4,       7.4,
  5,       7.2,
  6,       7.8,
  7,       8.4,
  8,       8.0,
  9,       7.6,
  10,      7.6,
  11,      7.6,
  12,      7.0,
  13,      6.8,
  14,      6.8,
  15,      6.0,
  16,      6.7,
  17,      6.6,
  18,      6.3,
  19,      6.6,
  20,      6.4,
  21,      6.6,
  
  # 2002
  22,      NA,
  
  # 2003
  23,      NA,
  24,      NA,
  25,      NA,
  26,      NA,
  27,      7.1,
  28,      7.2,
  29,      NA,
  30,      NA,
  31,      NA,
  32,      5.9,
  33,      NA,
  34,      NA,
  35,      NA,
  
  # 2004
  36,      NA,
  37,      NA,
  38,      NA,
  39,      NA,
  40,      NA,
  41,      NA,
  42,      NA,
  43,      NA,
  44,      NA,
  45,      NA,
  
  # 2005
  46,      NA,
  47,      NA,
  48,      NA,
  49,      4.7,
  50,      4.6,
  51,      4.5,
  
  # 2006
  52,      4.7,
  53,      NA,
  54,      NA,
  55,      6.6,
  56,      NA,
  57,      7.0,
  
  # 2007
  58,      NA,
  59,      NA,
  
  # 2008
  60,      NA,
  61,      4.9,
  62,      4.5,
  
  # 2009
  63,      6.3,
  64,      NA,
  65,      NA,
  
  # 2013
  66,      6.6,
  
  # 2016
  67,      NA,
  
  # 2017
  68,      NA,
  
  # 2018
  69,      NA
  
)

# Add new infilled DOC conconcentrations back in with the raw water DOC concentrations
bp_DOC_profile_infill <- bp_longterm_rawwater_NA %>% 
  select(-result) %>% 
  full_join(profile_vals) %>% 
  select(-rownum) %>% 
  full_join(bp_longterm_rawwater_CC) %>% 
  arrange(date_ymd)

# Plot where the missing DOC concentrations remain (from 1998-05-27 to 2019-12-31)
bp_DOC_profile_infill %>% 
  mutate(result = ifelse(is.na(result), 0, result)) %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) + 
  geom_point()


# Add the 1998-05-27 to 2019-12-31 data set back to the pre 1998-05-27 data set
bp_longterm_partial_infill <- bp_longterm_rawwater_pre1998 %>% 
  bind_rows(bp_DOC_profile_infill) %>% 
  select(-c(year, month, week)) %>% 
  mutate(date_ymd = as.character(date_ymd)) %>% 
  add_row(date_ymd = "1996-12-30", result = NA) %>%
  add_row(date_ymd = "2001-12-31", result = NA) %>% 
  add_row(date_ymd = "2002-12-30", result = NA) %>% 
  add_row(date_ymd = "2018-01-01", result = NA) %>% 
  add_row(date_ymd = "2019-01-07", result = NA) %>% 
  mutate(date_ymd = ymd(date_ymd),
         year = year(date_ymd),
         month = month(date_ymd),
         week = week(date_ymd),
         parameter = ifelse(is.na(parameter), "DOC", parameter)) %>% 
  arrange(date_ymd) %>% 
  select(date_ymd, year, month, week, parameter, DOC_longterm = result)


# After infilling with DOC profile DOC concentrations there are still 175 NAs
bp_longterm_partial_infill %>% filter(is.na(DOC_longterm))

bp_longterm_partial_infill %>% 
  group_by(year) %>% 
  summarise(count_NA = sum(is.na(DOC_longterm))) %>% 
  filter(count_NA > 0) %>% 
  print(n = Inf)
# A tibble: 23 × 2
# year count_NA
#    <dbl>    <int>
# 1   1990        1
# 2   1991       51
# 3   1992       52
# 4   1993       23
# 5   1994        3
# 6   1995        2
# 7   1996        1
# 8   1997        1
# 9   1998        1
# 10  2000        1
# 11  2001        1
# 12  2002        2
# 13  2003       10
# 14  2004       10
# 15  2005        3
# 16  2006        3
# 17  2007        2
# 18  2008        1
# 19  2009        2
# 20  2016        1
# 21  2017        1
# 22  2018        2
# 23  2019        1

bp_longterm_partial_infill %>% 
  mutate(DOC_longterm = ifelse(is.na(DOC_longterm), 0, DOC_longterm)) %>% 
  ggplot(aes(yday(date_ymd), DOC_longterm)) + 
  facet_wrap(~ year) + 
  geom_point()

# 49 NAs not including 1991—1993
bp_longterm_partial_infill_NA_sans9193 <- bp_longterm_partial_infill %>% 
  filter(is.na(DOC_longterm), !year %in% c(1991:1993)) %>% 
  mutate(rownum = row_number())

# bp_historical DOC concentrations 
bp_historical_DOC <- bp_historical_clean %>% filter(parameter == "DOC")

historical_vals <- tibble::tribble(
  
  ~rownum, ~result,
  
  # 1990
  1,       NA,
  
  # 1994
  2,       NA,
  3,       NA,
  4,       NA,
  
  # 1995
  5,       NA,
  6,       NA,
  
  # 1996
  7,       10,
  
  # 1997
  8,       NA,
  
  # 1998
  9,       NA,
  
  # 2000
  10,      NA,
  
  # 2001
  11,      6.6,
  
  # 2002
  12,      NA,
  13,      NA,
  
  # 2003
  14,      NA,
  15,      NA,
  16,      NA,
  17,      NA,
  18,      NA,
  19,      NA,
  20,      NA,
  21,      NA,
  22,      NA,
  23,      NA,
  
  # 2004
  24,      NA,
  25,      NA,
  26,      NA,
  27,      NA,
  28,      NA,
  29,      NA,
  30,      NA,
  31,      NA,
  32,      NA,
  33,      NA,
  
  # 2005
  34,      NA,
  35,      NA,
  36,      NA,
  
  # 2006
  37,      NA,
  38,      NA,
  39,      NA,
  
  # 2007
  40,      NA,
  41,      NA,
  
  # 2008
  42,      NA,
  
  # 2009
  43,      NA,
  44,      NA,
  
  # 2016
  45,      NA,
  
  # 2017
  46,      NA,
  
  # 2018
  47,      NA,
  48,      NA,
  
  # 2019
  49,      NA
)

# From the bp_historical file:
# 1996-12-30 DOC concentration = 10 mg/L
# 2001-12-31 DOC concentration = 6.6 mg/L

bp_longterm_partial_infill <- bp_longterm_partial_infill %>% 
  mutate(DOC_longterm = ifelse(date_ymd == "1996-12-30", 10,
                               ifelse(date_ymd == "2001-12-31", 6.6, DOC_longterm)))

# 173 NAs total
bp_longterm_partial_infill_NA <- bp_longterm_partial_infill %>% filter(is.na(DOC_longterm))

# 47 NAs not including 1991—1993
bp_longterm_partial_infill_NA_sans9193 <- bp_longterm_partial_infill %>% 
  filter(is.na(DOC_longterm), !year %in% c(1991:1993)) %>% 
  mutate(rownum = row_number()) %>% 
  select(rownum, everything())

# bp_masterfile DOC concentrations 
bp_masterfile_DOC <- bp_masterfile_clean %>% filter(parameter == "DOC")

masterfile_vals <- tibble::tribble(
  
  ~rownum, ~result,
  
  # 1990
  1,       NA,
  
  # 1994
  2,       NA,       
  3,       NA,       
  4,       NA,       
  
  # 1995
  5,       NA,
  6,       NA,
  
  # 1997
  7,       NA,
  
  # 1998
  8,       9.0,
  
  # 2000
  9,       NA,
  
  # 2002
  10,      NA,
  11,      NA,
  
  # 2003
  12,      NA,
  13,      NA,
  14,      NA,
  15,      NA,
  16,      NA,
  17,      NA,
  18,      NA,
  19,      NA,
  20,      NA,
  21,      6.0,
  
  # 2004
  22,      6.1,
  23,      6.2,
  24,      NA,
  25,      NA,
  26,      6.1,
  27,      NA,
  28,      NA,
  29,      NA,
  30,      5.5,
  31,      4.5,
  
  # 2005
  32,      NA,
  33,      NA,
  34,      NA,
  
  # 2006
  35,      NA,
  36,      NA,
  37,      NA,
  
  # 2007
  38,      NA,
  39,      6.2,
  
  # 2008
  40,      NA,
  
  # 2009
  41,      NA,
  42,      5.8,
  
  # 2016
  43,      NA,
  
  # 2017
  44,      NA,
  
  # 2018
  45,      NA,
  46,      NA,
  
  # 2019
  47,      NA
  
)

# From bp_masterfile:
# 1998-02-09 = 9.0
# 2003-12-29 = 6.0
# 2004-01-12 = 6.1
# 2004-01-26 = 6.2
# 2004-02-23 = 6.1
# 2004-03-29 = 5.5
# 2004-04-12 = 4.5
# 2007-12-31 = 6.2
# 2009-03-09 = 5.8

bp_longterm_partial_infill <- bp_longterm_partial_infill %>% 
  mutate(DOC_longterm = ifelse(date_ymd == "1998-02-09", 9.0,
                               ifelse(date_ymd == "2003-12-29", 6.0,
                                      ifelse(date_ymd == "2004-01-12", 6.1,
                                             ifelse(date_ymd == "2004-01-26", 6.2,
                                                    ifelse(date_ymd == "2004-02-23", 6.1,
                                                           ifelse(date_ymd == "2004-03-29", 5.5,
                                                                  ifelse(date_ymd == "2004-04-12", 4.5,
                                                                         ifelse(date_ymd == "2007-12-31", 6.2,
                                                                                ifelse(date_ymd == "2009-03-09", 5.8, DOC_longterm))))))))))

# 164 NAs total
bp_longterm_partial_infill_NA <- bp_longterm_partial_infill %>% filter(is.na(DOC_longterm))

# 38 NAs not including 1991—1993
bp_longterm_partial_infill_NA_sans9193 <- bp_longterm_partial_infill %>% 
  filter(is.na(DOC_longterm), !year %in% c(1991:1993)) %>% 
  mutate(rownum = row_number()) %>% 
  select(rownum, everything())

bp_longterm_clean %>% 
  filter(parameter == "TOC") %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) + 
  geom_point() +
  geom_point(data = bp_longterm_partial_infill, aes(yday(date_ymd), DOC_longterm), col = "green")

bp_doc <- bp_longterm_partial_infill %>% 
  filter(parameter == "DOC") %>% 
  rename(DOC_mg.L = DOC_longterm)

bp_toc <- bp_longterm_clean %>% 
  filter(parameter == "TOC", datasheet == "RawWater") %>% 
  mutate(date_ymd = as.character(date_ymd)) %>% 
  add_row(date_ymd = "1996-12-30", result = NA) %>%
  add_row(date_ymd = "2001-12-31", result = NA) %>% 
  add_row(date_ymd = "2002-12-30", result = NA) %>% 
  add_row(date_ymd = "2018-01-01", result = NA) %>% 
  add_row(date_ymd = "2019-01-07", result = NA) %>% 
  rename(TOC_mg.L = result) %>% 
  mutate(date_ymd = ymd(date_ymd),
         year = year(date_ymd),
         month = month(date_ymd),
         week = week(date_ymd)) %>% 
  arrange(date_ymd) %>% 
  select(date_ymd, year, month, week, parameter, TOC_mg.L)

bp_doc_toc <- bp_doc %>% mutate(TOC_mg.L = bp_toc$TOC_mg.L) 
bp_doc_toc_cc <- bp_doc_toc %>% filter(!is.na(DOC_mg.L) & !is.na(TOC_mg.L))

m1 <- lm(bp_doc_toc_cc$DOC_mg.L ~ bp_doc_toc_cc$TOC_mg.L)
# summary(m1)
# par(mfrow = c(2, 2)); plot(m1)

bp_doc_toc %>% 
  ggplot(aes(TOC_mg.L, DOC_mg.L)) + 
  theme_bw(base_size = 12) +
  geom_point(col = "steelblue", alpha = 1/5, size = 3) + 
  geom_abline(slope = 1, intercept = c(0, 0), col = "grey", lty = 2) +
  geom_abline(intercept = m1$coefficients[1], 
              slope = m1$coefficients[2],
              lwd = 1, colour = "forestgreen") +
  labs(x = "TOC", y = "DOC")

bp_doc_toc_filled <- bp_doc_toc %>%
  mutate(note = ifelse(is.na(DOC_mg.L) & is.na(TOC_mg.L), "Missing both DOC and TOC", 
                       ifelse(is.na(DOC_mg.L) & !is.na(TOC_mg.L), "Modeled with TOC", "Measured")),
         DOC_mg.L = ifelse(is.na(DOC_mg.L), m1$coefficients[2]*TOC_mg.L + m1$coefficients[1], DOC_mg.L)) 

bp_doc_toc_filled %>% 
  mutate(DOC_mg.L = ifelse(is.na(DOC_mg.L), 0, DOC_mg.L)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, col = note)) + 
  facet_wrap(~ year) + 
  geom_point()

# Now go back and see if any DOC profile values can be used (must match week
# number)
# 46 NAs remaining
bp_doc_toc_filled_NA <- bp_doc_toc_filled %>% filter(is.na(DOC_mg.L))
# 2000-12-25 = 8.5
# 2003-06-30 = 6.4 
# 2003-07-14 = 7.4
# 2003-08-18 = 7.0
# 2018-01-01 = 6.5
# 2019-01-07 = 5.79

bp_doc_toc_filled <- bp_doc_toc_filled %>% 
  mutate(DOC_mg.L = ifelse(date_ymd == "2000-12-25", 8.5,
                           ifelse(date_ymd == "2003-06-30", 6.4,
                                  ifelse(date_ymd == "2003-07-14", 7.4,
                                         ifelse(date_ymd == "2003-08-18", 7.0,
                                                ifelse(date_ymd == "2018-01-01", 6.5,
                                                       ifelse(date_ymd == "2019-01-07", 5.8, DOC_mg.L)))))),
         kNN = ifelse(is.na(DOC_mg.L), "kNN", NA),
         rownum = row_number()) 

# 40 NAs remaining (40/1564 = 2.46%)
# Infill the rest using the k nearest neighbour regression approach
bp_doc_toc_filled %>%
  mutate(rownum = row_number()) %>% 
  filter(is.na(DOC_mg.L)) %>% 
  select(rownum, everything()) 

bp_doc_toc_filled %>% 
  group_by(year) %>% 
  summarise(count_NA = sum(is.na(DOC_mg.L))) %>% 
  ggplot(aes(year, count_NA)) + 
  geom_col()


# Function to infill missing DOC with mean of nearest neighbours
doc_infill <- function(df = bp_doc_toc_filled, target_row = "", target_range = "") { 
  
  df <- df %>% 
    filter(rownum %in% c(target_range)) %>% 
    mutate(DOC_mg.L = ifelse(rownum == target_row, mean(DOC_mg.L, na.rm = TRUE), DOC_mg.L)) %>% 
    filter(rownum == target_row)
  
  return(df)
  
}



# Function to carry out DOC infilling using doc_infill() function
# target_rows and target_ranges selected in imputation-workspace.R
doc_NAs_filled <- function(df = doc_infill(), target_row = "", target_range = "") {
  
  df_filled <- bind_rows(
    doc_infill(target_row = 9, target_range = 8:10), ### k = 1
    doc_infill(target_row = 53, target_range = 52:58), ### k = 1
    doc_infill(target_row = 54, target_range = 52:58), ### k = 1
    doc_infill(target_row = 55, target_range = 52:58), ### k = 1
    doc_infill(target_row = 56, target_range = 52:58), ### k = 1
    doc_infill(target_row = 57, target_range = 52:58), ### k = 1
    doc_infill(target_row = 65, target_range = 64:66), ### k = 1
    doc_infill(target_row = 109, target_range = 108:111), ### k = 1
    doc_infill(target_row = 110, target_range = 108:111), ### k = 1
    doc_infill(target_row = 243, target_range = 242:245), ### k = 1
    doc_infill(target_row = 244, target_range = 242:245), ### k = 1
    doc_infill(target_row = 259, target_range = 258:261), ### k = 1
    doc_infill(target_row = 260, target_range = 258:261), ### k = 1
    doc_infill(target_row = 264, target_range = 263:265), ### k = 1
    doc_infill(target_row = 377, target_range = 376:378), ### k = 1
    doc_infill(target_row = 632, target_range = 631:633), ### k = 1
    doc_infill(target_row = 677, target_range = 676:678), ### k = 1
    doc_infill(target_row = 687, target_range = 686:688), ### k = 1
    doc_infill(target_row = 693, target_range = 692:694), ### k = 1
    doc_infill(target_row = 708, target_range = 707:709), ### k = 1
    doc_infill(target_row = 720, target_range = 719:721), ### k = 1
    doc_infill(target_row = 726, target_range = 725:727), ### k = 1
    doc_infill(target_row = 728, target_range = 727:729), ### k = 1
    doc_infill(target_row = 735, target_range = 734:737), ### k = 1
    doc_infill(target_row = 736, target_range = 734:737), ### k = 1
    doc_infill(target_row = 739, target_range = 738:742), ### k = 1
    doc_infill(target_row = 740, target_range = 738:742), ### k = 1
    doc_infill(target_row = 741, target_range = 738:742), ### k = 1
    doc_infill(target_row = 788, target_range = 787:789), ### k = 1
    doc_infill(target_row = 821, target_range = 820:822), ### k = 1
    doc_infill(target_row = 824, target_range = 823:825), ### k = 1
    doc_infill(target_row = 846, target_range = 845:847), ### k = 1
    doc_infill(target_row = 863, target_range = 862:864), ### k = 1
    doc_infill(target_row = 884, target_range = 883:885), ### k = 1
    doc_infill(target_row = 937, target_range = 936:938), ### k = 1
    doc_infill(target_row = 957, target_range = 956:958), ### k = 1
    doc_infill(target_row = 999, target_range = 998:1000), ### k = 1
    doc_infill(target_row = 1360, target_range = 1359:1361), ### k = 1
    doc_infill(target_row = 1459, target_range = 1458:1460), ### k = 1
    doc_infill(target_row = 1511, target_range = 1510:1512) ### k = 1
  ) 
  
  return(df_filled)
  
}


# Function to add k nearest neighbour DOC concentrations to full DOC time series
DOC_complete_1990_2019 <- function(df1 = bp_doc_toc_filled, df2 = doc_NAs_filled()) {
  
  df3 <- df1 %>%
    filter(!is.na(DOC_mg.L)) %>% 
    full_join(df2) %>% 
    # filter(!is.na(DOC_mg.L), date_ymd <= "2019-12-31") %>% 
    arrange(date_ymd) 
  
  return(df3)
  
}

# doc_cc <- DOC_complete_1990_2019()
# 
# doc_cc %>% 
#   ggplot(aes(yday(date_ymd), DOC_mg.L, col = kNN)) + 
#   facet_wrap(~ year) + 
#   geom_point() +
#   theme(legend.position = "bottom")


bp_DOC_monthly <- function(df = DOC_complete_1990_2019()) {
  
  df <- df %>% 
    group_by(year, month) %>% 
    summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
    ungroup() %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, DOC_mg.L)
  
  return(df)
  
}






