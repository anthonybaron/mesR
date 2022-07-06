library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(VIM)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")


# Data read in and prep ---------------------------------------------------


# BP_longterm_raw <- read_csv("./R_bpwtp/thesis/data/BPWTP_labdat_current.csv") 
# 
# BP_longterm <- BP_longterm_raw %>% 
#   filter(datasheet == "RawWater" & grepl("DOC", parm_unit)) %>%
#   separate(datetime_ymd.hms, into = c("date_ymd", "time"), sep = " ") %>% 
#   select(-c(datasheet, station, sheet_year, parameter, unit, parm_eval, 
#             result_org, result_flag, time)) %>% 
#   mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
#          date_ymd = as.Date(date_ymd),
#          DOY = yday(date_ymd)) %>% 
#   filter(date_ymd >= "1994-01-01" & date_ymd <= "2019-12-31") %>% 
#   select(date_ymd, result) %>% 
#   mutate(date_ymd = as.character(date_ymd)) %>% 
#   add_row(date_ymd = "1996-12-30", result = NA) %>%
#   add_row(date_ymd = "2001-12-31", result = NA) %>% 
#   add_row(date_ymd = "2002-12-30", result = NA) %>% 
#   add_row(date_ymd = "2018-01-01", result = NA) %>% 
#   add_row(date_ymd = "2019-01-07", result = NA) %>% 
#   arrange(date_ymd) %>% 
#   mutate(date_ymd = ymd(date_ymd),
#          obs_num = row_number()) %>% 
#   select(obs_num, date_ymd, result)  
# 
# BP_longterm %>% 
#   mutate(result = ifelse(is.na(result), 6.123456789, result),
#          is_NA = ifelse(result == 6.123456789, "yes", "no"),
#          is_NA = factor(is_NA)) %>% 
#   ggplot(aes(yday(date_ymd), result, col = is_NA)) +
#   facet_wrap(~ year(date_ymd)) + 
#   geom_point(alpha = 3/4)
# 
# BP_infill <- BP_longterm %>% 
#   mutate(infill_method = ifelse(date_ymd > "1996-01-01" & date_ymd < "2001-01-01" & is.na(result), "single", NA))
# 
# 
# BP_infill %>% filter(!is.na(infill_method))

# BP_longterm %>% 
#   filter(is.na(result)) %>% 
#   arrange(date_ymd) %>% 
#   # group_by(year) %>% 
#   mutate(lag_date_ymd = lag(date_ymd, 1),
#          weeks_bw_NA = as.numeric(difftime(date_ymd, lag_date_ymd, unit = "weeks"))) %>% 
#   mutate(infill_method = ifelse(weeks_bw_NA > 3, "single", "patch"))
# 
# BP_longterm %>% 
#   mutate(obs_num = row_number()) %>% 
#   # filter(date_ymd == "1997-03-31")
#   filter(obs_num %in% c(166:172)) %>%
#   mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result))
# 
# BP_longterm %>% 
#   filter(is.na(result)) %>% 
#   mutate(date_ymd = as.character(date_ymd)) %>% 
#   add_row(date_ymd = "1994-01-01", result = NA, obs_num = 1) %>% # dummy row, delete after
#   mutate(date_ymd = ymd(date_ymd)) %>% 
#   arrange(obs_num) %>% 
#   mutate(lag_behind = lag(obs_num, 1),
#          weeks_behind_NA = obs_num - lag_behind) %>% 
#   filter(!obs_num == 1) %>% 
#   mutate(infill_method = ifelse(weeks_behind_NA <= 3, "patch", "single")) 
  





# bp_infill_method <- BP_longterm %>% 
#   filter(is.na(result)) %>% 
#   mutate(year = year(date_ymd),
#          infill_method = ifelse(year %in% c(1994, 1995, 2001, 2004, 2007:2008, 2017:2019) | obs_num %in% c(496:537, 624:627, 677:678, 792:793), "patch",
#                                 ifelse(year %in% c(1996:2000, 2002, 2013:2016) | obs_num %in% c(480, 486, 581, 614, 617, 639:662, 786), "single", NA)))

# bp_single <- bp_infill_method %>% filter(infill_method == "single")

# bp_patch_missing <- bp_infill_method %>% filter(infill_method == "patch") %>%
#   mutate(patch_num = ifelse(obs_num %in% c(36:37), 1,
#                             ifelse(obs_num %in% c(52:57), 2,
#                                    ifelse(obs_num %in% c(398:418), 3, 
#                                           ifelse(obs_num %in% c(496:503), 4,
#                                                  ifelse(obs_num %in% c(513:537), 5,
#                                                         ifelse(obs_num %in% c(624:627), 6, 
#                                                                ifelse(obs_num %in% c(677:678), 7,
#                                                                       ifelse(obs_num %in% c(730:731), 8,
#                                                                              ifelse(obs_num %in% c(750:752), 9,
#                                                                                     ifelse(obs_num %in% c(792:793), 10, 
#                                                                                            ifelse(obs_num %in% c(1252:1253), 11,
#                                                                                                   ifelse(obs_num %in% c(1304:1306), 12, NA))))))))))))) 




# Infill singles (completed) ----------------------------------------------

# bp_single_lt <- full_join(BP_longterm, bp_single) 
# bp_single_lt %>% filter(is.na(result) & infill_method == "single")
# 
# w157 <- bp_single_lt %>% filter(obs_num %in% c(154:160)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 157) # 157
# w170 <- bp_single_lt %>% filter(obs_num %in% c(167:173)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 170) # 170
# w215 <- bp_single_lt %>% filter(obs_num %in% c(212:218)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 215) # 215
# w365 <- bp_single_lt %>% filter(obs_num %in% c(362:368)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 365) # 365
# w425 <- bp_single_lt %>% filter(obs_num %in% c(422:428)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 425) # 425
# w470 <- bp_single_lt %>% filter(obs_num %in% c(467:473)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 470) # 470
# w480 <- bp_single_lt %>% filter(obs_num %in% c(477:483)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 480) # 480
# w486 <- bp_single_lt %>% filter(obs_num %in% c(483:489)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 486) # 486
# w581 <- bp_single_lt %>% filter(obs_num %in% c(578:584)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 581) # 581
# w614 <- bp_single_lt %>% filter(obs_num %in% c(610:616)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 614) # 614 / 2 behind, 4 ahead
# w617 <- bp_single_lt %>% filter(obs_num %in% c(615:621)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 617) # 617 / 2 behind, 4 ahead
# w639 <- bp_single_lt %>% filter(obs_num %in% c(636:642)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 639) # 639
# w656 <- bp_single_lt %>% filter(obs_num %in% c(653:659)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 656) # 656
# w662 <- bp_single_lt %>% filter(obs_num %in% c(659:665)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 662) # 662
# w786 <- bp_single_lt %>% filter(obs_num %in% c(783:789)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 786) # 786
# w1044 <- bp_single_lt %>% filter(obs_num %in% c(1041:1047)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 1044) # 1044
# w1153 <- bp_single_lt %>% filter(obs_num %in% c(1150:1156)) %>% mutate(result = ifelse(is.na(result), mean(result, na.rm = TRUE), result)) %>% filter(obs_num == 1153) # 1153
# 
# bp_singles_cc <- bind_rows(w157,w170,w215,w365,w425,w470,w480,w486,w581,w614,w617,w639,w656,w662,w786,w1044,w1153) 
# 
# bp_single_completed <- full_join(BP_longterm, bp_singles_cc)
  

# Infill patches (incomplete) ---------------------------------------------

# 12 patches to fill... 
# 
# Paul Whitfield recommends doing two things:
# 1) use local information [in time] and 2) use seasonal information [across time] 
# to obtain a population of observations that could be considered reasonable for
# that time window. 
# 
# For example, if the missing gap is in June, try using all the June data across
# all years to determine if the 'population' and then sample from that to infill
# the missing points. 
# 
# If the population is ~normally distributed you can use rnorm() with the 
# appropriate mean and variance:
# rnorm(x, mean = pop_mean, sd = pop_sd)
# 
# If the population is not normally distributed you can bootstrap using an array
# of all appropriate samples "x" and the sample() function:
# infill_patch <- sample(x, size = length_of_gap, replace = FALSE)
# 
# You may or may not want to allow for replacement. If the gap is large and the
# population data small you may want to use replacement. 

# bp_longterm_weeks <- BP_longterm %>% 
#   mutate(week_num = week(date_ymd),
#          month = month(date_ymd, label = TRUE, abbr = TRUE))

# Patch 1 -----------------------------------------------------------------

# 2 missing values: obs_num %in% c(36:37)
# patch1 <- bp_patch_missing %>% filter(patch_num == 1)

# NAs are in September, so will use bootstrap resampling for September DOC to 
# fill NAs
# bp_september <- bp_longterm_weeks %>% 
#   filter(month == "Sep") %>% 
#   full_join(patch1) %>% 
#   filter(!is.na(result) | patch_num == 1)

# September DOC is skewed left
# bp_september %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# september_doc <- c(bp_september$result)
# set.seed(123)
# patch1_doc <- na.tools::na.bootstrap(september_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_september$obs_num) %>% 
#   filter(obs_num %in% c(36:37)) 


# Patch 2 -----------------------------------------------------------------

# 3 missing values: obs_num %in% c(52, 53, 57)
# patch2 <- bp_patch_missing %>% filter(patch_num == 2)
# 
# # NAs are in December and January, so will use bootstrap resampling for December
# # and January DOC to fill NAs
# bp_dec_jan <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec", "Jan")) %>% 
#   full_join(patch2) %>% 
#   filter(!is.na(result) | patch_num == 2)
# 
# # DOC is skewed left
# bp_dec_jan %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()

# dec_jan_doc <- c(bp_dec_jan$result)
# set.seed(123)
# patch2_doc <- na.tools::na.bootstrap(dec_jan_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec_jan$obs_num) %>% 
#   filter(obs_num %in% c(52, 53, 57)) 


# Patch 3 -----------------------------------------------------------------

# 21 missing values:
# obs_num %in% c(398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418)
# patch3 <- bp_patch_missing %>% filter(patch_num == 3)
# patch3$obs_num
# patch3$date_ymd

# NAs are from August to December, so will use bootstrap resampling for August
# to September to fill NAs
# bp_aug_dec <- bp_longterm_weeks %>% 
#   filter(month %in% c("Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#   full_join(patch3) %>% 
#   filter(!is.na(result) | patch_num == 3)
# 
# # DOC is skewed left
# bp_aug_dec %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# aug_dec_doc <- c(bp_aug_dec$result)
# set.seed(123)
# patch3_doc <- na.tools::na.bootstrap(aug_dec_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_aug_dec$obs_num) %>% 
#   filter(obs_num %in% c(398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418)) 


# Patch 4 -----------------------------------------------------------------

# 6 missing values:
# obs_num %in% c(496, 498, 499, 500, 501, 503)
# patch4 <- bp_patch_missing %>% filter(patch_num == 4)
# patch4$obs_num
# patch4$date_ymd

# NAs are from June to August, so will use bootstrap resampling for June
# to August to fill NAs
# bp_june_aug <- bp_longterm_weeks %>% 
#   filter(month %in% c("Jun", "Jul", "Aug")) %>% 
#   full_join(patch4) %>% 
#   filter(!is.na(result) | patch_num == 4)

# DOC is skewed left
# bp_june_aug %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# jun_aug_doc <- c(bp_june_aug$result)
# set.seed(123)
# patch4_doc <- na.tools::na.bootstrap(jun_aug_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_june_aug$obs_num) %>% 
#   filter(obs_num %in% c(496, 498, 499,500, 501, 503))


# Patch 5 -----------------------------------------------------------------

# 15 missing values:
# obs_num %in% c(513,515,519,521,522,524,526,528,529,530,532,533,534,535,537)
# patch5 <- bp_patch_missing %>% filter(patch_num == 5)
# patch5$obs_num
# patch5$date_ymd

# NAs are from June to August, so will use bootstrap resampling for October
# to April to fill NAs
# bp_oct_apr <- bp_longterm_weeks %>% 
#   filter(month %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) %>% 
#   full_join(patch5) %>% 
#   filter(!is.na(result) | patch_num == 5)

# DOC is skewed left
# bp_oct_apr %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# oct_apr_doc <- c(bp_oct_apr$result)
# set.seed(123)
# patch5_doc <- na.tools::na.bootstrap(oct_apr_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_oct_apr$obs_num) %>% 
#   filter(obs_num %in% c(513,515,519,521,522,524,526,528,529,530,532,533,534,535,537))


# Patch 6 -----------------------------------------------------------------

# 4 missing values:
# obs_num %in% c(624, 625, 626, 627)
# patch6 <- bp_patch_missing %>% filter(patch_num == 6)
# patch6$obs_num
# patch6$date_ymd

# NAs are in December and January, so will use bootstrap resampling for December 
# and January to fill NAs
# bp_dec_jan <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec", "Jan")) %>% 
#   full_join(patch6) %>% 
#   filter(!is.na(result) | patch_num == 6)

# DOC is skewed left
# bp_dec_jan %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# dec_jan_doc <- c(bp_dec_jan$result)
# set.seed(123)
# patch6_doc <- na.tools::na.bootstrap(dec_jan_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec_jan$obs_num) %>% 
#   filter(obs_num %in% c(624, 625, 626, 627))


# Patch 7 -----------------------------------------------------------------

# 2 missing values:
# obs_num %in% c(677, 678)
# patch7 <- bp_patch_missing %>% filter(patch_num == 7)
# patch7$obs_num
# patch7$date_ymd
# 
# # NAs are in December, so will use bootstrap resampling for December to fill NAs
# bp_dec <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec")) %>% 
#   full_join(patch7) %>% 
#   filter(!is.na(result) | patch_num == 7)
# 
# # DOC is skewed left
# bp_dec %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# dec_doc <- c(bp_dec$result)
# set.seed(123)
# patch7_doc <- na.tools::na.bootstrap(dec_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec$obs_num) %>% 
#   filter(obs_num %in% c(677, 678))


# Patch 8 -----------------------------------------------------------------

# 2 missing values:
# obs_num %in% c(730, 731)
# patch8 <- bp_patch_missing %>% filter(patch_num == 8)
# patch8$obs_num
# patch8$date_ymd
# 
# # NAs are in December, so will use bootstrap resampling for December to fill NAs
# bp_dec <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec")) %>% 
#   full_join(patch8) %>% 
#   filter(!is.na(result) | patch_num == 8)
# 
# # DOC is skewed left
# bp_dec %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# may_doc <- c(bp_dec$result)
# set.seed(123)
# patch8_doc <- na.tools::na.bootstrap(may_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec$obs_num) %>% 
#   filter(obs_num %in% c(730, 731))


# Patch 9 -----------------------------------------------------------------

# 3 missing values:
# obs_num %in% c(750, 751, 752)
# patch9 <- bp_patch_missing %>% filter(patch_num == 9)
# patch9$obs_num
# patch9$date_ymd
# 
# # NAs are in May, so will use bootstrap resampling for May to fill NAs
# bp_may <- bp_longterm_weeks %>% 
#   filter(month %in% c("May")) %>% 
#   full_join(patch9) %>% 
#   filter(!is.na(result) | patch_num == 9)
# 
# # DOC is skewed left
# bp_may %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# may_doc <- c(bp_may$result)
# set.seed(123)
# patch9_doc <- na.tools::na.bootstrap(may_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_may$obs_num) %>% 
#   filter(obs_num %in% c(750:752))


# Patch 10 ----------------------------------------------------------------

# 2 missing values:
# obs_num %in% c(792, 793)
# patch10 <- bp_patch_missing %>% filter(patch_num == 10)
# patch10$obs_num
# patch10$date_ymd
# 
# # NAs are in March, so will use bootstrap resampling for March to fill NAs
# bp_mar <- bp_longterm_weeks %>% 
#   filter(month %in% c("Mar")) %>% 
#   full_join(patch10) %>% 
#   filter(!is.na(result) | patch_num == 10)
# 
# # DOC is skewed left
# bp_mar %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# mar_doc <- c(bp_mar$result)
# set.seed(123)
# patch10_doc <- na.tools::na.bootstrap(mar_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_mar$obs_num) %>% 
#   filter(obs_num %in% c(792, 793))


# Patch 11 ----------------------------------------------------------------

# 2 missing values:
# obs_num %in% c(1252, 1253)
# patch11 <- bp_patch_missing %>% filter(patch_num == 11)
# patch11$obs_num
# patch11$date_ymd
# 
# # NAs are in December and January, so will use bootstrap resampling for December
# # and January to fill NAs
# bp_dec_jan <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec", "Jan")) %>% 
#   full_join(patch11) %>% 
#   filter(!is.na(result) | patch_num == 11)
# 
# # DOC is skewed left
# bp_dec_jan %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# dec_jan_doc <- c(bp_dec_jan$result)
# set.seed(123)
# patch11_doc <- na.tools::na.bootstrap(dec_jan_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec_jan$obs_num) %>% 
#   filter(obs_num %in% c(1252, 1253))


# Patch 12 ----------------------------------------------------------------

# 2 missing values:
# obs_num %in% c(1304, 1306)
# patch12 <- bp_patch_missing %>% filter(patch_num == 12)
# patch12$obs_num
# patch12$date_ymd
# 
# # NAs are in December and January, so will use bootstrap resampling for December
# # and January to fill NAs
# bp_dec_jan <- bp_longterm_weeks %>% 
#   filter(month %in% c("Dec", "Jan")) %>% 
#   full_join(patch12) %>% 
#   filter(!is.na(result) | patch_num == 12)
# 
# # DOC is skewed left
# bp_dec_jan %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# dec_jan_doc <- c(bp_dec_jan$result)
# set.seed(123)
# patch12_doc <- na.tools::na.bootstrap(dec_jan_doc) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_dec_jan$obs_num) %>% 
#   filter(obs_num %in% c(1304, 1306))




# Fill in patch NAs and combine with singles  -----------------------------

# bp_patches_cc <- 
#   bind_rows(patch1_doc, patch2_doc, patch3_doc, patch4_doc, patch5_doc, patch6_doc,
#             patch7_doc, patch8_doc, patch9_doc, patch10_doc, patch11_doc, patch12_doc)
# 
# bp_patches_cc <- bp_patch_missing %>% 
#   select(-result) %>% 
#   full_join(bp_patches_cc) %>% 
#   select(-patch_num)
#   
# bp_singles_cc <- bind_rows(w157,w170,w215,w365,w425,w470,w480,w486,w581,
#                            w614,w617,w639,w656,w662,w786,w1044,w1153) 
# 
# 
# bp_NAs_filled <- bind_rows(bp_singles_cc, bp_patches_cc) %>% 
#   mutate(infill_method = ifelse(is.na(infill_method), "patch", infill_method))
# 
# BP_longterm_cc <- BP_longterm %>%
#   filter(!is.na(result)) %>% 
#   full_join(bp_NAs_filled) %>% 
#   arrange(obs_num) %>% 
#   mutate(`Infill method` = factor(infill_method),
#          `Infill method` = ifelse(is.na(infill_method), "None", 
#                                   ifelse(infill_method == "single", "Single", "Patch"))) 

# BP_longterm_cc %>% 
#   ggplot(aes(yday(date_ymd), result,  col = `Infill method`)) +
#   facet_wrap(~ year(date_ymd)) +
#   geom_point() +
#   theme(legend.position = "bottom")
# 
# BP_longterm_cc %>% 
#   ggplot(aes(date_ymd, result, col = `Infill method`)) +
#   geom_point(alpha = 3/4, size = 3) +
#   theme(legend.position = "bottom",
#         axis.title.x = element_blank()) +
#   labs(y = "DOC (mg/L)")
# 
# BP_longterm %>% 
#   ggplot(aes(date_ymd, result)) +
#   geom_point(alpha = 1/10, size = 3, col = "brown") +
#   geom_point(data = bp_NAs_filled, aes(date_ymd, result, col = infill_method), 
#              shape = 17, size = 3) +
#   theme(legend.position = "bottom",
#         axis.title.x = element_blank()) +
#   labs(y = "DOC (mg/L)")




# Imputation method 2: Monthly chunks -------------------------------------

# bp_patch_months <- bp_infill_method %>% filter(infill_method == "patch") %>%
#   mutate(month = month(date_ymd)) %>% 
#   arrange(month)
# 
# bp_longterm_months <- BP_longterm %>% mutate(month = month(date_ymd)) 

# January -----------------------------------------------------------------

# 7 missing values: obs_num %in% c(53, 57, 524, 526, 627, 1253, 1306)
# patch_jan <- bp_patch_months %>% filter(month == 1)
# 
# bp_jan <- bp_longterm_months %>% 
#   filter(month == 1) %>% 
#   full_join(patch_jan) %>% 
#   filter(!is.na(result) | month == 1) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# # January DOC is skewed left
# bp_jan %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# jan_doc <- c(bp_jan$result)
# set.seed(123)
# patch_jan_doc <- na.tools::na.bootstrap(jan_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_jan$obs_num) %>% 
#   filter(obs_num %in% c(patch_jan$obs_num)) 






# # 10 missing values
# patch_jan_dec <- bp_patch_months %>% 
#   filter(month %in% c(1, 2)) %>%
#   mutate(result = ifelse(obs_num == 1306, NA, result))
# 
# bp_jan_dec <- bp_longterm_months %>% 
#   filter(month %in% c(1, 2)) %>% 
#   full_join(patch_jan_dec) %>% 
#   filter(!is.na(result) | month %in% c(1, 2)) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# # January DOC is skewed left
# bp_jan_dec %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# jan_dec_doc <- c(bp_jan_dec$result)
# set.seed(123)
# patch_jan__dec_doc <- na.tools::na.bootstrap(jan_dec_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_jan_dec$obs_num) %>% 
#   filter(obs_num %in% c(patch_jan_dec$obs_num)) 






# March -------------------------------------------------------------------

# 6 missing values
# patch_mar <- bp_patch_months %>% filter(month == 3) 
# 
# bp_mar <- bp_longterm_months %>% 
#   filter(month == 3) %>% 
#   full_join(patch_mar) %>% 
#   filter(!is.na(result) | month == 3) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_mar %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# mar_doc <- c(bp_mar$result)
# set.seed(123)
# patch_mar_doc <- na.tools::na.bootstrap(mar_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_mar$obs_num) %>% 
#   filter(obs_num %in% c(patch_mar$obs_num))


# April -------------------------------------------------------------------

# 1 missing value
# patch_apr <- bp_patch_months %>% filter(month == 4) 
# 
# bp_apr <- bp_longterm_months %>% 
#   filter(month == 4) %>% 
#   full_join(patch_apr) %>% 
#   filter(!is.na(result) | month == 4) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_apr %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# apr_doc <- c(bp_apr$result)
# set.seed(123)
# patch_apr_doc <- na.tools::na.bootstrap(apr_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_apr$obs_num) %>% 
#   filter(obs_num %in% c(patch_apr$obs_num))

# May --------------------------------------------------------------------

# # 3 missing values
# patch_may <- bp_patch_months %>% filter(month == 5) 
# 
# bp_may <- bp_longterm_months %>% 
#   filter(month == 5) %>% 
#   full_join(patch_may) %>% 
#   filter(!is.na(result) | month == 5) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_may %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# may_doc <- c(bp_may$result)
# set.seed(123)
# patch_may_doc <- na.tools::na.bootstrap(may_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_may$obs_num) %>% 
#   filter(obs_num %in% c(patch_may$obs_num))

# June --------------------------------------------------------------------

# 1 missing value
# patch_jun <- bp_patch_months %>% filter(month == 6) 
# 
# bp_jun <- bp_longterm_months %>% 
#   filter(month == 6) %>% 
#   full_join(patch_jun) %>% 
#   filter(!is.na(result) | month == 6) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_jun %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# jun_doc <- c(bp_jun$result)
# set.seed(123)
# patch_jun_doc <- na.tools::na.bootstrap(jun_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_jun$obs_num) %>% 
#   filter(obs_num %in% c(patch_jun$obs_num))


# July --------------------------------------------------------------------

# 3 missing values
# patch_jul <- bp_patch_months %>% filter(month == 7) 
# 
# bp_jul <- bp_longterm_months %>% 
#   filter(month == 7) %>% 
#   full_join(patch_jul) %>% 
#   filter(!is.na(result) | month == 7) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_jul %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# jul_doc <- c(bp_jul$result)
# set.seed(123)
# patch_jul_doc <- na.tools::na.bootstrap(jul_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_jul$obs_num) %>% 
#   filter(obs_num %in% c(patch_jul$obs_num)) 


# August ------------------------------------------------------------------

# 5 missing values
# patch_aug <- bp_patch_months %>% filter(month == 8) 
# 
# bp_aug <- bp_longterm_months %>% 
#   filter(month == 8) %>% 
#   full_join(patch_aug) %>% 
#   filter(!is.na(result) | month == 8) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_aug %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# aug_doc <- c(bp_aug$result)
# set.seed(123)
# patch_aug_doc <- na.tools::na.bootstrap(aug_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_aug$obs_num) %>% 
#   filter(obs_num %in% c(patch_aug$obs_num)) 


# September ---------------------------------------------------------------

# 6 missing values
# patch_sep <- bp_patch_months %>% filter(month == 9) 
# 
# bp_sep <- bp_longterm_months %>% 
#   filter(month == 9) %>% 
#   full_join(patch_sep) %>% 
#   filter(!is.na(result) | month == 9) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_sep %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# sep_doc <- c(bp_sep$result)
# set.seed(123)
# patch_sep_doc <- na.tools::na.bootstrap(sep_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_sep$obs_num) %>% 
#   filter(obs_num %in% c(patch_sep$obs_num)) 


# October -----------------------------------------------------------------

# 6 missing values
# patch_oct <- bp_patch_months %>% filter(month == 10) 
# 
# bp_oct <- bp_longterm_months %>% 
#   filter(month == 10) %>% 
#   full_join(patch_oct) %>% 
#   filter(!is.na(result) | month == 10) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_oct %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# oct_doc <- c(bp_oct$result)
# set.seed(123)
# patch_oct_doc <- na.tools::na.bootstrap(oct_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_oct$obs_num) %>% 
#   filter(obs_num %in% c(patch_oct$obs_num)) 


# November ----------------------------------------------------------------

# 5 missing values
# patch_nov <- bp_patch_months %>% filter(month == 11) 
# 
# bp_nov <- bp_longterm_months %>% 
#   filter(month == 11) %>% 
#   full_join(patch_nov) %>% 
#   filter(!is.na(result) | month == 11) %>% 
#   mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
#   filter(!is.na(result) | infill_method == "patch")
# 
# bp_nov %>% 
#   ggplot(aes(result)) + 
#   geom_histogram()
# 
# nov_doc <- c(bp_nov$result)
# set.seed(123)
# patch_nov_doc <- na.tools::na.bootstrap(nov_doc, replace = TRUE) %>% 
#   as_tibble() %>% 
#   rename(result = value) %>% 
#   mutate(obs_num = bp_nov$obs_num) %>% 
#   filter(obs_num %in% c(patch_nov$obs_num)) 










infill_patches_by_month <- function(month_patch) {
  
  BP_longterm_raw <- read_csv("./R_bpwtp/thesis/data/BPWTP_labdat_current.csv") 
  
  BP_longterm <- BP_longterm_raw %>% 
    filter(datasheet == "RawWater" & grepl("DOC", parm_unit)) %>%
    separate(datetime_ymd.hms, into = c("date_ymd", "time"), sep = " ") %>% 
    select(-c(datasheet, station, sheet_year, parameter, unit, parm_eval, 
              result_org, result_flag, time)) %>% 
    mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
           date_ymd = as.Date(date_ymd),
           DOY = yday(date_ymd)) %>% 
    filter(date_ymd >= "1994-01-01" & date_ymd <= "2019-12-31") %>% 
    select(date_ymd, result) %>% 
    mutate(date_ymd = as.character(date_ymd)) %>% 
    add_row(date_ymd = "1996-12-30", result = NA) %>%
    add_row(date_ymd = "2001-12-31", result = NA) %>% 
    add_row(date_ymd = "2002-12-30", result = NA) %>% 
    add_row(date_ymd = "2018-01-01", result = NA) %>% 
    add_row(date_ymd = "2019-01-07", result = NA) %>% 
    arrange(date_ymd) %>% 
    mutate(date_ymd = ymd(date_ymd),
           obs_num = row_number()) %>% 
    select(obs_num, date_ymd, result) 
  
  bp_infill_method <- BP_longterm %>% 
    filter(is.na(result)) %>% 
    mutate(year = year(date_ymd),
           infill_method = ifelse(year %in% c(1994, 1995, 2001, 2004, 2007:2008, 2017:2019) | obs_num %in% c(496:537, 624:627, 677:678, 792:793), "patch",
                                  ifelse(year %in% c(1996:2000, 2002, 2013:2016) | obs_num %in% c(480, 486, 581, 614, 617, 639:662, 786), "single", NA)))
  
  bp_longterm_months <- BP_longterm %>% mutate(month = month(date_ymd)) 
  
  bp_patch_months <- bp_infill_method %>% filter(infill_method == "patch") %>%
    mutate(month = month(date_ymd)) %>% 
    arrange(month)
  
  patch_month <- bp_patch_months %>% filter(month == month_patch) 
  
  bp_month <- bp_longterm_months %>% 
    filter(month == month_patch) %>% 
    full_join(patch_month) %>% 
    filter(!is.na(result) | month == month_patch) %>% 
    mutate(infill_method = ifelse(is.na(result) & is.na(infill_method), "single", infill_method)) %>% 
    filter(!is.na(result) | infill_method == "patch")
  
  month_doc <- c(bp_month$result)
  set.seed(123)
  patch_month_doc <- na.bootstrap(month_doc, replace = TRUE) %>% 
    as_tibble() %>% 
    rename(result = value) %>% 
    mutate(obs_num = bp_month$obs_num) %>% 
    filter(obs_num %in% c(patch_month$obs_num))
  
  return(patch_month_doc)
  
}

pjan <- infill_patches_by_month(month_patch = 1)
pfeb <- infill_patches_by_month(month_patch = 2)
pmar <- infill_patches_by_month(month_patch = 3)
papr <- infill_patches_by_month(month_patch = 4)
pmay <- infill_patches_by_month(month_patch = 5)
pjun <- infill_patches_by_month(month_patch = 6)
pjul <- infill_patches_by_month(month_patch = 7)
paug <- infill_patches_by_month(month_patch = 8)
psep <- infill_patches_by_month(month_patch = 9)
poct <- infill_patches_by_month(month_patch = 10)
pnov <- infill_patches_by_month(month_patch = 11)
pdec <- infill_patches_by_month(month_patch = 12)

bp_patches_bind <- bind_rows(pjan,pfeb,pmar,papr,pmay,pjun,pjul,paug,psep,poct,pnov,pdec)

bp_patches_cc <- bp_patch_missing %>% 
  select(-c(result, patch_num)) %>% 
  full_join(bp_patches_bind)


bp_singles_cc <- bind_rows(w157,w170,w215,w365,w425,w470,w480,w486,w581,
                           w614,w617,w639,w656,w662,w786,w1044,w1153) 


bp_NAs_filled <- bind_rows(bp_singles_cc, bp_patches_cc) %>% 
  mutate(infill_method = ifelse(is.na(infill_method), "patch", infill_method))

BP_longterm_cc <- BP_longterm %>%
  filter(!is.na(result)) %>% 
  full_join(bp_NAs_filled) %>% 
  arrange(obs_num) %>% 
  mutate(`Infill method` = factor(infill_method),
         `Infill method` = ifelse(is.na(infill_method), "Observed", 
                                  ifelse(infill_method == "single", "7-week mean", "Bootstrap"))) 

BP_longterm_observed <- BP_longterm_cc %>% filter(`Infill method` == "Observed")

cbPalette <- c("cyan", "#D55E00", "#999999")
#009E73 = green

BP_longterm_cc %>% 
  filter(`Infill method` %in% c("7-week mean", "Bootstrap")) %>% 
  ggplot(aes(yday(date_ymd), result,  col = `Infill method`)) +
  facet_wrap(~ year(date_ymd)) +
  geom_point(data = BP_longterm_observed, aes(yday(date_ymd), result), alpha = 1/2, size = 2) + 
  geom_point(size = 2) +
  expand_limits(y = c(0, 15)) +
  scale_color_manual(values = cbPalette) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(x = "Day of year", y = "DOC (mg/L)")

BP_longterm_cc %>% 

  ggplot(aes(date_ymd, result, col = `Infill method`)) +
  geom_point(alpha = 3/4, size = 3) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +


BP_longterm %>% 
  ggplot(aes(date_ymd, result)) +
  geom_point(alpha = 1/10, size = 3, col = "brown") +
  geom_point(data = bp_NAs_filled, aes(date_ymd, result, col = infill_method), 
             shape = 17, size = 3) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  labs(y = "DOC (mg/L)")









# 2021-11-23 Complete infilling DOC ---------------------------------------

# The data set is missing dates (2002-12-30, 2019-01-07, 1996-12-30, 2001-12-31, 2018-01-01)
doc_infill_raw <- bp_doc_toc_cc_fn() 
doc_infill_raw %>% filter(is.na(DOC_mg.L)) %>% print(n = Inf)
# A tibble: 21 × 12
#   datasheet date_ymd    year month  week parameter unit   parm_unit         DOC_mg.L TOC_mg.L note                      row_num
#   <chr>     <date>     <dbl> <dbl> <dbl> <chr>     <chr>  <chr>                <dbl>    <dbl> <chr>                       <int>
#  1 RawWater  1991-01-21  1991     1     3 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC      55
#  2 RawWater  1991-01-28  1991     1     4 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC      56
#  3 RawWater  1991-02-04  1991     2     5 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC      57
#  4 RawWater  1995-01-30  1995     1     5 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     264
#  5 RawWater  1996-12-30  1996    12    53 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     364
#  6 RawWater  2001-12-31  2001    12    53 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     625
#  7 RawWater  2002-12-30  2002    12    52 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     677
#  8 RawWater  2003-08-18  2003     8    33 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     710
#  9 RawWater  2003-12-29  2003    12    52 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     729
# 10 RawWater  2004-01-26  2004     1     4 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     733
# 11 RawWater  2004-02-09  2004     2     6 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     735
# 12 RawWater  2004-02-23  2004     2     8 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     737
# 13 RawWater  2004-03-08  2004     3    10 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     739
# 14 RawWater  2004-03-22  2004     3    12 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     741
# 15 RawWater  2004-03-29  2004     3    13 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     742
# 16 RawWater  2005-02-14  2005     2     7 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     788
# 17 RawWater  2005-12-12  2005    12    50 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     831
# 18 RawWater  2009-03-02  2009     3     9 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC     999
# 19 RawWater  2016-02-01  2016     2     5 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC    1360
# 20 RawWater  2018-01-01  2018     1     1 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC    1460
# 21 RawWater  2019-01-07  2019     1     1 DOC       mg/L C DOC.GFdiss_mg.L.C       NA       NA Missing both DOC and TOC    1513

doc_infill_raw %>% 
  mutate(DOC_mg.L = ifelse(is.na(DOC_mg.L), 0, DOC_mg.L)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, col = note)) + 
  facet_wrap(~ year) +
  geom_point(size = 2, alpha = 3/5) +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  # axis.title.x = element_blank()) + 
  labs(x = "Day", y = DOC_lab)

doc_infill <- function(df = bp_doc_toc_cc_fn(), target_row = "", target_range = "") { 
  
  # Ideally nearest neighbour will be mean of 3 weeks ahead and 3 weeks behind
  # missing value
  
  df <- df %>% 
    filter(row_num %in% c(target_range)) %>% # ie row_num %in% c(target_range)
    mutate(DOC_mg.L = ifelse(row_num == target_row, mean(DOC_mg.L, na.rm = TRUE), DOC_mg.L)) %>% # ie row_num == target_row
    filter(row_num == target_row)
  
  return(df)
  
}

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

doc_complete <- DOC_complete_1990_2019()

doc_complete %>% 
  mutate(DOC_mg.L = ifelse(is.na(DOC_mg.L), 0, DOC_mg.L)) %>%
  ggplot(aes(yday(date_ymd), DOC_mg.L, col = note)) + 
  facet_wrap(~ year) +
  geom_point(size = 2, alpha = 3/5) +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  # axis.title.x = element_blank()) + 
  labs(x = "Day", y = DOC_lab)

doc_complete %>% 
  ggplot(aes(date_ymd, DOC_mg.L, col = note)) +
  # facet_wrap(~ year) +
  geom_point(size = 3, alpha = 3/5) +
  scale_color_manual(values = colas) +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(x = "Year", y = DOC_lab)

doc_complete %>% 
  ggplot(aes(DOC_mg.L)) +
  geom_boxplot() +
  coord_flip()







