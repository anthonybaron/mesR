## Data exploration following Zuur et al. (2010); 
## A vital step preceding the application of more sophistiated statistical 
## techniques (e.g., generalized additive (mixed) models).
## 
## Date created: 2020-08-05
## 
## Author: Anthony Baron
## 
## Keywords: collinearity, data exploration, independence, transformations,
## type I and II errors, zero inflation

## Protocol for data exploration:
## 
## 1. Formulate hypotheses & carry out experiment and collect data.
## 
## 2. Data exploration
##    2.1. Outliers Y & X      (boxplot & Cleveland dotplot)
##    2.2. Homogeneity Y       (conditional boxplot)
##    2.3. Normality Y         (histogram or QQ-plot)
##    2.4. Zero trouble Y      (frequency plot or corrgram)
##    2.5. Collinearity        (VIF, scatterplots, PCA)
##    2.6. Relationships Y & X ((multipanel) scatterplots, conditional boxplots)
##    2.7. Interactions        (coplots)
##    2.8. Independence        (ACF, variogram, plot Y versus time/space)
##    
## 3. Apply statistical model    

library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 16))

DOC_label <- expression(paste("DOC concentration (mg L"^-1*")")) 

BP_longterm <- read_csv("data/BPWTP_labdat_current.csv") %>% 
  filter(datasheet == "RawWater") %>% 
  select(-c(datasheet))

BP_longterm_cc <- BP_longterm %>% 
  filter(station == "Raw", !is.na(result), year >= 1983, year <= 2019) 

doc_longterm <- BP_longterm_cc %>% 
  filter(parm_unit == "DOC.GFdiss_mg.L.C") %>% 
  mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
         month = factor(month, levels = month.abb)) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(!is.na(result))

summary(doc_longterm)

unique(doc_longterm$year)

doc_mean_median_bymonth <- doc_longterm %>% 
  group_by(month) %>% 
  summarise(DOC_mean = mean(result, na.rm = TRUE),
            DOC_median = median(result, na.rm = TRUE))

doc_mean_median_overall <- doc_longterm %>% 
  summarise(DOC_mean = mean(result, na.rm = TRUE),
            DOC_median = median(result, na.rm = TRUE))



## Step 1. Are there outliers in Y and X? ----------------------------------

doc_longterm %>% 
  ggplot(aes(parm_unit, result)) + 
  geom_boxplot(width = 0.5) +
  geom_rug(aes(colour = year)) +
  scale_colour_viridis_c() +
  coord_flip() +
  labs(x = "",
       y = DOC_label) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(c(0, 15)) 




## Step 2. Do we have homogeneity of variance? -----------------------------
# Come back to later



## Step 3. Are the data normally distributed?  -----------------------------
doc_longterm %>% 
  ggplot(aes(result)) + 
  geom_histogram() +
  geom_rug() +
  labs(x = DOC_label,
       y = "Count") +
  xlim(c(0, 15))

doc_longterm %>% 
  ggplot(aes(result)) + 
  facet_wrap(~ month) +
  geom_histogram() +
  geom_rug() +
  labs(x = DOC_label,
       y = "Count") 

doc_longterm %>% 
  ggplot(aes(sample = result)) + 
  geom_qq() +
  geom_qq_line() +
  ylim(c(0, 15)) +
  xlim(c(-4, 4)) +
  labs(x = "Theoretical",
       y = "Sample")



## Step 8. Are obs of response var independent? ----------------------------
# Are observations independet? Does information from any one observation provide
# information on another observation?

doc_longterm %>% 
  ggplot(aes(datetime_ymd.hms, result)) +
  geom_point(alpha = 1/2, size = 3) +
  labs(x = "Year",
       y = DOC_label) +
  theme_bw(base_size = 25)

p_doc_monthly <- doc_longterm %>% 
  ggplot(aes(month, result)) + 
  geom_boxplot(outlier.colour = "white", outlier.fill = "white", colour = "grey") +
  geom_point(alpha = 0.4) + 
  # geom_point(aes(month, DOC_mean), data = doc_mean_median, fill = "red", size = 4, shape = 22) +
  # geom_point(aes(month, DOC_median), data = doc_mean_median, fill = "green", size = 4, shape = 24) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  labs(x = "Month",
       y = DOC_label,
       title = "Monthly DOC concentrations (1980–2020)")

p_doc_weekly <- doc_longterm %>% 
  ggplot(aes(week, result)) + 
  geom_point(alpha = 1/2) + 
  geom_smooth() +
  labs(x = "Week",
       y = DOC_label)

p_doc_quarterly_boxplot <- doc_longterm %>% 
  mutate(quarter = 1,
         quarter = ifelse(month %in% c("Jan", "Feb", "Mar"), "1", 
                          ifelse(month %in% c("Apr", "May", "Jun"), "2",
                                 ifelse(month %in% c("Jul", "Aug", "Sep"), "3",
                                        ifelse(month %in% c("Oct", "Nov", "Dec"), "4", quarter))))) %>%
  ggplot(aes(quarter, result)) + 
  geom_boxplot(outlier.colour = "white", outlier.fill = "white", colour = "grey") +
  geom_point(alpha = 1/2) +
  labs(x = "Season",
       y = DOC_label,
       title = "Seasonal DOC concentrations (1980–2020)",
       subtitle = "1 = Jan–Mar, 2 = Apr–Jun, 3 = Jul–Sep, 4 = Oct–Dec")
  
p_doc_quarterly <- doc_longterm %>% 
  mutate(Season = 1,
         Season = ifelse(month %in% c("Jan", "Feb", "Mar"), "1", 
                          ifelse(month %in% c("Apr", "May", "Jun"), "2",
                                 ifelse(month %in% c("Jul", "Aug", "Sep"), "3",
                                        ifelse(month %in% c("Oct", "Nov", "Dec"), "4", Season))))) %>%
  group_by(year, Season) %>%
  summarise(DOC_quarterly_mean = mean(result, na.rm = TRUE)) %>%
  mutate(Season = factor(Season)) %>% 
  ggplot(aes(year, DOC_quarterly_mean, colour = Season, shape = Season)) +
  # geom_smooth(alpha = 0.1, method = 'gam', se = F) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5),
                     labels = c("1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")) +
  scale_colour_viridis_d() +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = DOC_label,
       title = "Mean quarterly DOC concentrations (1980–2020)",
       subtitle = "1 = Jan–Mar, 2 = Apr–Jun, 3 = Jul–Sep, 4 = Oct–Dec")
  
  

# Plot autocorrelation factor (ACF).
# An ACF calculates the Pearson correlation between a time series and the same
# time series shifted by k time units.
doc_cc <- doc_longterm %>% filter(!is.na(result))

acf(doc_cc$result)



# UV254 -------------------------------------------------------------------

uv254_longterm <- BP_longterm_cc %>% 
  filter(parm_unit == "UV254_abs.10cm" & station == "Raw") %>% 
  mutate(month = factor(month, levels = month.abb))

unique(uv254_longterm$unit)

summary(uv254_longterm$result)
# length = 2107; NAs = 754

uv254_longterm %>% 
  filter(!is.na(result)) %>% 
  ggplot(aes(year, result)) +
  geom_point() 

uv254_longterm_cc <- uv254_longterm %>% 
  filter(!is.na(result))

tail(uv254_longterm_cc)

unique(uv254_longterm_cc$year)

uv254_longterm_cc %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col()

# SUVA --------------------------------------------------------------------

doc_uv254 <- bind_rows(doc_longterm, uv254_longterm)



# DOC with ice-on and ice-off dates ---------------------------------------

ice_dates <- read_csv("./data/operational_dates.csv") %>%
  filter(grepl("Ice", parameter_updated)) %>% 
  select(year = season, parameter = parameter_updated, date_ymd = value, -sheet_year) %>% 
  mutate_if(is.character, as.factor) %>%   
  mutate(ice_week = week(date_ymd))

doc_vals <- doc_longterm %>% 
  select(datetime_ymd.hms:parameter, result) %>% 
  mutate(datetime_ymd.hms = as.character(datetime_ymd.hms)) %>% 
  separate(col = datetime_ymd.hms, into = c("date_ymd", "time"), sep = " ") %>% 
  select(-time) %>% 
  mutate(date_ymd = ymd(date_ymd))

doc_ice <- full_join(doc_vals, ice_dates) %>% 
 mutate(Ice = ifelse(parameter == "Ice On", "On", 
                     ifelse(parameter == "Ice Off", "Off", NA)),
        ice_val = ifelse(!is.na(ice_week), 10, NA)) %>% 
  mutate_if(is.character, as.factor)

doc_ice %>% 
  filter(year > 1982) %>% 
  ggplot(aes(week, result)) +
  facet_wrap(~ year) + 
  geom_point() +
  geom_point(aes(ice_week, ice_val), colour = "red") +
  geom_hline(yintercept = 6.7, colour = "blue", alpha = 1/2) +
  scale_x_continuous(breaks = seq(0, 52, by = 8)) +
  labs(y = DOC_label,
       x = "Week") +
  theme_bw(base_size = 10)
       # title = "Buffalo Pound DOC concentrations (1983–2020)",
       # subtitle = "Red dots are dates of ice-on and ice-off; blue line is mean DOC concentration (6.7 mg/L)")


doc_ice %>% 
  group_by(Ice) %>% 
  summarise(ice_dates_mean = mean(ice_week, na.rm = TRUE))



# Temperature -------------------------------------------------------------
# weekly sampling from 1983—2019

unique(BP_longterm_cc$parameter)

bp_temp <- BP_longterm_cc %>% 
  filter(parameter == "Temperature") 

unique(bp_temp$year) # 1983—2019

bp_temp %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_temp %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col()

# Conductivity ------------------------------------------------------------
# weekly to monthly from 1983—2019

unique(BP_longterm_cc$parameter)

bp_cond <- BP_longterm_cc %>% 
  filter(parameter == "Conductivity") 

unique(bp_cond$year) # 1983—2019

bp_cond %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_cond %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# pH ----------------------------------------------------------------------
# weekly from 1983—2019

bp_pH <- BP_longterm_cc %>% 
  filter(parameter == "pH") 

unique(bp_pH$year) # 1983—2019

bp_pH %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_pH %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 52, col = "red")


# Chl a -------------------------------------------------------------------
# Varies weekly to monthly from 1983—2019

bp_Chla <- BP_longterm_cc %>% 
  filter(parameter == "Chlorophyll a") 

unique(bp_Chla$year) # 1983—2019

bp_Chla %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_Chla %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Colour ------------------------------------------------------------------
# Varies weekly to monthly from 1983—2019

bp_colour <- BP_longterm_cc %>% 
  filter(parameter == "Colour (Apparent)") 

unique(bp_colour$year) # 1983—2019

bp_colour %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_colour %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Turbidity ---------------------------------------------------------------
# weekly 1983—2019

bp_turb <- BP_longterm_cc %>% 
  filter(parameter == "Turbidity") 

unique(bp_turb$year) # 1983—2019

bp_turb %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_turb %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Alkalinity --------------------------------------------------------------
# weekly to monthly 1983—2019

bp_talk <- BP_longterm_cc %>% 
  filter(parameter == "Alkalinity (total)") 

unique(bp_talk$year) # 1983—2019

bp_talk %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_talk %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Hardness --------------------------------------------------------------
# weekly to monthly 1983—2019

bp_hardness <- BP_longterm_cc %>% 
  filter(parameter == "Hardness (total)") 

unique(bp_hardness$year) # 1983—2019

bp_hardness %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_hardness %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")

# TP --------------------------------------------------------------
# weekly to monthly 1983—2019

bp_TP <- BP_longterm_cc %>% 
  filter(parameter == "Phosphate (total)") 

unique(bp_TP$year) # 1983—2019

bp_TP %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_TP %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")

# SRP --------------------------------------------------------------
# weekly to monthly 1983—2019

bp_SRP <- BP_longterm_cc %>% 
  filter(parameter == "Phosphate (ortho)") 

unique(bp_SRP$year) # 1983—2019

bp_SRP %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_SRP %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# SO4 --------------------------------------------------------------
# weekly to monthly 1983—2019

bp_SO4 <- BP_longterm_cc %>% 
  filter(parameter == "Sulphate") 

unique(bp_SO4$year) # 1983—2019

bp_SO4 %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_SO4 %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Organic N ---------------------------------------------------------------
# weekly to monthly 1983—2019

bp_OrgN <- BP_longterm_cc %>% 
  filter(parameter == "Organic N") 

unique(bp_OrgN$year) # 1983—2019

bp_OrgN %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_OrgN %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Nitrate -----------------------------------------------------------------
# weekly to monthly 1983—2019

bp_NO3 <- BP_longterm_cc %>% 
  filter(parameter == "Nitrate") 

unique(bp_NO3$year) # 1983—2019

bp_NO3 %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_NO3 %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")


# Ammonia N ---------------------------------------------------------------
# weekly to monthly 1983—2019

bp_NH3 <- BP_longterm_cc %>% 
  filter(parameter == "Ammonia N") 

unique(bp_NH3$year) # 1983—2019

bp_NH3 %>% 
  ggplot(aes(year, result)) + 
  geom_point()

bp_NH3 %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year, n)) + 
  geom_col() +
  geom_hline(yintercept = 12, col = "red") +
  geom_hline(yintercept = 52, col = "red")

