library(readr)
library(dplyr)
library(ggplot2)

coherence <- read_csv("./Untitled spreadsheet - Sheet1.csv")
coherence2 <- read_csv("./Untitled spreadsheet - Sheet2(1).csv")
coherence3 <- read_csv("./Untitled spreadsheet - Sheet3.csv")

coherence %>% 
  ggplot(aes(var_names, start)) + 
  geom_point() + 
  geom_point(data = coherence, aes(var_names, end)) +
  lims(y = c(0, 120))


var_order <- c("DOC and sulphate", "DOC and TP", "DOC and SRP", 
               "DOC and organic N", "DOC and ammonia", "DOC and nitrate")

var_order <- c("DOC and nitrate", "DOC and ammonia", "DOC and organic N",
               "DOC and SRP", "DOC and TP", "DOC and sulphate")

coherence2 %>% 
  mutate(var_name = factor(var_name),
         var_name = fct_relevel(var_name, var_order)) %>% 
  ggplot(aes(var_name, bands)) + 
  geom_point() + 
  geom_line() +
  geom_point(data = coherence2, aes(var_name, bands2)) + 
  geom_line(data = coherence2, aes(var_name, bands2)) +
  labs(x = NULL, y = "Timescales (months)",
       subtitle = "Timescale bands of significant coherence between DOC and drivers") +
  coord_flip() 
  

dief_order <- c("Nitrate and L. Diefenbaker outflow", "Ammonia and L. Diefenbaker outflow",
                "Organic N and L. Diefenbaker outflow", "SRP and L. Diefenbaker outflow",
                "TP and L. Diefenbaker outflow", "Sulphate and L. Diefenbaker outflow",
                "DOC and L. Diefenbaker outflow")

coherence3 %>% 
  filter(grepl("Diefenbaker", var_name)) %>% 
  mutate(var_name = fct_relevel(var_name, dief_order)) %>% 
  ggplot(aes(var_name, bands)) + 
  geom_point() + 
  geom_line() +
  geom_point(data = coherence3 %>% filter(grepl("Diefenbaker", var_name)), aes(var_name, bands2)) + 
  geom_line(data = coherence3 %>% filter(grepl("Diefenbaker", var_name)), aes(var_name, bands2)) +
  labs(x = NULL, y = "Timescales (months)",
       subtitle = "Timescale bands of significant coherence between L. Diefenbaker outflow and analytes") +
  coord_flip() 


rcic_order <- c("Nitrate and Ridge + Iskwao Creek flows", "Ammonia and Ridge + Iskwao Creek flows",
                "Organic N and Ridge + Iskwao Creek flows", "SRP and Ridge + Iskwao Creek flows",
                "TP and Ridge + Iskwao Creek flows", "Sulphate and Ridge + Iskwao Creek flows",
                "DOC and Ridge + Iskwao Creek flows")

coherence3 %>% 
  filter(grepl("Ridge", var_name)) %>% 
  mutate(var_name = fct_relevel(var_name, rcic_order)) %>% 
  ggplot(aes(var_name, bands)) + 
  geom_point() + 
  geom_line() +
  geom_point(data = coherence3 %>% filter(grepl("Ridge", var_name)), aes(var_name, bands2)) + 
  geom_line(data = coherence3 %>% filter(grepl("Ridge", var_name)), aes(var_name, bands2)) +
  labs(x = NULL, y = "Timescales (months)",
       subtitle = "Timescale bands of significant coherence between Ridge + Iskwao Creek flows and analytes") +
  coord_flip() 


bpl_order <- c("Nitrate and Buffalo Pound inflow", "Ammonia and Buffalo Pound inflow",
                "Organic N and Buffalo Pound inflow", "SRP and Buffalo Pound inflow",
                "TP and Buffalo Pound inflow", "Sulphate and Buffalo Pound inflow",
                "DOC and Buffalo Pound inflow")

coherence3 %>% 
  filter(grepl("Buffalo", var_name)) %>% 
  mutate(var_name = fct_relevel(var_name, bpl_order)) %>% 
  ggplot(aes(var_name, bands)) + 
  geom_point() + 
  geom_line() +
  geom_point(data = coherence3 %>% filter(grepl("Buffalo", var_name)), aes(var_name, bands2)) + 
  geom_line(data = coherence3 %>% filter(grepl("Buffalo", var_name)), aes(var_name, bands2)) +
  labs(x = NULL, y = "Timescales (months)",
       subtitle = "Timescale bands of significant coherence between Buffalo Pound inflow and analytes") +
  coord_flip() 
