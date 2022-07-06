# https://cran.r-project.org/web/packages/mapcan/vignettes/choropleth_maps_vignette.html

library(ggplot2)
library(mapcan)
library(dplyr)

mapcan(boundaries = province, type = standard) %>% head()
mapcan(boundaries = census, type = standard) %>% head()
mapcan(boundaries = ridings, type = standard) %>% head()

mapcan(boundaries = province, type = standard) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() +
  theme_mapcan()

pop_2017 <- mapcan::province_pop_annual %>% filter(year == 2017)

pr_geographic <- mapcan(boundaries = province, type = standard) %>% 
  inner_join(pop_2017, by = c("pr_english" = "province"))

pr_geographic %>% 
  ggplot(aes(long, lat, group = group, fill = population)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_mapcan() + 
  scale_fill_viridis_c(name = "Population", begin = 1, end = 0) + 
  ggtitle("Canadian population by province")

### 

sk_ridings <- mapcan(boundaries = ridings, type = standard, province = SK)

sk_ridings %>% 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(col = "grey30", fill = "steelblue") + 
  coord_fixed() + 
  theme_mapcan() + 
  ggtitle("Saskatchewan Federal Electoral Ridings")
