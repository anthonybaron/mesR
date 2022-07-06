library(ggplot2)
library(sf)
library(cowplot)
library(ggrepel)
library(googleway)
library(ggspatial)
# library(libwgeom)
library(rnaturalearth)
library(rnaturalearthdata)

theme_set(theme_bw())

# The package rnaturalearth provides a map of countries of the entire world. Use
# ne_countries to pull country data and choose the scale (rnaturalearthhires is
# necessary for scale = "large"). The function can return sp classes (default)
# or directly sf classes, as defined in the argument returnclass:
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
class(world)

world %>% ggplot() + geom_sf()

world %>% 
  ggplot() + 
  geom_sf() +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))

world %>% 
  ggplot() + 
  geom_sf(col = "grey70", fill = "steelblue") +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))

world %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(trans = "sqrt", begin = 1, end = 0) +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))

# The function coord_sf allows to deal with the coordinate system, which
# includes both projection and extent of the map. By default, the map will use
# the coordinate system of the first layer that defines one (i.e. scanned in the
# order provided), or if none, fall back on WGS84 (latitude/longitude, the
# reference system used in GPS). Using the argument crs, it is possible to
# override this setting, and project on the fly to any projection. This can be
# achieved using any valid PROJ4 string (here, the European-centric ETRS89
# Lambert Azimuthal Equal-Area projection):
world %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_est)) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  scale_fill_viridis_c(trans = "sqrt", begin = 1, end = 0) +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))

# The extent of the map can also be set in coord_sf, in practice allowing to
# “zoom” in the area of interest, provided by limits on the x-axis (xlim), and
# on the y-axis (ylim). Note that the limits are automatically expanded by a
# fraction to ensure that data and axes don’t overlap; it can also be turned off
# to exactly match the limits provided with expand = FALSE:
world %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
  scale_fill_viridis_c(trans = "sqrt", begin = 1, end = 0) +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))

# scale_bar that allows to add simultaneously the north symbol and a scale bar
# into the ggplot map. Five arguments need to be set manually: lon, lat,
# distance_lon, distance_lat, and distance_legend. The location of the scale bar
# has to be specified in longitude/latitude in the lon and lat arguments. The
# shaded distance inside the scale bar is controlled by the distance_lon
# argument. while its width is determined by distance_lat. Additionally, it is
# possible to change the font size for the legend of the scale bar (argument
# legend_size, which defaults to 3). The North arrow behind the “N” north symbol
# can also be adjusted for its length (arrow_length), its distance to the scale
# (arrow_distance), or the size the N north symbol itself (arrow_north_size,
# which defaults to 6). Note that all distances (distance_lon, distance_lat,
# distance_legend, arrow_length, arrow_distance) are set to "km" by default in
# distance_unit; they can also be set to nautical miles with “nm”, or miles with
# “mi”.
world %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
  scale_fill_viridis_c(trans = "sqrt", begin = 1, end = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = grey(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  labs(x = "Longitude", y = "Latitude",
       title = "World map",
       subtitle = paste0("(", length(unique(world$name)), " countries)"))


