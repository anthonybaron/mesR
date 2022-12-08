library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(dplyr)
library(mapcan)
library(patchwork)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))


# Read and prep data ------------------------------------------------------

source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
stations <- station_lat_long() %>% mutate(latitude = ifelse(site_name == "Buffalo Pound inflow", 50.776, latitude))

source("./R_data-cleaning/EEMs/code/site_coordinates.R")
sites <- subset(site_coords_wip(), !site_num %in% c(1, 3, 5))
sites$site_code_long <- ifelse(sites$site_num == "2", "Lake Inflow", sites$site_code_long)
sites$site_code_long <- ifelse(sites$site_num == "4", "Upstream Causeway", sites$site_code_long)
sites_c <- sites[["site_code_long"]]
sites$site_code_long <- factor(sites$site_code_long, levels = sites_c)
sites$longitude <- ifelse(sites$site_code_long == "Above Outlet", -105.335, sites$longitude)


# Lake and river files
mm <- st_read("~/Downloads/ghy_000c11a_e/ghy_000c11a_e.shp")
ffa <- read_sf("~/Downloads/ghy_000c11a_e/ghy_000c11a_e.shp")
rra <- read_sf("~/Downloads/ghy_000d11a_e/ghy_000d11a_e.shp")
wwa <- read_sf("~/Downloads/ghy_000d06a_e/ghy_000d06a_e.shp")

# st_crs(ffa) # North American Datum 1983
# st_crs(rra) # North American Datum 1983

# Buffalo Pound gross areas (convert WGS 84 / Pseudo-Mercator to North American Datum 1983)
bga <- read_sf("./R_maps/Gross_areas-Colin/05JG004_gross_area.shp") %>% st_transform(crs = 4269)
rga <- read_sf("./R_maps/Gross_areas-Colin/05JG013_gross_area.shp")  %>% st_transform(crs = 4269)
iga <- read_sf("./R_maps/Gross_areas-Colin/05JG014_gross_area.shp") %>% st_transform(crs = 4269)
g005 <- read_sf("./R_maps/Gross_areas-Colin/05JG005_gross_area.shp") %>% st_transform(crs = 4269)
g007 <- read_sf("./R_maps/Gross_areas-Colin/05JG007_gross_area.shp") %>% st_transform(crs = 4269)
g010 <- read_sf("./R_maps/Gross_areas-Colin/05JG010_gross_area.shp") %>% st_transform(crs = 4269)
g011 <- read_sf("./R_maps/Gross_areas-Colin/05JG011_gross_area.shp") %>% st_transform(crs = 4269)

gross_areas <- bind_rows(list(bga, rga, iga, g005, g010, g011))

# Buffalo Pound effective areas (convert WGS 84 / Pseudo-Mercator to North American Datum 1983)
bea <- read_sf("./R_maps/effective_areas-Colin/05JG004_effective_area.shp") %>% st_transform(crs = 4269)
rea <- read_sf("./R_maps/effective_areas-Colin/05JG013_effective_area.shp") %>% st_transform(crs = 4269)
iea <- read_sf("./R_maps/effective_areas-Colin/05JG014_effective_area.shp") %>% st_transform(crs = 4269)
e005 <- read_sf("./R_maps/effective_areas-Colin/05JG005_effective_area.shp") %>% st_transform(crs = 4269)
e007 <- read_sf("./R_maps/effective_areas-Colin/05JG007_effective_area.shp") %>% st_transform(crs = 4269)
e010 <- read_sf("./R_maps/effective_areas-Colin/05JG010_effective_area.shp") %>% st_transform(crs = 4269)
e011 <- read_sf("./R_maps/effective_areas-Colin/05JG011_effective_area.shp") %>% st_transform(crs = 4269)

effective_areas <- bind_rows(list(bea, rea, iea, e005, e010, e011))


# Canada map
cda <- read_sf("~/Downloads/gpr_000b11a_e/gpr_000b11a_e.shp") %>% transform(crs = 4269)


# Find Lake Diefenbaker... ------------------------------------------------

# for dief 
# subset ffa for PRUID == "47"
# subset ffa for RANK %in% c("2", "7") 
# grepl "Sask", may be under sk river

# sask data
sk <- subset(ffa, PRUID == "47")

# different ranks
rank2 <- subset(sk, RANK == "2")
rank7 <- subset(sk, RANK == "7") # rank 7 not needed... Dief in rank 2
ranks <- subset(sk, RANK %in% c("2", "7"))
ranks$RANK <- factor(ranks$RANK, levels = c("2", "7"))

# plot ranks 2 and 7
# ggplot(ranks) + geom_sf(aes(col = RANK, fill = RANK)) + coord_sf(xlim = c(NA, -105), ylim = c(50.5, 51.5))

# subset SK River
skriv <- subset(sk, grepl("Saskatchewan", NAME))
skriv$RANK <- factor(skriv$RANK, levels = c("6"))

# plot SK River
# ggplot(skriv) + geom_sf() + coord_sf()

# subset and plot names in sk containing "Arm" and at rank 2
# Thomson Arm, Gordon McKenzie Arm
arms <- subset(rank2, grepl("Arm", NAME))
# ggplot(arms) + geom_sf() + coord_sf()

# looks like this part of the South SK River is what I'm looking for
# ggplot(subset(skriv, HYDROUID == "8190105")) + geom_sf() + coord_sf()

bpd <- subset(sk, grepl("Buffalo Pound Lake|Thomson Arm|Gordon McKenzie Arm|Eyebrow Lake", NAME) | HYDROUID == "8190105")
bpd$NAME <- ifelse(bpd$HYDROUID == "8190105", "South Saskatchewan River", bpd$NAME)



# Start plotting ----------------------------------------------------------

fillblue <- "#BEE8FF"
colblue <- "#00C5FF"
colyellow <- "#FFEBBE"
viridisgreen <- "#AADC32FF"
viridisgreen2 <- "#8FD744FF"

p_bpd <- ggplot(bpd) + 
  geom_sf(col = colblue, size = 1.5) +
  geom_sf(data = bpd, fill = fillblue, col = NA) 

bp <- subset(bpd, NAME == "Buffalo Pound Lake")
p_bp <- ggplot(bp) + 
  geom_sf(col = "#00C5FF", size = 1) +
  geom_sf(data = bp, fill = fillblue, col = NA) 




# Sask rivers
skr <- subset(rra, PRUID == "47")
qpr_all <- subset(skr, grepl("Qu'Appelle River", NAME))

p_qpr_all <- ggplot(qpr_all) + 
  geom_sf(col = colblue) +
  coord_sf()

p_bqd <- p_bpd + 
  geom_sf(data = qpr_all, col = colblue, size = 1.15) 



# Add gross and effective areasd  -----------------------------------------

p_bpd <- ggplot(bpd) + 
  geom_sf(col = colblue, size = 1.5) +
  geom_sf(data = bpd, fill = fillblue, col = NA) 

p_bqd <- p_bpd + 
  geom_sf(data = qpr_all, col = colblue, size = 1.15) 

p_bqd + 
  geom_sf(data = bga) + 
  coord_sf(xlim = c(-106.65, -105.38), ylim = c(50.5, 51.333))  

# trial 1
# ggplot() + 
#   geom_sf(data = bga, col = NA) + 
#   geom_sf(data = bea, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = rea, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = bpd, col = colblue, size = 1.5) + 
#   geom_sf(data = bpd, fill = fillblue, col = NA) +
#   geom_sf(data = qpr_all, col = colblue, size = 1) +
#   geom_sf(data = bga, size = 0.85,  fill = NA) +
#   coord_sf(xlim = c(-106.65, -105.38), ylim = c(50.58, 51.4))

# trial 2
# ggplot() + 
#   geom_sf(data = gross_areas, col = NA) + 
#   geom_sf(data = effective_areas, fill = "#FFEBBE", col = "grey60") +
#   # geom_sf(data = rea, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = bpd, col = colblue, size = 1.5) + 
#   geom_sf(data = bpd, fill = fillblue, col = NA) +
#   geom_sf(data = qpr_all, col = colblue, size = 1) +
#   geom_sf(data = gross_areas, size = 0.85,  fill = NA) +
#   coord_sf(xlim = c(-106.65, -105.38), ylim = c(50.58, 51.4))

# trial 3
# ggplot() + 
#   geom_sf(data = bga, col = NA) + 
#   # geom_sf(data = rga, col = NA) # same as bga
#   # geom_sf(data = iga, col = NA) # same as bga 
#   geom_sf(data = g005, col = NA) +
#   geom_sf(data = bea, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = e005, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = bpd, col = colblue, size = 1.5) + 
#   geom_sf(data = bpd, fill = fillblue, col = NA) +
#   geom_sf(data = qpr_all, col = colblue, size = 1) +
#   # geom_sf(data = bga, size = 0.85,  fill = NA) +
#   geom_sf(data = g005, size = 0.85,  fill = NA) +
#   coord_sf(xlim = c(-106.65, -105.36), ylim = c(50.52, 51.38))

# trial 4
# ggplot() + 
#   geom_sf(data = bb005, col = NA)
#   # geom_sf(data = bga, col = NA) + 
#   # geom_sf(data = g005, col = NA) +
#   geom_sf(data = bea, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = e005, fill = "#FFEBBE", col = "grey60") +
#   geom_sf(data = bpd, col = colblue, size = 1.5) + 
#   geom_sf(data = bpd, fill = fillblue, col = NA) +
#   geom_sf(data = qpr_all, col = colblue, size = 1) +
#     geom_sf(data = bb005, size = 0.85,  fill = NA) +
#   # geom_sf(data = bga, size = 0.85,  fill = NA) +
#   # geom_sf(data = g005, size = 0.85,  fill = NA) +
#   coord_sf(xlim = c(-106.65, -105.36), ylim = c(50.52, 51.38))
  
# bb005 <- bind_rows(bga, g005)
# bb005 <- st_combine(bb005)
# class(bb005)

# tmp <- st_union(bb005)
# class(tmp)

# trial 5 ### winner

lake_labs <- tibble::tribble(
  ~"label",             ~"latitude", ~"longitude",
  "L.\nDiefenbaker",    51.06,       -106.55,
  "Buffalo\nPound\nL.", 50.67,       -105.54,
  "Eyebrow\nL.",        50.95,       -106.18
)

river_labs <- tibble::tribble(
  ~"label",             ~"latitude", ~"longitude",
  "Qu'Appelle R.",      50.84,       -105.88,
)

station_labs <- tibble::tribble(
  ~"label",                  ~"latitude", ~"longitude",
  "L. Diefenbaker\noutflow",  50.97,      -106.39,
  "Ridge Creek",              50.95,      -106.32,
  "Iskwao Creek",             50.97,      -105.95,
  "Buffalo Pound L.\ninflow", 50.776,     -105.82
)

wtp_lab <- tibble::tribble(
  ~"label",         ~"latitude", ~"longitude", 
   "BPWTP\nintake",  50.585778,    -105.3834   
)

bpd$label <- ifelse(bpd$NAME == "Gordon McKenzie Arm", "L.\nDiefenbaker", NA)
bpd$label <- ifelse(bpd$NAME == "Buffalo Pound Lake", "Buffalo\nPound\nL.", bpd$label)

wtp <- data.frame(latitude = 50.585778, longitude = -105.3834)

catchment_plot <- ggplot() + 
  geom_sf(data = g005, col = NA) +
  geom_sf(data = e005, fill = viridisgreen, col = "grey60", alpha = 1/5) +
  geom_sf(data = bpd, col = colblue, size = 1.5) + 
  geom_sf(data = bpd, fill = fillblue, col = NA) +
  geom_sf(data = qpr_all, col = colblue, size = 1) +
  geom_sf(data = g005, size = 0.75,  fill = NA, col = "grey40") +
  geom_sf(data = subset(rra, grepl("Iskwao", NAME)), col = colblue, size = 0.5) +
  coord_sf(xlim = c(-106.65, -105.36), ylim = c(50.532, 51.36)) + 
  geom_point(data = wtp, aes(longitude, latitude),
             shape = 21, col = "black", fill = "#FAFA33", size = 6) + 
  geom_point(data = stations, aes(longitude, latitude),
             shape = 23, col = "black", fill = "red", size = 3.5) + 
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.4, fontface = "bold.italic", col = "white",
            nudge_x = 0.002) + 
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.5, fontface = "bold.italic", col = "white") +
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white",
            nudge_y = 0.002) + 
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white",
            nudge_y = -0.002) + 
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white",
            nudge_y = -0.001) + 
  geom_text(data = lake_labs, aes(longitude, latitude, label = label),
            size = 3.4, fontface = "bold.italic", col = "black") +
  ## river labels
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.4, fontface = "bold.italic", col = "white",
            nudge_x = 0.002, angle =  320) + 
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.5, fontface = "bold.italic", col = "white",
            angle =  320) +
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white",
            nudge_y = 0.002, angle =  320) + 
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white",
            nudge_y = -0.002, angle =  320) + 
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.6, fontface = "bold.italic", col = "white", 
            nudge_y = -0.001, angle =  320) + 
  geom_text(data = river_labs, aes(longitude, latitude, label = label),
            size = 3.4, fontface = "bold.italic", col = "black",
            angle =  320) +
  annotation_scale(location = "bl", width_hint = 0.8, text_cex = 0.75,
                   height = unit(0.2, "cm")) +
  geom_text_repel(data = station_labs[1, ], aes(longitude, latitude, label = label),
                  nudge_x = -0.22,
                  nudge_y = 0.01,
                  size = 3,
                  point.padding = 0.5) +
  geom_text_repel(data = station_labs[2, ], aes(longitude, latitude, label = label),
                  nudge_x = -0.28,
                  nudge_y = -0.019,
                  size = 3,
                  point.padding = 0.5) +
  geom_text_repel(data = station_labs[3, ], aes(longitude, latitude, label = label),
                  nudge_x = 0.19,
                  nudge_y = 0.03,
                  size = 3,
                  point.padding = 0.5) +
  geom_text_repel(data = station_labs[4, ], aes(longitude, latitude, label = label),
                  nudge_x = 0.32,
                  nudge_y = 0.01,
                  size = 3,
                  point.padding = 0.5) +
  geom_text_repel(data = wtp_lab, aes(longitude, latitude, label = label),
                  nudge_x = -0.25,
                  nudge_y = -0.043,
                  fontface = 'bold',
                  size = 3,
                  point.padding = 0.8) +
  annotation_north_arrow(pad_x = unit(0.4, "cm"), pad_y = unit(0.8, "cm"), 
                         width = unit(1.25, "cm"), height = unit(1.4, "cm"),
                         style = north_arrow_orienteering(fill = c("white", "black"), text_size = 7)) + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent")) + 
  labs(x = NULL, y = NULL)
catchment_plot

# add map of Canada inset
cda <- select(cda, PRUID, PRENAME, geometry)
cda <- mapcan(boundaries = province, type = standard) %>% as_tibble()
cda_sf <- st_as_sf(x = cda, coords = c("long", "lat"), crs = 3857) %>% st_transform(crs = 4269)
cda_df <- as_tibble(cda_sf)

pgon1 <- tibble::tribble(
  ~"point",       ~"long", ~"lat", 
  "bottom left",  -790000, 280000,
  "top left",     -770000, 380000,
  "top right",    -668000, 367000,
  "bottom right", -680000, 262000
)

canlab <- tibble::tribble(
  ~"label",     ~"long",  ~"lat",
  "C A N A D A", -480000, 800000
)

canada_plot <- cda %>%
  ggplot() + 
  geom_polygon(data = cda, aes(x = long, y = lat, group = group), col = "grey60", fill = "grey90", alpha = 1/4) + 
  coord_fixed() +
  geom_polygon(data = pgon1, aes(long, lat), fill = "pink", col = "black") +
  geom_text(data = canlab, aes(long, lat, label = label),
            size = 3, angle = 349, fontface = "bold", col = "black") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) + 
  labs(x = NULL, y = NULL)

catchment_plot <- catchment_plot + 
  inset_element(canada_plot,
                left = 0.63, 
                bottom = 0.63,
                right = 1, 
                top = 1,
                on_top = FALSE)

#
#
#

# lake plot
sites$site_abbr <- dplyr::case_when(
  sites$site_code_long == "Lake Inflow" ~ "B1.7",
  sites$site_code_long == "Upstream Causeway" ~ "B3.8",
  sites$site_code_long == "Below Causeway" ~ "B5.2",
  sites$site_code_long == "Opposite South Lake" ~ "B9.0",
  sites$site_code_long == "Opposite Sun Valley" ~ "B13.0",
  sites$site_code_long == "Opposite Parkview" ~ "B19.8",
  sites$site_code_long == "WTP Intake" ~ "B25.2",
  sites$site_code_long == "Above Outlet" ~ "B29.1",
)

sites$dist_km <- dplyr::case_when(
  sites$site_code_long == "Lake Inflow" ~ 1.71,
  sites$site_code_long == "Upstream Causeway" ~ 3.75,
  sites$site_code_long == "Below Causeway" ~ 5.241089,
  sites$site_code_long == "Opposite South Lake" ~ 8.986356,
  sites$site_code_long == "Opposite Sun Valley" ~ 13.012424,
  sites$site_code_long == "Opposite Parkview" ~ 19.805607,
  sites$site_code_long == "WTP Intake" ~ 25.241939,
  sites$site_code_long == "Above Outlet" ~ 29.085310,
)

sites <- select(sites, site_code_long, site_abbr, latitude, longitude, dist_km)
sites$site_abbr <- factor(sites$site_abbr, 
                          levels = c("B1.7", "B3.8", "B5.2", "B9.0", 
                                     "B13.0", "B19.8", "B25.2", "B29.1"))

bplake_plot <- ggplot() +
  geom_sf(data = g005, col = NA) +
  geom_sf(data = e005, fill = viridisgreen, col = "grey60", alpha = 1/5) +
  geom_sf(data = bpd, col = colblue, size = 1.5) + 
  geom_sf(data = bpd, fill = fillblue, col = NA) +
  geom_sf(data = qpr_all, col = colblue, size = 1) +
  geom_sf(data = g005, size = 0.75,  fill = NA, col = "grey40") + 
  geom_point(data = sites, aes(longitude, latitude, fill = site_abbr),
             shape = 23, size = 3.5, col = "white") +
  geom_point(data = sites, aes(longitude, latitude, fill = site_abbr),
             shape = 23, size = 3.8, col = "black") +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option = 'viridis') + 
  coord_sf(xlim = c(-105.67, -105.33), ylim = c(50.57, 50.76)) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        legend.position = c(0.875, 0.75),
        legend.text = element_text(size = 11),
        # legend.background = element_rect(color = "#323232", size = 0.35),
        legend.title = element_text(face = "bold"),
        # legend.title.align = -0.5,
        legend.margin = margin(c(5, 5, 5, 5))) + 
  labs(x = NULL, y = NULL, fill = "Sampling\nLocation") + 
  annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.75,
                   height = unit(0.2, "cm")) + 
  annotation_north_arrow(pad_x = unit(0.4, "cm"), pad_y = unit(0.8, "cm"), 
                         width = unit(1.25, "cm"), height = unit(1.4, "cm"),
                         style = north_arrow_orienteering(fill = c("white", "black"), text_size = 7)) 

p_outname <- "./R_maps/outputs/figures/"
dd <- "20221031_"
ggsave(paste0(p_outname, dd, "m_catchment.png"), catchment_plot, w = 7, h = 7.5)
ggsave(paste0(p_outname, dd, "m_bplake.png"), bplake_plot, w = 7, h = 7)



# workspace ---------------------------------------------------------------

# library(rgdal)
# library(geojsonio)
# library(spdplyr)
# library(rmapshaper)
# 
# canada_raw <- rgdal::readOGR(dsn = "~/Downloads/gpr_000b11a_e/", layer = "gpr_000b11a_e")
# 
# canada_raw_json <- geojson_json(canada_raw)
# 
# 
# canada_raw_sim <- ms_simplify(canada_raw_json)
# 
# geojson_write(canada_raw_sim, file = "~/Downloads/gpr_000b11a_e/canada_cd_sim.geojson")
# 
# install.packages("sf")
# install.packages("rgdal")
# install.packages("geojsonio")
# install.packages("spdplyr")
# install.packages("rmapshaper")






# extras ------------------------------------------------------------------


  


# 13U 464652.1303487 5611023.7189501

# ggplot() + 
#   geom_map(aes("Canada"))
#  
# map_data("world", "Canada") %>% 
#   ggplot(aes(long, lat)) + 
#   geom_polygon()
# 
# library(spData)
# 
# ?world()
# 
# spData::world
# 
# ww <- st_read(system.file("shapes/world.gpkg", package = "spData"))
# 
# cc <- subset(ww, name_long == "Canada")
# cc <- st_transform(cc, crs = 4269)
# 
# ggplot() + 
#   geom_sf(data = cc, fill = "white") + 
#   geom_sf(data = north_carolina_2163_bb, fill = NA, color = "red", size = 1.2) +
#   theme_void()
# 
# library(raster)
# cc <- getData("GADM", country = "CAN", level = 1)
# plot(cc)
