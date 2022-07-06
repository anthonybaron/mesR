library(sf)
library(ggplot2)
library(dplyr)

bp_ga <- st_read("./R_maps/Gross_areas-Colin/05JG004_gross_area.shp")
# using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 11 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: -11873880 ymin: 6560083 xmax: -11777370 ymax: 6688760
# Projected CRS: WGS 84 / Pseudo-Mercator

class(bp_ga)
# [1] "sf"         "data.frame"

st_geometry_type(bp_ga)
# [1] POLYGON
# 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE

levels(st_geometry_type(bp_ga))
# [1] "GEOMETRY"           "POINT"              "LINESTRING"         "POLYGON"            "MULTIPOINT"         "MULTILINESTRING"    "MULTIPOLYGON"       "GEOMETRYCOLLECTION"
# [9] "CIRCULARSTRING"     "COMPOUNDCURVE"      "CURVEPOLYGON"       "MULTICURVE"         "MULTISURFACE"       "CURVE"              "SURFACE"            "POLYHEDRALSURFACE" 
# [17] "TIN"                "TRIANGLE"   

st_crs(bp_ga)
# WGS 84 / Pseudo-Mercator

st_bbox(bp_ga)
# xmin        ymin      xmax      ymax 
# -11873882   6560083 -11777367   6688760

bp_ga

bp_ga %>% 
  ggplot() + 
  geom_sf(size = 0.5, col = "black", fill = "grey90") + 
  coord_sf()


bp_ea <- st_read("./R_maps/effective_areas-Colin/05JG004_effective_area.shp")
ridge_ga <- st_read("./R_maps/Gross_areas-Colin/05JG013_gross_area.shp")
ridge_ea <- st_read("./R_maps/effective_areas-Colin/05JG013_effective_area.shp")
iskwao_ga <- st_read("./R_maps/Gross_areas-Colin/05JG014_gross_area.shp")
iskwao_ea <- st_read("./R_maps/effective_areas-Colin/05JG014_effective_area.shp")

p1 <- ggplot() + 
  geom_sf(data = bp_ga, size = 0.5, col = "steelblue", fill = "grey90") + 
  geom_sf(data = bp_ea, size = 0.5, col = "steelblue", fill = "steelblue", alpha = 3/10) +
  geom_sf(data = ridge_ga, size = 0.5, col = "red", fill = "grey90") + 
  geom_sf(data = ridge_ea, size = 0.5, col = "red", fill = "red", alpha = 3/10) +
  geom_sf(data = iskwao_ga, size = 0.5, col = "green", fill = "grey90") + 
  geom_sf(data = iskwao_ea, size = 0.5, col = "green", fill = "green", alpha = 3/10) +
  coord_sf()

ggplot() + 
  geom_sf(data = bp_ga, size = 0.5, col = "black", fill = "grey30") + 
  geom_sf(data = bp_ea, size = 0.5, col = "black", fill = "steelblue", alpha = 3/10) +
  coord_sf()

ggplot() + 
  geom_sf(data = ridge_ga, size = 0.5, col = "black", fill = "grey60") + 
  geom_sf(data = ridge_ea, size = 0.5, col = "black", fill = "red", alpha = 3/10) +
  coord_sf()

ggplot() + 
  geom_sf(data = iskwao_ga, size = 0.5, col = "black", fill = "grey90") + 
  geom_sf(data = iskwao_ea, size = 0.5, col = "black", fill = "green", alpha = 3/10) +
  coord_sf()




# CanVec data -------------------------------------------------------------


cvs1 <- st_read("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/watercourse_1.shp")
cvs2 <- st_read("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/waterbody_2.shp")
cvs3 <- st_read("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/hydro_obstacle_0.shp")
cvs4 <- st_read("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/hydro_obstacle_1.shp")
cvs5 <- st_read("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/hydro_obstacle_2.shp")

cvs2a <- read_sf("./R_maps/CanVec/canvec_250K_SK_Hydro_shp/canvec_250K_SK_Hydro/waterbody_2.shp")

class(cvs1)
class(cvs2)
class(cvs3)
class(cvs4)
class(cvs5)

st_geometry_type(cvs1)
st_geometry_type(cvs2)
st_geometry_type(cvs3)
st_geometry_type(cvs4)
st_geometry_type(cvs5)

st_crs(cvs1) # GCS_North_American_1983_CSRS98 
st_crs(cvs2) # GCS_North_American_1983_CSRS98
st_crs(cvs3) # GCS_North_American_1983_CSRS98
st_crs(cvs4) # GCS_North_American_1983_CSRS98
st_crs(cvs5) # GCS_North_American_1983_CSRS98


p1 <- cvs1 %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()

p2 <- cvs2 %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()

p3 <- cvs3 %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()

p4 <- cvs4 %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()

p5 <- cvs5 %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()


?st_read

cvs2a <- cvs2a %>% select(feature_id:perm_en, geometry)
gg <- unlist(cvs2a$geometry)
class(gg)
cvs2a %>%
  filter(grepl("-105.", geometry) | grepl("-106.", geometry) | grepl("50.", geometry) | grepl("51.", geometry)) %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()


cvs2a %>% mutate(rownum = row_number()) %>% select(rownum, everything()) %>% 
  filter(rownum %in% c(1:10000)) %>% 
  ggplot() + 
  geom_sf() + 
  coord_sf()




# CanVec data request -----------------------------------------------------

mm <- read_sf("~/Downloads/FME_5644441C_1655920610145_1944/ESRISHAPE_1/canvec_220622_355018/waterbody_2.shp")
View(mm)
class(mm)
st_geometry_type(mm)
st_crs(mm)
mm %>% filter(perm_en == "Permanent" & definit_en %in% c("Lake")) %>% ggplot() + geom_sf() + coord_sf()
mm %>% filter(perm_en == "Permanent" & definit_en %in% c("Lake")) %>% View()
