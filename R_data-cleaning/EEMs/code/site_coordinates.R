# [1]  Upstream of Causeway Centre -- 50.722927, -105.605669             
# [2]  Upstream of Causeway East -- 50.722156, -105.598620           
# [3]  1.5 km below Qu'Appelle R. Inflow Centre -- 50.738224, -105.629690
# [4]  1.5 km below Qu'Appelle R. Inflow East -- 50.740458, -105.627255
# [5]  1.5 km below Qu'Appelle R. Inflow West -- 50.735799, -105.632272
# [6]  0.5 mi Below Causeway -- 50.711847, -105.590922                   
# [7]  Opposite South Lake -- 50.680467, -105.556099                     
# [8]  Opposite Sun Valley -- 50.658001, -105.523099                    
# [9]  Opposite Parkview -- 50.620929, -105.450768                        
# [10] Opposite WTP Intake -- 50.586174, -105.383313                     
# [11] 0.5 mi Above Outlet -- 50.571910, -105.342087

# site_coords_wip <- function() { ##### coords guessed from site names
#   
#   site_coords <- tidyr::tribble(
#     
#     ~site_name,                                 ~site_code_long,             ~latitude, ~longitude,
#     "Upstream of Causeway Centre",              "Upstream Causeway Centre",  50.722927, -105.605669,  
#     "Upstream of Causeway East",                "Upstream Causeway East",    50.722156, -105.598620,           
#     "1.5 km below Qu'Appelle R. Inflow Centre", "Inflow Centre",             50.740555, -105.633243,
#     "1.5 km below Qu'Appelle R. Inflow East",   "Inflow East",               50.742877, -105.630260,
#     "1.5 km below Qu'Appelle R. Inflow West",   "Inflow West",               50.738909, -105.636231,
#     "0.5 mi Below Causeway",                    "Below Causeway",            50.711847, -105.590922,                   
#     "Opposite South Lake",                      "Opposite South Lake",       50.680467, -105.556099,                     
#     "Opposite Sun Valley",                      "Opposite Sun Valley",       50.658001, -105.523099,                   
#     "Opposite Parkview",                        "Opposite Parkview",         50.620929, -105.450768,                        
#     "Opposite WTP Intake",                      "WTP Intake",                50.586174, -105.383313,                     
#     "0.5 mi Above Outlet",                      "Above Outlet",              50.571910, -105.342087,
#   )
#   
# }

site_coords_wip <- function() { ##### coords taken from BuffalloPound_DOM_AllData.csv
  
  library(geosphere)
  
  site_coords <- tidyr::tribble(
    
    ~site_num,  ~site_name,                                 ~site_code_long,             ~latitude, ~longitude,
    1,          "1.5 km below Qu'Appelle R. Inflow East",   "Inflow East",               50.736322,	-105.623713,
    2,          "1.5 km below Qu'Appelle R. Inflow Centre", "Inflow Centre",             50.735692,	-105.626390,
    3,          "1.5 km below Qu'Appelle R. Inflow West",   "Inflow West",               50.734637,	-105.629262,
    4,          "Upstream of Causeway Centre",              "Upstream Causeway Centre",  50.721563,	-105.603458, 
    5,          "Upstream of Causeway East",                "Upstream Causeway East",    50.722173,	-105.599311,
    6,          "0.5 mi Below Causeway",                    "Below Causeway",            50.711590,	-105.587770,  
    7,          "Opposite South Lake",                      "Opposite South Lake",       50.682950,	-105.559100,                     
    8,          "Opposite Sun Valley",                      "Opposite Sun Valley",       50.656150,	-105.520580,                   
    9,          "Opposite Parkview",                        "Opposite Parkview",         50.617800,	-105.443830,                        
    10,         "Opposite WTP Intake",                      "WTP Intake",                50.586530,	-105.384210,                     
    11,         "0.5 mi Above Outlet",                      "Above Outlet",              50.572630,	-105.330270,
    
  ) %>% 
    arrange(site_num)
  
  # https://www.movable-type.co.uk/scripts/latlong.html
  # Use the ‘haversine’ formula to calculate the great-circle distance between
  # two points – that is, the shortest distance over the earth’s surface –
  # giving an ‘as-the-crow-flies’ distance between the points.
  # 
  # See also 
  
  inflow_east <- c(site_coords$longitude[1], site_coords$latitude[1])
  inflow_centre <- c(site_coords$longitude[2], site_coords$latitude[2])
  inflow_west <- c(site_coords$longitude[3], site_coords$latitude[3])
  upstream_causeway_centre <- c(site_coords$longitude[4], site_coords$latitude[4])
  upstream_causeway_east <- c(site_coords$longitude[5], site_coords$latitude[5])
  below_causeway <- c(site_coords$longitude[6], site_coords$latitude[6])
  opposite_south_lake <- c(site_coords$longitude[7], site_coords$latitude[7])
  opposite_sun_valley <- c(site_coords$longitude[8], site_coords$latitude[8])
  opposute_parkview <- c(site_coords$longitude[9], site_coords$latitude[9])
  wtp_intake <- c(site_coords$longitude[10], site_coords$latitude[10])
  above_outlet <- c(site_coords$longitude[11], site_coords$latitude[11])
  
  dd1 <- distHaversine(p1 = inflow_east, p2 = inflow_east)
  dd2 <- distHaversine(p1 = inflow_centre, p2 = inflow_east)
  dd3 <- distHaversine(p1 = inflow_west, p2 = inflow_east)
  dd4 <- distHaversine(p1 = upstream_causeway_centre, p2 = inflow_east)
  dd5 <- distHaversine(p1 = upstream_causeway_east, p2 = inflow_east)
  dd6 <- distHaversine(p1 = below_causeway, p2 = inflow_east)
  dd7 <- distHaversine(p1 = opposite_south_lake, p2 = inflow_east)
  dd8 <- distHaversine(p1 = opposite_sun_valley, p2 = inflow_east)
  dd9 <- distHaversine(p1 = opposute_parkview, p2 = inflow_east)
  dd10 <- distHaversine(p1 = wtp_intake, p2 = inflow_east)
  dd11 <- distHaversine(p1 = above_outlet, p2 = inflow_east)
  
  site_coords_Haversine <- site_coords %>% 
    mutate(distHaversine_m = c(dd1, dd2, dd3, dd4, dd5, dd6, dd7, dd8, dd9, dd10, dd11),
           distHaversine_km = distHaversine_m / 1000)
  
  return(site_coords_Haversine)
  
}

# scwip <- site_coords_wip() %>% mutate(distHaversine_km = distHaversine_m / 1000)

# write_csv(scwip, paste0(getwd(), "/R_data-cleaning/EEMs/data/clean/site_coordinates.csv"))



