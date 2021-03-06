site_names_all <- function() {
  
  site_names <- tidyr::tribble(
    ~site_name,                                  ~site_altname,                        
    "Upstream of Causeway West",                 "BP u/s (upstream) causeway west",
    "Upstream of Causeway West",                 "BP u/s (upstream) causeway westside",
    "Upstream of Causeway West",                 "BP u/s (upstream) causeway WS (west side)",
    "Upstream of Causeway West",                 "BPL u/s (upstream) causeway W (west side)",
    "Upstream of Causeway West",                 "BPL u/s causeway west",
    "Upstream of Causeway West",                 "BPL u/s causeway West",
    "Upstream of Causeway West",                 "BPL ups Causeway west",
    "Upstream of Causeway West",                 "BPL upstream causeway West",
    "Upstream of Causeway West",                 "BPL Us Causeway West",
    "Upstream of Causeway West",                 "Buffalo Pound Lake Upstream of Causeway - WEST",  
    "Upstream of Causeway West",                 "Buffalo Pound Lake Upstream of Causeway- WEST",          
    "Upstream of Causeway West",                 "Buffalo Pound Lake US Causeway - WEST",
    "Upstream of Causeway West",                 "Buffalo Pound Lake US Causeway West",                    
    
    "Upstream of Causeway East",                 "BP u/s (upstream) causeway east",
    "Upstream of Causeway East",                 "BP u/s (upstream) causeway east side",
    "Upstream of Causeway East",                 "BP u/s (upstream) causeway ES (east side)",
    "Upstream of Causeway East",                 "BP u/s causeway east",
    "Upstream of Causeway East",                 "BP u/s Causeway East",
    "Upstream of Causeway East",                 "BP upstream causeway East",
    "Upstream of Causeway East",                 "BPL u/s (upstream) causeway (E - east side)",
    "Upstream of Causeway East",                 "BPL u/s causeway east",
    "Upstream of Causeway East",                 "BPL u/s causeway East",
    "Upstream of Causeway East",                 "BPL Us Causeway East",
    "Upstream of Causeway East",                 "Buffalo Pound Lake Upstream of Causeway - EAST",
    "Upstream of Causeway East",                 "Buffalo Pound Lake Upstream of Causeway- EAST",
    "Upstream of Causeway East",                 "Buffalo Pound Lake US Causeway - EAST",
    "Upstream of Causeway East",                 "Buffalo Pound Lake US Causeway East",
    
    "Upstream of Causeway Centre",               "BP u/s (upstream) causeway",
    "Upstream of Causeway Centre",               "BP u/s Causeway",
    "Upstream of Causeway Centre",               "BP u/s courseway",
    "Upstream of Causeway Centre",               "BP u/s upstream causeway",
    "Upstream of Causeway Centre",               "BP upstream causeway",
    "Upstream of Causeway Centre",               "BPL u/s (upstream) of causeway",
    "Upstream of Causeway Centre",               "BPL u/s causeway",
    "Upstream of Causeway Centre",               "BPL u/s Causeway",
    "Upstream of Causeway Centre",               "BPL u/s causeway Centre",
    "Upstream of Causeway Centre",               "BPL u/s causeway middle",
    "Upstream of Causeway Centre",               "BPL u/s Courseway",
    "Upstream of Causeway Centre",               "BPL Upstream Causeway",
    "Upstream of Causeway Centre",               "Buffalo Pound Lake upstream Causeway",
    "Upstream of Causeway Centre",               "Buffalo Pound Lake Upstream of Causeway",
    "Upstream of Causeway Centre",               "Buffalo Pound Lake US Causeway",
    "Upstream of Causeway Centre",               "Buffalo Pound Lake US Causeway Center",
    
    "0.5 mi Below Causeway",                     "BPL 0.5 mi below causeway",
    "0.5 mi Below Causeway",                     "BP 0.5 miles below causeway",
    "0.5 mi Below Causeway",                     "BP below causeway",
    "0.5 mi Below Causeway",                     "BP 0.5 d/s causeway",
    "0.5 mi Below Causeway",                     "BP 0.5m d/s Causeway",
    "0.5 mi Below Causeway",                     "BPL 0.5 mi Below Causeway",
    "0.5 mi Below Causeway",                     "BPL 0.5 miles below causeway",
    "0.5 mi Below Causeway",                     "BPL below causeway",
    "0.5 mi Below Causeway",                     "BPL downstream causeway",
    "0.5 mi Below Causeway",                     "Buffalo Pound Lake 0.5 mi below Causeway",
    "0.5 mi Below Causeway",                     "Buffalo Pound Lake 0.5 mi Below Causeway",
    "0.5 mi Below Causeway",                     "Buffalo Pound Lake D/s Causeway",
    "0.5 mi Below Causeway",                     "Buffalo Pound Lake DS Causeway",
    
    "0.5 mi Below Causeway West",                "Buffalo Pound Lake 0.5 mi Below Causeway - WEST",  
    "0.5 mi Below Causeway West",                "Buffalo Pound Lake 0.5 mi ds Causeway - WEST",
    
    "Opposite WTP Intake",                       "BP opp WTP",
    "Opposite WTP Intake",                       "BP opposite treatment plant",
    "Opposite WTP Intake",                       "BP opposite treatment plant intake",
    "Opposite WTP Intake",                       "BP treatment plant intake",
    "Opposite WTP Intake",                       "BPL opp WTP",
    "Opposite WTP Intake",                       "BPL opp WTP intake",
    "Opposite WTP Intake",                       "BPL Opp. WTP",
    "Opposite WTP Intake",                       "BPL opp. WTP (water treatment plant) intake",
    "Opposite WTP Intake",                       "BPL WTP",
    "Opposite WTP Intake",                       "Buffalo Pound Lake Opp Treatment Plant Intake",
    "Opposite WTP Intake",                       "Buffalo Pound Lake Opp WTP",
    
    "0.5 mi Above Outlet",                       "BP 0.5m above outlet",
    "0.5 mi Above Outlet",                       "BP 0.5 mi above outlet",
    "0.5 mi Above Outlet",                       "BPL 0.5 mi Abore Oulet",
    "0.5 mi Above Outlet",                       "BPL 0.5 miles above outlet",
    "0.5 mi Above Outlet",                       "BPL above Outlet",
    "0.5 mi Above Outlet",                       "Buffalo Pound Lake 0.5 mi above Outlet",
    "0.5 mi Above Outlet",                       "Buffalo Pound Lake 0.5mi above Outlet",
    "0.5 mi Above Outlet",                       "Buffalo Pound Lake Above Outlet",
    
    "0.5 mi Below Outlet",                       "BP @ Outlet",
    "0.5 mi Below Outlet",                       "BP outlet",
    "0.5 mi Below Outlet",                       "BP Outlet",
    "0.5 mi Below Outlet",                       "Buffalo Pound outlet",
    "0.5 mi Below Outlet",                       "Buffalo Pound Outlet",
    
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BP 1.5 km below Qu'ap.",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BP below Qu'ap inflow",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BP below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BP 1.5m Bellow Qu'Appelle inflow",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL 1.5 km below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL below Qu'ap inflow",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL Below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL bellow Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL below Qu'Appelle Centre",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL 1.5 mi below Qu'Appelle inflow",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL Qu'Appelle Inflow mid",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "BPL 1.5 Bellow QuAR",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "Buffalo Pound below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "Buffalo Pound Lake 1.5 km below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "Buffalo Pound Lake below Qu'Appelle",
    "1.5 km below Qu'Appelle R. Inflow Centre",  "Buffalo Pound Lake below Qu'Appelle Center",
    
    "1.5 km below Qu'Appelle R. Inflow West",    "BP 1.5 km below Qu'ap West",
    "1.5 km below Qu'Appelle R. Inflow West",    "BP lake below Qu'appelle inflow WS (west side)",
    "1.5 km below Qu'Appelle R. Inflow West",    "BP West 1.5 km below Qu'ap",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL 1.5 km below Qu'Ap W",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL below Qu'ap. inflow (W)",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL below Qu'Appelle west",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL Below Qu'Appelle West",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL 1.5 mi below Qu'Appelle inflow W",
    "1.5 km below Qu'Appelle R. Inflow West",    "BPL Qu'Appelle Inflow West",
    "1.5 km below Qu'Appelle R. Inflow West",    "Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow - WEST",
    "1.5 km below Qu'Appelle R. Inflow West",    "Buffalo Pound Lake1.5 km below Qu’Appelle - WEST",
    "1.5 km below Qu'Appelle R. Inflow West",    "Buffalo Pound Lake 1.5 km below Qu’Appelle R. Inflow- WEST",
    "1.5 km below Qu'Appelle R. Inflow West",    "Buffalo Pound Lake below Qu'Appelle West",
    
    "1.5 km below Qu'Appelle R. Inflow East",    "BP 1.5 km below Qu'ap East",
    "1.5 km below Qu'Appelle R. Inflow East",    "BP East 1.5 km below Qu'ap.",
    "1.5 km below Qu'Appelle R. Inflow East",    "BP lake below Qu'ap inflow ES (east side)",
    "1.5 km below Qu'Appelle R. Inflow East",    "BP 1.5m Bellow Qu'Appelle in flow east",
    "1.5 km below Qu'Appelle R. Inflow East",    "BPL 1.5 km below Qu'Appelle E",
    "1.5 km below Qu'Appelle R. Inflow East",    "BPL 1.5 below Qu'Appelle inflow E",
    "1.5 km below Qu'Appelle R. Inflow East",    "BPL below Qu'ap inflow (E - east side)",
    "1.5 km below Qu'Appelle R. Inflow East",    "BPL Below Qu'Appelle East",
    "1.5 km below Qu'Appelle R. Inflow East",    "BPL Qu'Appelle Inflow East",
    "1.5 km below Qu'Appelle R. Inflow East",    "Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow - EAST",
    "1.5 km below Qu'Appelle R. Inflow East",    "Buffalo Pound Lake 1.5 km below Qu’Appelle - EAST",
    "1.5 km below Qu'Appelle R. Inflow East",    "Buffalo Pound Lake 1.5 km below Qu’Appelle R. Inflow- EAST",
    "1.5 km below Qu'Appelle R. Inflow East",    "Buffalo Pound Lake below Qu'Appelle East",
    
    "Opposite South Lake",                       "BPL opp South lake",
    "Opposite South Lake",                       "BP opp South Lake",
    "Opposite South Lake",                       "BP opp. South Lake",
    "Opposite South Lake",                       "BP opposite S lake",
    "Opposite South Lake",                       "BP opposite south lake",
    "Opposite South Lake",                       "BPL opp South Lake",
    "Opposite South Lake",                       "BPL opp. South Lake",
    "Opposite South Lake",                       "BPL Opp. South Lake",
    "Opposite South Lake",                       "BPL South Lake",
    "Opposite South Lake",                       "Buffalo Pound Lake Opp South Lake",
    
    "Opposite Sun Valley",                       "BP opp Sun Valley",
    "Opposite Sun Valley",                       "BP opp. Sun Valley",
    "Opposite Sun Valley",                       "BP opposite Sun Valley",
    "Opposite Sun Valley",                       "BPL opp Sun Valley",
    "Opposite Sun Valley",                       "BPL opp. Sun Valley",
    "Opposite Sun Valley",                       "BPL Opp. Sun Valley",
    "Opposite Sun Valley",                       "BPL Sun Valley",
    "Opposite Sun Valley",                       "Buffalo Pound Lake Opp Sun Valley",
    
    "Opposite Parkview",                         "BP opp Parkview",
    "Opposite Parkview",                         "BP opp. park view",
    "Opposite Parkview",                         "BP opposite park view",
    "Opposite Parkview",                         "BP opp Partview",
    "Opposite Parkview",                         "BPL opp Parkview",
    "Opposite Parkview",                         "BPL opp Park View",
    "Opposite Parkview",                         "BPL Opp. Parkview",
    "Opposite Parkview",                         "BPL opposite of park view",
    "Opposite Parkview",                         "BPL Parkview",
    "Opposite Parkview",                         "Buffalo Pound Lake Opp Parkview",
    
    "Qu'Appelle River at Marquis",               "Qu'Appelle @ Marquis",
    "Qu'Appelle River at Marquis",               "Qu'Appelle at Marquis",
    "Qu'Appelle River at Marquis",               "Qu'Appelle R at Marquis",
    "Qu'Appelle River at Marquis",               "Qu'appelle River at Marquis",
    "Qu'Appelle River at Marquis",               "Qu'Appelle R @ Marquis",
    "Qu'Appelle River at Marquis",               "Qu'Appelle River @ Marquis",
    
    "Qu'Appelle River at Hwy 19",                "Qu'Appelle @ HWY19", 
    "Qu'Appelle River at Hwy 19",                "Qu'Appelle @ Highway 19", 
    "Qu'Appelle River at Hwy 19",                "Qu'Appelle at Highway 19",
    "Qu'Appelle River at Hwy 19",                "Qu'Appelle R at Hwy 19",
    "Qu'Appelle River at Hwy 19",                "Qu’Appelle R @ Hwy#19", 
    "Qu'Appelle River at Hwy 19",                "Qu’Appelle River at  Hwy#19", 
    "Qu'Appelle River at Hwy 19",                "Qu'Appelle River at Highway 19",
    
    "Iskwao Creek",                              "Iskwao Creek", 
    "Iskwao Creek 1",                            "Iskwao Creek 1",
    
    "Ridge Creek",                               "Ridge Creek ",
    "Ridge Creek 1",                             "Ridge Creek 1",
    
    "Moose Jaw Creek at TWP RD 184",             "Moose Jaw Creek at TWP RD 184",
    
    "Opposite Sun Valley (1 m)",                 "Buffalo Pound Lake Opp Sun Valley (1m)",
    
    "Opposite Sun Valley (3 m)",                 "Buffalo Pound Lake Opp Sun Valley (3m)",
    
    "Pelican Lake",                              "Pelican Lake",
    "Pelican Lake",                              "Pelican Lake (1 of 2)",
    "Pelican Lake",                              "Pelican Lake S (south) basin"
    
  )
  
  return(site_names)
  
}

snma <- site_names_all()
unique(snma$site_name)
unique(snma$site_altname)