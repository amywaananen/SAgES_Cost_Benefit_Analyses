#SAgES Cost-Benefit Analysis

# Started Feb 12, 2024 by AW 

# Summary: 

# This script performs a cost-benefit analysis for solar land use conversion in 
# Minnesota. Reads in spatially and/or temporally specific estimates of energy
# production, agricultural net benefits, and ecosystem services associated with 
# solar land use in Minnesota.

# Load packages -----------------------------------------------------------

library(tidyverse)
library(purrr)
library(readxl)
library(janitor)
library(sf)
library(terra)
library(patchwork)

# Read in data ------------------------------------------------------------

# Geodata
trs <- st_read("01_Data/shp_plan_mngeo_trs/trs.shp") %>%
  select(COUN, COUN_LC) %>%
  distinct()

# Geodata: Get GEOCODE with county data
sectpy3 <- st_read("01_Data/shp_plan_mndnr_public_land_survey/pls_sectpy3.shp") %>%
  mutate(TWPRNGSEC = as.numeric(paste(TOWN, RANG, SECT, sep = ''))) %>%
  select(GEOSECT, TOWN, RANG, SECT) %>%
  st_join(trs)

county_boundaries <- st_read("01_Data/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp")

# Ecosystem Services
ES <- read_excel('01_Data/ES_core_results_pls_sectpy3.xlsx') %>% 
  clean_names() %>% # Clean up column names
  left_join(sectpy3, by = join_by(geosect == GEOSECT)) %>%   # Merge with geo data right away
  rename("mn_county" = "COUN_LC") # rename county column for later merges

# Ag Cost-Benefit: Corn ($/acre)
row_collapse <- 4
nms <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', range = cell_rows(seq_len(row_collapse)), col_names = F)
nms <- lapply(nms, na.omit)
nms <- lapply(nms, paste, collapse = "_")

Ag_corn <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()

# Ag Cost-Benefit: Soy ($/acre)
row_collapse <- 4
nms <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', range = cell_rows(seq_len(row_collapse)), col_names = F)
nms <- lapply(nms, na.omit)
nms <- lapply(nms, paste, collapse = "_")

Ag_soy <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()

# Pivot data from wide to long --------------------------------------------

Ag_corn <- Ag_corn %>% 
  pivot_longer(
    cols = starts_with(c("corn", "turfgrass", "prairie", "grazing", "switchgrass")),
    names_to = c("land_use","year_min","year_max","farm_size"),
    names_sep = "_",
    values_to = "ag_USD_per_acre"
  )

Ag_soy <- Ag_soy %>% 
  pivot_longer(
    cols = starts_with(c("corn", "turfgrass", "prairie", "grazing", "switchgrass")),
    names_to = c("land_use","year_min","year_max","farm_size"),
    names_sep = "_",
    values_to = "ag_USD_per_acre"
  )


solar <- read.csv('04_shiny/Explore-SAgES-Cost-Benefits/data/combined_SAM_output.csv')
head(solar)

# Expand ag tables to include all years -----------------------------------

Ag_corn_long <- Ag_corn %>%
  mutate(year_min = as.numeric(year_min), year_max = as.numeric(year_max)) %>% 
  mutate(year = map2(year_min, year_max, seq)) %>%
  select(-c(year_min, year_max)) %>% 
  unnest(year) %>%
  ungroup %>% 
  arrange(fips, land_use, farm_size, year)

Ag_soy_long <- Ag_soy %>%
  mutate(year_min = as.numeric(year_min), year_max = as.numeric(year_max)) %>% 
  mutate(year = map2(year_min, year_max, seq)) %>%
  select(-c(year_min, year_max)) %>% 
  unnest(year) %>%
  ungroup %>% 
  arrange(fips, land_use, farm_size, year)

nw <- c("Kittson", "Roseau", "Lake of the Woods", "Marshall", "Pennington", "Red Lake", "Polk", "Beltrami", "Mahnomen", "Clearwater", "Clay", "Becker", "Hubbard", "Norman")
central <- c("Wilkin", "Otter Tail", "Wadena", "Traverse", "Big Stone", "Grant", "Douglas", "Stevens", "Pope", "Kandiyohi", "Stearns", "Meeker", "Wright", "Todd", "Benton","Morrison", "Mille Lacs", "Swift")
ne <- c("Koochiching", "St. Louis", "Itasca", "Cass", "Aitkin", "Crow Wing", "Carlton", "Kanabec", "Pine", "Lake", "Cook")
metro <- c('Sherburne', "Isanti", "Anoka", "Hennepin", "Carver", "Scott", "Dakota", "Ramsey", "Washington", "Chisago")
sw <- c("Lac qui Parle", "Chippewa","Renville", "Yellow Medicine", "Lincoln", "Lyon", "Redwood", "Pipestone", "Murray", "Cottonwood", "Rock", "Nobles", "Jackson")
sc <- c("McLeod", "Sibley", "Nicollet", "Le Sueur", "Brown", "Watonwan", "Blue Earth", "Waseca", "Martin", "Faribault")
se <- c("Rice", "Goodhue", "Wabasha", "Steele", "Dodge", "Olmsted", "Winona", "Freeborn", "Mower", "Fillmore", "Houston")

mn_regions <- data.frame(mn_county = c(nw, central, ne, metro, sw, sc, se),region = c(rep("Northwest", times = length(nw)), rep("Central", times = length(central)), rep("Northeast", times = length(ne)), rep("Metro", times = length(metro)), rep("Southwest", times = length(sw)), rep("South Central", times = length(sc)), rep("South East", times = length(se))))

# Combine Solar, Ag, and Ecosystem Services -------------------------------

# Make process for selecting one scenario and doing the merge
# 4*3*10*2*2*3 = 1440 possible combinations

# 1. panel type (20, 30, 45, fixed)
# 2. output (capacity/energy/capacity+energy)
# 3. cambium (10 scenarios)
# 4. previous land use (corn / soy)
# 5. agrivoltaic land use (turfgrass, pollinator)
# 6. farm size (small / medium / large)

# Select "defaults": MidCase, Prairie vs. Turfgrass, Medium, 45 fixed tilt, energy output

ss <- solar %>% 
  filter(panel_type %in% "fl", # 45 degree tilt
         output %in% "e", # output in energy ($/acre)
         Cambium %in% "MidCase") # MidCase Cambium scenario

aa <- Ag_corn_long %>% 
  filter(land_use %in% c("prairie", "turfgrass"), # Prairie and turfgrass
         farm_size %in% "medium") # medium farm size

# Parse columns from ES dataframe for different scenarios

ee_barren <- ES %>% 
  select(-c(grep("herbaceous", colnames(ES)))) %>% 
  select(-c(grep("baseline", colnames(ES))))

ee_prairie <- ES %>% 
  select(-c(grep("barren", colnames(ES)))) %>% 
  select(-c(grep("baseline", colnames(ES))))

ee_baseline <- ES %>% 
  select(-c(grep("herbaceous", colnames(ES)))) %>% 
  select(-c(grep("barren", colnames(.))))

# Assign carbon price(s)
# Assess range from a low/conservative estimate ($20/tC?) to $191/tC (EPA’s new proposed value)
carbon_price <- 20  

# Merge solar + ag data with ecosystem services data
# First for prairie
big_merge_prairie <- aa %>%
  filter(land_use == "prairie") %>% 
  select(-c(latitude, longitude)) %>%
  left_join(ss, by = c('mn_county', 'year')) %>%
  left_join(mn_regions, by = 'mn_county') %>%
  mutate(net_profit = value + ag_USD_per_acre) %>%
  filter(year < 2051) %>%
  filter(!is.na(mn_county)) %>% 
  left_join(ee_prairie %>% select(-mn_county), by = c("fips" = "county_geoid"), relationship = "many-to-many") %>% 
  mutate(net_C20 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*20,
         net_C40 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*40,
         net_C60 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*60,
         net_C80 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*80,
         net_C100 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*100,
         net_C120 = net_profit + (carbon_mv_kg_640ac_herbaceous_gcr_04_masked_sum / 907.185)*120)

# Then for turfgrass
# this is a placeholder -- turfgrass is not analogous to barren
big_merge_turfgrass <- aa %>%
  filter(land_use == "turfgrass") %>% 
  select(-c(latitude, longitude)) %>%
  left_join(ss, by = c('mn_county', 'year')) %>%
  left_join(mn_regions, by = 'mn_county') %>%
  mutate(net_profit = value + ag_USD_per_acre) %>%
  filter(year < 2051) %>%
  filter(!is.na(mn_county)) %>% 
  left_join(ee_barren %>% select(-mn_county), by = c("fips" = "county_geoid"), relationship = "many-to-many") %>% 
  # Calculate carbon value -- convert from kg to tons and multiply by assigned price
  mutate(net_C20 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*20,
         net_C40 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*40,
         net_C60 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*60,
         net_C80 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*80,
         net_C100 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*100,
         net_C120 = net_profit + (carbon_mv_kg_640ac_barren_land_gcr_04_masked_sum / 907.185)*120)



# Calculate cumulative returns --------------------------------------------

cumulativeROI_prairie <- big_merge_prairie %>% 
  group_by(id) %>% 
  summarize(totalROIC20 = sum(net_C20, na.rm = T),
            totalROIC40 = sum(net_C40, na.rm = T),
            totalROIC60 = sum(net_C60, na.rm = T),
            totalROIC80 = sum(net_C80, na.rm = T),
            totalROIC100 = sum(net_C100, na.rm = T),
            totalROIC120 = sum(net_C120, na.rm = T))

# Visualizations ----------------------------------------------------------

# Focus on a geographic subset

big_merge_prairie <- big_merge_prairie %>% 
  filter(mn_county == "Douglas") %>% 
  st_as_sf()

big_merge_turfgrass <- big_merge_turfgrass %>% 
  filter(mn_county == "Douglas") %>% 
  st_as_sf()

# Make plots

library(ggplot2)
library(sf)

ggplot(big_merge_turfgrass)+
  # geom_sf(county_boundaries)+
  geom_sf(aes(fill = net_C120))




