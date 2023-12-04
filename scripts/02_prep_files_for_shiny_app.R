# Author: Katja Lemermeyer and Chris Madsen

# Description: 
# This script takes a collection of .csv data files (1 per state), accessed from
# the NPPES dataset, and summarises health care providers by the groupings of
# 'Behavioral Health & Social Service Providers' and 'Allopathic & Osteopathic Physicians'.
# Providers are further summarised by classification. 
# These summaries are performed at the scale of Hospital Service Areas,
# accessed from the Dartmouth Atlas of Health Care (https://www.dartmouthatlas.org/),
# which are assigned to NPPES data via the linkage of ZIP code.

# Most recent edits: 2023-12-03 (YMD)

# ==== Libraries ====

library(readr)
library(tigris)
library(archive)
library(tidyverse)
library(sf)
library(zipcodeR)

remake_base_layers = F


## cities = tigris::urban_areas()

# ==== Functions ====
source('scripts/data_cleaning_functions.R')

# ==== Paths and global variables ====
state_dat_filepaths = list.files(path ='data/', pattern = 'state_[A-Z]+.*csv',
                                 full.names = T)

# Path to NUCC taxonomy csv
nucc_path = 'data/nucc_taxonomy_230.csv'

# Path to BCBS file
bcbs_path = 'data/bcbs/bcbs_providers.csv'

# ==== Read in files =====

# Read in NUCC taxonomy csv
taxonomies <- read_csv(nucc_path)

# Read in Blue Cross lookup table
bcbs = read_csv(bcbs_path)
# Read in additional state-specific BCBS tables.
bcbs_al = readxl::read_excel('data/bcbs/BCBS-AL - Provider Data_03162023_DOL(dhooven@bcbsal.org).xlsx') |> 
  dplyr::summarise(npi = PROV_NPI,
                state = 'AL')

bcbs = bcbs |> 
  dplyr::select(npi = `PRACTITIONER NPI`, state = STATE) |> 
  bind_rows(bcbs_al) |> 
  distinct()

# Get list of unique IDs that are in BCBS.
bcbs_npi_list <- unique(bcbs$npi)

# write_csv(data.frame(NPI_practicioner_number = npi_test_list), 'output/npi_test_list.csv')

# # Bring in the crosswalk table for zipcode -> HSA/HRR
# crosswalk = readr::read_csv(crosswalk_path) |>
#   dplyr::rename(shortzip = zipcode19)

# Shall we remake the 'base-layer' shapefiles, e.g. counties and states?
if(remake_base_layers){
# Bring in state shapefiles from {tigris}; find centroids for 
# shiny app.
zipcodeR::download_zip_data()

# Counties
counties = tigris::counties(cb = TRUE)

# Some counties are split into multiple contiguous shapes; need to bind those together
counties = counties |> 
  group_by(NAMELSAD, STUSPS, STATE_NAME) |> 
  dplyr::summarise()

all_states = tigris::states(cb = T) |> 
  dplyr::filter(!NAME %in% c('American Samoa',
                             'Guam',
                             'United States Virgin Islands',
                             'Puerto Rico',
                             'Commonwealth of the Northern Mariana Islands'))

# Calculate the bounding box of each of the states; use this 
# to inform the zoom level for the shiny app leaflet map.
all_states = all_states |> 
  rowwise() |> 
  mutate(bbox_area = as.numeric(st_area(st_as_sfc(st_bbox(geometry))))) |> 
  mutate(bbox_area_bin = case_when(
    bbox_area <= 1e+11 ~ 'small',
    bbox_area > 1e+11 & bbox_area <= 5e+11 ~ 'medium',
    bbox_area > 5e+11 ~ 'large',
    T ~ 'woops!'
  )) |> 
  mutate(zoom_factor = case_when(
    bbox_area_bin == 'small' ~ 7,
    bbox_area_bin == 'medium' ~ 6,
    NAME == 'Florida' ~ 6,
    bbox_area_bin == 'large' ~ 5,
  )) |> 
  rmapshaper::ms_simplify()

state_centroids = sf::st_centroid(all_states) |> 
  dplyr::select(state_name = NAME, zoom_factor) |> 
  sf::st_transform(4326)

# Bring in the HSA (and HRR, for now?) spatial files.
# hsa = sf::read_sf(hsa_path)

# Simplify.
# hsa_s = rmapshaper::ms_simplify(hsa)
# sf::write_sf(counties, 'data/counties_boundaries_nonsimplified.gpkg')
# Too big if unsimplified!

counties_s = rmapshaper::ms_simplify(counties) 

write_sf(counties_s, 'data/counties_boundaries.gpkg')

counties = read_sf('data/counties_boundaries.gpkg') |> 
  st_transform(crs = 4326) |> 
  dplyr::rename(county = NAMELSAD,
                state = STATE_NAME)

# write_sf(hsa_s, 'data/hsa_boundaries.gpkg')

# hsa = read_sf('data/hsa_boundaries.gpkg') |> 
#   st_make_valid(geom) |> 
#   st_transform(crs = 4269) |> 
#   dplyr::rename(hsanum = HSA93) |>
#   dplyr::mutate(hsalabel = stringr::str_squish(HSA_label)) |> 
#   dplyr::select(-HSA_label) |> 
#   mutate(state_abbr = stringr::str_extract(HSANAME,'^[A-Z]{2}'))

# Calculate population density at the chosen scale.
population_density = zipcodeR::zip_code_db |> 
  as_tibble() |> 
  left_join(
    dplyr::tibble(
      state_full = state.name,
      state = state.abb
    )
  ) |> 
  # mutate(county = stringr::str_remove_all(county, ' [A-Za-z]*$')) |> 
  dplyr::select(-state) |> 
  dplyr::rename(state = state_full) |> 
  left_join(counties |> 
              dplyr::select(county,
                            state)) |> 
  # left_join(crosswalk |> 
  #             dplyr::select(zipcode = shortzip, 
  #                           hsanum)) |> 
  dplyr::select(county, state, population, land_area_in_sqmi) |> 
  group_by(county, state) |> 
  reframe(across(everything(), \(x) sum(x, na.rm=T))) |> 
  # group_by(county, state) |>
  # reframe()
  mutate(population_density = population / land_area_in_sqmi) |> 
  filter(county != "", !is.na(state)) |> 
  dplyr::filter(population != 0)
}

# Summarise data to select administrative boundary level (e.g. county). This takes a bit to run!
# summary_results = summarise_providers_to_chosen_scale(state_dat_filepaths)
summarise_providers_to_chosen_scale(state_dat_filepaths)

# Reallocate NPI records based on zipcode-inferred state,
# rather than the NPI data column 'Provider.State...'
summary_results = clean_up_dat_summarised_to_zip()

# Drop states outside contiguous USA + Alaska + Hawaii
summary_results = summary_results |> 
  filter(state %in% c(state.abb,'DC'))

# Replace the 2-letter abbreviation of state names with full state names.
summary_results = summary_results |> 
  left_join(
    tibble(
      state = c(state.abb,'DC'),
      state_long = c(state.name,'District of Columbia'),
    )
  ) |> 
  dplyr::rename(state_abbr = state,
                state = state_long) |> 
  dplyr::select(-state_abbr) 

npi_test_list = summary_results[c(1,5,10,15,20,25:30,35:38),]

# Write out the population density excel file.
openxlsx::write.xlsx(population_density, 'draft_app/www/population_density.xlsx')

# Write out the cleaned data to our www/ folder for the app.
write_csv(summary_results, 'draft_app/www/npidata_summarized_zip2.csv')

# Write out cleaned data to our www/ folder, drop 2 columns.
#write.csv(npi_data_summarized_to_zip |> dplyr::select(-shortzip,-city_from_zipcode), 'draft_app/www/npidata_summarized_national_lite.csv', row.names = F)

# Write out the HSA spatial file, which now includes county and state info.
# sf::write_sf(st_transform(hsa,4326), 'draft_app/www/hsa_boundaries.gpkg')

# Write out counties in the proper CRS transformation.
sf::write_sf(counties, 'draft_app/www/counties.gpkg')

# Write out shapefile for state centroids.
sf::write_sf(state_centroids, 'draft_app/www/state_centroids.gpkg')

# Write out state shapefile.
sf::write_sf(all_states |> 
               dplyr::select(state_name = NAME) |>
               sf::st_transform(4326),
             'draft_app/www/state_boundaries.gpkg')

# Write out the simplified counties shapefile.
# sf::write_sf(st_transform(counties,4326), 'draft_app/www/counties_simple.gpkg')
