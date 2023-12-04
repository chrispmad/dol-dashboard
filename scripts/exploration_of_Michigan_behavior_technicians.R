# The following functions allow us to quickly assess and summarise 
# odd records from Farmington Hills, Michigan. There are a suspicious number
# of behavior technicians.
library(zipcodeR)
#i = 22
all_dat = read_csv('draft_app/www/npidata_summarized_zip.csv')


## For Grouping of MH/SUD, calculate proportion of healthcare providers
## that are classified as 'Behavior Technician'. Attach which city has the most.
behav_techs_analysis = all_dat |> 
  filter(Grouping == 'Behavioral Health And Social Service Providers') |> 
  group_by(state, Classification) |> 
  summarise(n_providers = sum(n)) |> 
  mutate(just_behavior_techs = sum(n_providers[Classification == 'Behavior Technician']), all_providers = sum(n_providers)) |> 
  reframe(just_behavior_techs, all_providers) |> 
  distinct() |> 
  mutate(proportion = just_behavior_techs/all_providers) |> 
  arrange(desc(proportion)) |> 
  dplyr::left_join(
    # Zip code with highest number of behavior technicians by state
    all_dat |> filter(Grouping == 'Behavioral Health And Social Service Providers',
                      Classification == 'Behavior Technician') |> 
      group_by(state, shortzip) |> 
      summarise(behav_techs_in_city = sum(n)) |> 
      arrange(state, desc(behav_techs_in_city)) |> 
      slice(1) |> 
      left_join(
        zip_code_db |> dplyr::select(shortzip = zipcode, major_city)
      )
  ) |> 
  dplyr::select(just_behav_techs_by_state = just_behavior_techs,
                all_providers_by_state = all_providers,
                prop_of_all_provs_are_behav_techs = proportion,
                zip_with_most_behav_techs_in_state = shortzip,
                city_matching_zipcode = major_city,
                behav_techs_in_city)

write_csv(behav_techs_analysis,'data/behavior_techs_by_state_analysis.csv')

all_dat_just_bt = all_dat |> 
  filter(Classification == '')
dat = read_csv('data/michigan_behavior_techs.csv')
dat_unsum = read_csv('data/michigan_behavior_techs_unsummarized.csv')

# First, count by classification.
dat_unsum |> 
  dplyr::count(Classification, sort = T)

# Second, count by Classification and city name.
dat_unsum |> 
  dplyr::count(Classification, `Provider Business Practice Location Address City Name`, sort = T)
# Farmington hills!!

# Third, for Farmington Hills, count by Classification.
dat_unsum |> 
  filter(`Provider Business Practice Location Address City Name` == 'FARMINGTON HILLS') |> 
  dplyr::count(Classification, sort = T)
# So many behavior technicians.

dat_unsum |> 
  rename(phone_number = `Provider Business Practice Location Address Telephone Number`) |> 
  filter(`Provider Business Practice Location Address City Name` == 'FARMINGTON HILLS') |> 
  dplyr::count(Classification, phone_number, sort = T)

# Look specifically at that phone number.
dat_unsum |> 
  filter(`Provider Business Practice Location Address Telephone Number` ==  2484364400) |> 
  View()
