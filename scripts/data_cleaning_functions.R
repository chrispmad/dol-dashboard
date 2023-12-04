# This script lays out the functions we use to clean and 
# analyse the NPPES dataset for the U.S. Department of Labor 
# and the app we have created for them.

# Script author: Chris Madsen
# Most recent edits: 2023-10-24 (YMD)

# ====================================

# 1. Split NPPES data by state.
split_nppes_by_state = function(force_update = F){
  
  # Set up dataframe with state names + abbrevs.
  state_names = data.frame(
    state_name = state.name,
    name_caps = stringr::str_to_upper(state.name),
    abbr = state.abb)
  
  for(i in 1:nrow(state_names)){
    # For each state, do the following:
    
    # Check to see if we have the state-specific .csv dataset 
    # already. If we do, don't repartition the data (unless force_update == T).
    if(!file.exists(paste0('data/state_',state_names[i,'state_name'],'.csv')) | force_update){
      
      # i. Tell us which state the code is currently processing.
      print(paste0("Working on ",state_names[i,'state_name'],", ",i," of 50"))
      
      # ii. Create filter based on full name and abbreviation.
      state_filter = function(x, pos){
        subset(x, `Provider Business Practice Location Address State Name` %in% unname(state_names[i,]))
      }
      
      # iii. Apply filter.
      state_dat = readr::read_csv_chunked(
        'data/npidata_pfile_20050523-20230813.csv',
        DataFrameCallback$new(
          state_filter), 
        chunk_size = 1000,
        progress = TRUE,
        show_col_types = FALSE
      )
      
      # iv. Write out results to state-specific .csv file.
      write.csv(state_dat, paste0('data/state_',state_names[i,'state_name'],'.csv'),row.names = F)
    }
  }
}

# 2. I (Chris) hand-corrected mismatches in county names. This function 
#    therefore may need attention in the future or with other datasets...
# 1. Zipcode county corrector

correct_zipcode_counties = function(dat){
  dat |> 
    mutate(county = stringr::str_to_title(county)) |> 
    mutate(county = case_when(
      county %in% c('Doa Ana County','Dona Ana County') ~ 'Doña Ana County',
      county == 'Dupage County' ~ 'DuPage County',
      county == 'La Porte County' ~ 'LaPorte County',
      county == 'Lagrange County' ~ 'LaGrange County',
      county == 'De Witt County' ~ 'DeWitt County',
      county == 'Mclennan County' ~ 'McLennan County',
      county == 'La Salle County' ~ 'LaSalle County',
      county == 'Mchenry County' ~ 'McHenry County',
      county == 'Mclean County' ~ 'McLean County',
      county == 'Mcminn County' ~ 'McMinn County',
      county == 'Mcdowell County' ~ 'McDowell County',
      county == 'Mckinley County' ~ 'McKinley County',
      county == 'Municipality of Anchorage' ~ 'Anchorage Municipality',
      county == 'City and Borough of Juneau' ~ 'Juneau City and Borough',
      county == 'City And Borough Of Juneau' ~ 'Juneau City and Borough',
      county == 'St Clair County' ~ 'St. Clair County',
      county == 'Desoto County' ~ 'DeSoto County',
      county == 'Carolina' ~ 'Carolina Municipio',
      county == 'Mccracken County' ~ 'McCracken County',
      county == 'Coös County' ~ 'Coos County',
      county == 'Rincn Municipio' ~ 'Rincón Municipio',
      county == 'Ro Grande Municipio' ~ 'Río Grande Municipio',
      county == 'San Juan' ~ 'San Juan Municipio',
      county == 'Toa Alta' ~ 'Toa Alta Municipio',
      county %in% c('Catao Municipio','Cataño') ~ 'Cataño Municipio',
      stringr::str_detect(county,'^St ') ~ stringr::str_replace_all(county,'St ','St. '),
      county %in% c('Doa Ana County','Dona Ana County') ~ 'Doña Ana County',
      T ~ county
    ))
}

# 3. This function downloads spatial files for US counties, 
#    if they are not present on the local machine already.
download_counties = function(force_update = F){
  # ==== Download Spatial Files =====
  if(!file.exists('draft_app/www/counties_national.gpkg') | force_update){
    counties = tigris::counties(
      #The 'cb' argument, when true, codes for simplified spatial files. This is good!
      cb = TRUE)
    
    # Further simplify the counties shapefile.
    counties_simple = rmapshaper::ms_simplify(counties)
    
    # Write out the counties as a shapefile.
    sf::write_sf(counties |> sf::st_transform(crs = 4326), 'draft_app/www/counties_national.gpkg')
  }
}

summarise_providers_to_chosen_scale = function(state_data_paths){
  
  # for(i in 1:length(state_data_paths)){
  for(i in c(1:50)){
    
    state_data_file = state_data_paths[i]
    
    state_of_interest = stringr::str_extract(state_data_file,'[ A-Za-z]+(?=\\.csv)')
    state_of_interest_abbr = state.abb[state.name == state_of_interest]
    
    print(paste0("Working on ",state_of_interest))
    
    # Seek list of BCBS network providers, if we have it.
    state_bcbs_list = bcbs[bcbs$state == state_of_interest_abbr,]
    
    if(nrow(state_bcbs_list) == 0){
      bcbs_present = FALSE
    } else {
      bcbs_present = TRUE
    }
    
    dat = vroom::vroom(state_data_file,
                          col_types = cols(.default = "c")) |> 
      mutate(shortzip = substring(`Provider Business Mailing Address Postal Code`,1,5))
    
    dat = dat[,-c(5:6,12:14,21:29,44:47,69:108,129:308,310:330)]
    
    # Just keep 2 Groupings' records.
    dat = dat %>% 
      rename('Code' = `Healthcare Provider Taxonomy Code_1`) |> 
      left_join(taxonomies, by = join_by(Code)) |>
      filter(Grouping %in% c('Behavioral Health & Social Service Providers',
                             'Allopathic & Osteopathic Physicians'))
    
    # Add a column indicating whether or not a given provider 
    # is in the Blue Cross network.
    if(bcbs_present){
      dat = dat |>
        mutate(in_bcbs_network = NPI %in% bcbs_npi_list)
      # TEMPORARY ADDITION # 
      # Make this TRUE/FALSE column RANDOM, just for showing the dashboard purposes.
      # Once we get the real data from the US government, add it here.
    } else {
      dat = dat |> 
        mutate(in_bcbs_network = sample(c(0,1), size = nrow(dat), replace = TRUE))
    }
    # Maybe not necessary? Some formatting of grouping labels.
    dat = dat %>% mutate(Grouping = str_replace_all(Grouping, '&', 'And'),
                         Classification = str_replace_all(Classification, '&', 'And'))
    
    # ==== Split Psychiatry and Neurology ====
    # Note: There were some entries whose classification did not contain
    # either key prefix (neither 'Psych' nor 'Neur'). These turned out to be 
    # Sleep Medicine, Pain Medicine, and Epilepsy. I have classified these as Neurology,
    # but could be wrong!
    dat = dat |> 
      mutate(Classification = case_when(
        Classification == 'Psychiatry And Neurology' & str_detect(Specialization, 'Psych') ~ 'Psychiatry',
        Classification == 'Psychiatry And Neurology' & str_detect(Specialization, 'Neur') ~ 'Neurology',
        Classification == 'Psychiatry And Neurology' & str_detect(Specialization, '(Pain Medicine|Sleep Medicine|Epilepsy)') ~ "Neurology",
        T ~ Classification
      ))
    
    ## ADD IN SECTION TO CHANGE GROUPING OF PSYCHIATRY CLASSIFICATION RECORDS FROM GROUP 1 TO GROUP 2.
    dat = dat |> 
      mutate(Grouping = case_when(
        Classification == 'Psychiatry' ~ "Behavioral Health And Social Service Providers",
        T ~ Grouping
      ))
    
    # Summarize to zip code, add in number in BCBS network.
    dat = dat |> 
      mutate(shortzip = as.character(shortzip)) |>
      # Summarize the data by zip code, provider type, entity code.
      group_by(shortzip, Grouping, Classification, `Entity Type Code`) |> 
      count() |> 
      # Add in a summary that shows how many are in the Blue Cross network.
      left_join(
        dat |>
          mutate(shortzip = as.character(shortzip)) |>
          group_by(shortzip, Grouping, Classification, `Entity Type Code`, in_bcbs_network) |>
          count(name = 'number_in_bcbs_network') |>
          filter(in_bcbs_network == T),
        by = join_by(shortzip, Grouping, Classification, `Entity Type Code`)
      ) |>
      mutate(number_in_bcbs_network = replace_na(number_in_bcbs_network, 0)) |> 
      dplyr::select(-in_bcbs_network) |>
      ungroup()
    
    if(state_of_interest == 'Michigan'){
      write_csv(dat, 'data/michigan_behavior_techs.csv')
      write_csv(dat_b, 'data/michigan_behavior_techs_unsummarized.csv')
    }
    # dat = dat |> 
    #   left_join(crosswalk) |> 
    #   # Should we be worried we are losing a few rows here (e.g. 9 of 5033 for Alabama), 
    #   # as they don't match any HSA zip code? Maybe unavoidable.
    #   filter(!is.na(hsanum)) |> 
    #   group_by(hsanum,hsacity,Grouping,Classification,Entity.Type.Code) |> 
    #   summarise(n = sum(n),
    #             number_in_bcbs_network = sum(number_in_bcbs_network))
    dat = dat |> 
      left_join(
        # sf::st_drop_geometry(counties) |> 
        #   dplyr::select(state, county, state_abbr = STUSPS)
        zip_code_db |> 
          dplyr::select(shortzip = zipcode, county, state)
      )
    
    write_csv(dat, paste0('data/summarised_to_zip/labelled_as_',state_of_interest,'.csv'))
    
  }
}

clean_up_dat_summarised_to_zip = function(){
  match_report = dplyr::tibble(
    state = state.name,
    pre_match_rows = 0,
    post_match_rows = 0,
    matched_states = 'a'
  )
  
  dat = list.files('data/summarised_to_zip/', pattern = '.csv', full.names = T) |> 
    lapply(\(x) read_csv(x)) |> 
    dplyr::bind_rows()
  
  # Remove Behavior Techs of Farmington Hills, Michigan, from data.
  dat = dat |> 
    dplyr::filter(!(shortzip == 48334 & Classification == 'Behavior Technician'))
  
  # If we trust the zipcode over the 'Provider.State...' column that was included
  # in the original NPI dataset, we can use the 'state' column
  # from the zipcode table match as the locator.
  data_loss_results = dat |> 
    mutate(no_zip_match = is.na(state)) |> 
    count(state, no_zip_match, name = 'count') |> 
    mutate(data_percent = count / sum(count))
  
  write.csv(data_loss_results, 
            'data/zip_state_match_report.csv')
  
  return(dat)
  # results = lapply(\(x) {
  #     
  #     state_of_interest = stringr::str_extract(x,'(?<=_)[A-Za-z]+(?=\\.csv)')
  #     
  #     dat = readr::read_csv(paste0('data/summarised_to_zip/',x))
  #     
  #     cat(paste0("\n",nrow(dat)," rows before dropping records that didn't match to state."))
  #     
  #     match_report[match_report$state == x,]$pre_match_rows = nrow(dat)
  #     
  #     dat_state_match = dat |> 
  #       count(state, sort = T) |> 
  #       mutate(state_number = paste0(state, ' (', n, ')'))
  #     
  #     match_report[i,]$matched_states = paste0(dat_state_match$state_number, collapse = ", ")
  #     
  #     dat = dat |> 
  #       filter(!is.na(county),
  #              state == state_of_interest_abbr)
  #     
  #   cat(paste0("\n",nrow(dat)," rows after dropping records that didn't match to state."))
  #   
  #   match_report[i,]$post_match_rows = nrow(dat)
  #   
  #   dat = dat |> 
  #     group_by(county,state,Grouping,Classification,`Entity Type Code`) |> 
  #     summarise(n = sum(n),
  #               number_in_bcbs_network = sum(number_in_bcbs_network))
  #   
  #   # Add in the state, for later reference. Maybe not necessary.
  #   # dat = dat |> mutate(state = state_of_interest)
  #   
  #   if(i == 1){
  #     results = dat
  #   } else {
  #     results = dplyr::bind_rows(results, dat)
  #   }
  #   }) |> 
  #     bind_rows()
  # 
  # readr::write_csv(match_report, 'data/zip_state_match_report.csv')
  # 
  # return(results)
}
