# Author: Katja and Chris
# Date: 2023-10-24 (YMD)

# Description: 

# ==== Libraries ====

library(readr)
library(archive)
library(tidyverse)

# ==== Filtering data ====

# Now that we've seen a preview of the data, we can craft a function to filter
# the data for our needs. This function will skim off only records that match
# some condition we set. 

# By Provider Business Location ----------

state_names = data.frame(state_name = state.name,
                         name_caps = stringr::str_to_upper(state.name),
                         abbr = state.abb)

for(i in 1:50){
  # For rows 1 to 50, each of which is one of the US states, 
  # do the following:
    
  print(paste0("Working on ",state_names[i,'state_name'],", ",i," of 50"))
  
  # Filter data to only include rows within the state selected (full name or abbreviation)
  state_filter = function(x, pos){
    subset(x, `Provider Business Practice Location Address State Name` %in% unname(state_names[i,]))
  }
  
  state_dat = readr::read_csv_chunked(
    'data/npidata_pfile_20050523-20230813.csv',
    DataFrameCallback$new(
      state_filter), 
    chunk_size = 1000,
    progress = TRUE,
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )
  
  write.csv(state_dat, paste0('data/state_',state_names[i,'state_name'],'.csv'),row.names = F)
  
}
# The above code took about six hours on my machine!


