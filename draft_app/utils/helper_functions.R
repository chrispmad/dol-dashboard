# ===================================================
# Clean names of Group 2 classification and 
# pull them out as a vector.
get_clean_values = function(dat){
  dat |> 
    filter(!is.na(`MH/SUD Providers`)) |> 
    dplyr::select(Classification = `MH/SUD Providers`) |> 
    mutate(Classification = str_replace_all(Classification, ',', '<br>')) |>
    mutate(Classification = str_replace_all(Classification, '\\(', '<br>(')) |> 
    mutate(Classification = str_replace_all(Classification, 'Medicine ', 'Medicine<br>')) |> 
    dplyr::pull(Classification)
}


# ===================================================

# DOL pickerInput with various preset options
dol_pickerInput = function(inputId, choices){
  pickerInput(
    inputId = inputId,
    label = '',
    multiple = TRUE,
    choices = choices,
    # For names of options, we use some HTML to define line breaks;
    # here we render each name as HTML.
    selected = lapply(
      choices,
      htmltools::HTML),
    # List of options for pickerInput.
    options = list(
      `actions-box` = T, 
      size = 10, 
      `selected-text-format` = "count > 3"
    ),
    choicesOpt = list(
      content = choices
    )
  )
}

# =========================================================

# Join the selected healthcare provider data table 
# to the ZCTA spatial object.
join_healthcare_data_to_shapes = function(dat, 
                                        group_dat_interm,
                                        group_dat_choice){
  
  dat |>
    left_join(
      group_dat_interm |>
        # filter(Entity.Type.Code %in% as.numeric(ent_type)) |>
        filter(Classification %in% group_dat_choice) |>
        dplyr::select(state, shape_name, n, number_in_bcbs_network) |>
        # Summarize Group 1 data to the zip code
        group_by(state, shape_name) |>
        summarise(across(everything(), sum))
    )
}

calculate_population_based_metrics = function(dat,
                                        chosen_variable){
  dat |> 
    # Calculate metrics that compare the number of providers
    # to aspects of the population (density).
    mutate(providers_per_population = n / population) |>
    mutate(people_per_provider = round(1 / providers_per_population,0)) |>
    mutate(providers_per_pop_density_sq_mile = n / population_density) |>
    mutate(prop_in_bcbs = number_in_bcbs_network / n) |>
    mutate(prop_in_bcbs = replace_na(prop_in_bcbs, 0)) |>
    dplyr::rename(var_chosen = !!sym(chosen_variable)) |>
    dplyr::select(state, shape_name, var_chosen)
}

# =========================================================

calc_summary_value = function(state_chosen,
                              county_chosen,
                              dat,
                              var_to_map,
                              var_chosen){

  if(state_chosen != 'Contiguous USA'){
    dat = dat |> 
      dplyr::filter(state == state_chosen)
  }
  if(!is.null(county_chosen)){
    if(county_chosen != 'All'){
      dat = dat |> 
        dplyr::filter(shape_name == county_chosen)
    }
  }
  if(var_to_map == 'n'){
    # Calculate total N for group 1.
    dat |> 
      sf::st_drop_geometry() |> 
      dplyr::summarise(total_n = sum(var_chosen,na.rm=T)) |> 
      dplyr::pull(total_n)
    
  } else {
    # Calculate average of variable chosen for all ZCTAs.
    round(
      dat |> 
        sf::st_drop_geometry() |> 
        dplyr::summarise(average_for_var = mean(var_chosen,na.rm=T)) |> 
        dplyr::pull(average_for_var),
      5)
  }
}

# Add Text portion to summary value.
add_text_to_summary = function(var_to_map, summary_number){
  if(is.character(summary_number)){
    switch(var_to_map,
           n = paste0("Total: ",summary_number),
           people_per_provider = paste0("Total: ",summary_number),
           providers_per_pop_density_sq_mile = paste0("Average: ",summary_number),
           prop_in_bcbs = paste0("Average: ",summary_number))
  } else {
  switch(var_to_map,
         n = paste0("Total: ",summary_number),
         people_per_provider = paste0("Total: ",round(summary_number,0)),
         providers_per_pop_density_sq_mile = paste0("Average: ",summary_number),
         prop_in_bcbs = paste0("Average: ",round(100*summary_number,2),"%"))
  }
}
# =========================================================

# Conditional selection of which ZCTA variable choropleth
# to map.
select_shape_to_map = function(g_to_map,shape_1,shape_2,class_2_choice){
  if(g_to_map == 'group1'){
    req(shape_1)
    return(shape_1)
  }
  if(g_to_map == 'group2'){
    return(shape_2)
  }
  if(g_to_map == 'ratio'){
    req(class_2_choice != 'None')
    shape_1 |> 
      left_join(
        st_drop_geometry(
          shape_2 |> dplyr::rename(var_2 = var_chosen))
      ) |> 
      dplyr::rename(var_1 = var_chosen) |> 
      mutate(var_chosen = round(var_1/var_2,2)) |> 
      mutate(var_chosen = ifelse(is.infinite(var_chosen) | var_chosen == 0,NaN,var_chosen))
  } 
  else {
    return(
      st_set_geometry(
        tibble(shape_name = 0, var_chosen = 0),
        st_as_sfc(
          st_bbox(
            st_as_sf(
              tibble(
                lat = c(0,1),
                lng = c(0,1)),
              coords = c('lng','lat'),
              crs = 4326
            )
          )
        )
      )
    )
  }
}
# =========================================================

# Calculate leaflet colour palette (either for map or for legend)
calc_leaflet_col_palette = function(legend, g_to_map, shape_to_map, 
                                    # Test - feed in both zcta groups.
                                    min_value, 
                                    max_value,
                                    var_to_map){
  
  if(!legend){
    # Map colour palette...
    if(g_to_map == 'ratio'){
      min_ratio = min(shape_to_map$var_chosen,na.rm=T)
      max_ratio = max(shape_to_map$var_chosen,na.rm=T)
      
      # my_bins <- c(0,0.25,0.5,0.75,1.25,1.5,100)
      my_bins <- c(0,1,1.5,2,2.5,100)
      color_val = colorRampPalette(colors = c("darkgreen","#09B91F","orange",'red','darkred'),
                                   space = "Lab")(length(my_bins))
      
      colorBin(color_val, bins = my_bins)
    } else {
      colorNumeric(
        palette = 'viridis',
        # domain = zcta_to_map$var_chosen,
        # Test - set domain to be all of group 1 and 2.
        domain = c(min_value,max_value),
        reverse = (var_to_map %in% 'people_per_provider')
      )
    }
  } else {
    # Legend colour palette...
    if(g_to_map == 'ratio'){
      # my_bins = c(0,0.25,0.5,0.75,1.25,1.5,100)
      my_bins = c(0,1,1.5,2,2.5,100)
      # color_val = colorRampPalette(colors = c("darkred",'orange','#8DF204',"#09B91F",'#8DF204',"orange",'darkred'),
      color_val = colorRampPalette(colors = c("darkgreen","#09B91F","orange",'red','darkred'),
                                   space = "Lab")(length(my_bins))
      colorBin(color_val, bins = my_bins,
               na.color = NA)
    } else {
      colorNumeric(
        reverse = (!var_to_map %in% 'people_per_provider'),
        palette = 'viridis',
        # domain = zcta_to_map$var_chosen,
        domain = c(min_value,max_value),
        na.color = NA
      )
    }
  }
}

# =========================================================

# Put together the label for each ZCTA in the choropleth.
# These are visible when the mouse hovers over a given
# ZCTA.
calc_var_label = function(g_to_map,var_to_map,shape_to_map){
  
  if(g_to_map == 'ratio'){
    lapply(
      paste0("",shape_to_map$shape_name,": ",shape_to_map$var_chosen,":1<br>(",round(shape_to_map$var_1,3),":",round(shape_to_map$var_2,3),")"),
      htmltools::HTML)
  } else {
    if(var_to_map == 'prop_in_bcbs'){
      paste0("",shape_to_map$shape_name,": ",paste0(round(100*shape_to_map$var_chosen,2),"%"))
    } else {
      paste0("",shape_to_map$shape_name,": ",round(shape_to_map$var_chosen,4))
    }
  }
}

# =========================================================
# Put together the legend label.

render_legend_label = function(g_to_map, var_to_map){
  main_label = switch(var_to_map,
                      n = 'Number of <br>Providers',
                      people_per_provider = 'People per <br>Provider',
                      providers_per_pop_density_sq_mile = 'Providers by <br>population <br>density',
                      prop_in_bcbs = 'Percent Providers in <br>Blue Cross<br>Blue Shield<br>Network')
  if(g_to_map == 'ratio'){
    main_label = paste0('Ratio of <br>',main_label)
  }
  main_label
}

# =========================================================
# Initialize leaflet map for DOL dashboard with presets.

initialize_dol_leaflet = function(state_centroids, state_boundaries, shape_outlines, 
                                  min_universal_num, max_universal_num, my_pal_legend,
                                  legend_label, group_to_map,
                                  variable_to_map, shape_to_map,
                                  variable_label, my_pal, set_view = NULL,
                                  is_interactive = T,
                                  state_chosen = NULL){
  m = leaflet() %>%
    addTiles(group = "Street Maps") %>%
    addProviderTiles("CartoDB", provider = providers$CartoDB) %>%
    addPolygons(
      group = 'pubs',
      label = ~state_name,
      col = 'black',
      weight = 2,
      data = state_boundaries
    ) |> 
    addLabelOnlyMarkers(
      group = 'state_centroids',
      label = ~state_name,
      data = state_centroids,
    ) |> 
    setView(lng = -98.788, lat = 39.039, zoom = 4) |> 
    addScaleBar('bottomright') |>
    addLayersControl(baseGroups = c("CartoDB","Street Maps"),
                     options = layersControlOptions(collapsed = F)) |> 
    # leaflet.extras::addSearchFeatures(targetGroups = 'state_centroids',
    #                                   options = leaflet.extras::searchFeaturesOptions(
    #                                     zoom = 5,
    #                                     firstTipSubmit = T,
    #                                     collapsed = F
    #                                   )) |> 
  clearGroup('mapped_polys') |> 
    clearControls() |> 
    addPolygons(
      group = 'mapped_polys',
      layerId = ~paste0(state,"_",shape_name),
      color = 'black',
      weight = 1,
      fillColor = ~my_pal()(var_chosen),
      fillOpacity = ifelse(group_to_map == 'ratio', 0.6, 0.3),
      opacity = 0.2,
      label = ~ variable_label(),
      data = shape_to_map()
    )
  if(is_interactive){
    m = m |> 
      addLegend(
        pal = my_pal_legend(),
        title = legend_label(),
        na.label = '',
        labFormat = myLabelFormat(group_to_map,
                                  variable_to_map), # Our custom labeller.
        values = c(min_universal_num(),
                   max_universal_num())
      )
  } else {
    m = m |> 
      addLegend(
      pal = my_pal_legend(),
      title = legend_label(),
      na.label = '',
      labFormat = myLabelFormat('ratio',
                                variable_to_map), # Our custom labeller.
      values = c(min_universal_num(),
                 max_universal_num())
    )
  }
  
  if(is_interactive) {
    m = m |> 
      addMiniMap(zoomLevelFixed = 1,
                 position = 'bottomleft',
                 height = 100,
                 toggleDisplay = T) |> 
      leaflet.extras::addResetMapButton()
  } else {
      if(state_chosen != 'Contiguous USA'){
        m = m |> 
          clearGroup('highlighted') |> 
          addPolylines(
            group = 'highlighted',
            color = 'gold',
            stroke = 2,
            fillColor = 'transparent',
            data = state_boundaries |> 
              filter(state_name == state_chosen) |> 
              sf::st_cast("MULTILINESTRING")
          )
      }
  }
  m
}
# =========================================================
# Custom function for formatting ratio labels on leaflet map.
formatRatioLabels = function(x) {
  bin_labels = paste0(x, ' - ',x[-1])[-length(x)]
  bin_labels[length(bin_labels)] = str_replace(bin_labels[length(bin_labels)],' - .*','+')
  bin_labels
}

# Custom labelling function for leaflet map.
# Using this function allows us to call the ratio 
# labelling function we defined just above.
myLabelFormat = function(g_to_map,
                         var_to_map,
                         ...){ 
  
  # Are we looking at ratios? If so, replace final bin upper bound with '+'
  if(g_to_map == 'ratio'){ 
    function(type = "numeric", cuts){ 
      paste0(formatRatioLabels(cuts))
    } 
  }else{
    labelFormat(transform = function(x){
      # Adjust numbers in legend so that percentages are, e.g., 90(%), instead of 0.90
      ifelse(var_to_map == 'prop_in_bcbs' & g_to_map != 'ratio',100,1)*sort(round(x,2), decreasing = T)
    })
  }
}

# =========================================================
# 

add_minimap_with_shapes = function(map, shapes){
  map |> 
  htmlwidgets::onRender(paste0("
    function(el, t) {
      var myMap = this;

      var ",shapes," = myMap.layerManager._byGroup.",shapes,";
      var ",shapes,"2 = new L.FeatureGroup();

      for(shape in ",shapes,") {
        var m = new L.polygon(",shapes,"[shape]._latlngs, 
         {
        color: 'black'
        });
        m.options.fillOpacity = 0.2;
        m.options.weight = 1;
        ",shapes,"2.addLayer(m);
      }
      
      //console.log(myMap.minimap);

      var layers = new L.LayerGroup([myMap.minimap._layer, ",shapes,"2]);
      //var layers_switch_2 = new L.LayerGroup([myMap.minimap._layer, myMap.minimap._layers[4]]);

      myMap.minimap.changeLayer(layers);
      //myMap.minimap.changeLayer(layers_switch_2);
      
    }"))
}