
# Include the separate R scripts that:
# 1. Define the user interface
source('utils/UI.R')
# 2. Define helper functions that improve readability of this script.
source('utils/helper_functions.R')

# The following code block defines the server 'back-end' of our app.
server <- function(input, output, session) {
  
  # Read a separate R script to set options for app and 
  # read in data.

  if(!stringr::str_detect(getwd(),'/www$')) setwd(paste0(getwd(),'/www'))

  # ===== Set Options ========#
  # We fix the identity of group two:
  group_two = 'Behavioral Health And Social Service Providers'

  # ===== Read in data  ===== #
  # 1. NPI data summary
  # npidata_sum = vroom::vroom('npidata_summarized_county.csv') |>
  #   dplyr::rename(shape_name = county,
  #                 Entity.Type.Code = `Entity Type Code`)
  npidata_zip = vroom::vroom('npidata_summarized_zip.csv')
  
  npidata_sum = npidata_zip |> 
    dplyr::rename(shape_name = county) |> 
    dplyr::group_by(state,shape_name,Grouping,Classification) |> 
    dplyr::summarise(n = sum(n,na.rm=T),
              number_in_bcbs_network = sum(number_in_bcbs_network,na.rm=T)) |> 
    dplyr::ungroup()

  # HSA spatial file
  # hsa = sf::read_sf('hsa_boundaries.gpkg')
  
  # Counties spatial file
  counties = sf::read_sf('counties.gpkg')

  # Population density excel file
  population_density = readxl::read_excel('population_density.xlsx')

  # State centroids (used in leaflet map)
  state_c = sf::read_sf('state_centroids.gpkg')
  state_b = sf::read_sf('state_boundaries.gpkg')
  
  # Update a column name in data and left-join the
  # population data to it.
  counties = counties |> 
    left_join(population_density,
              by = join_by(county, state))
  
  counties |> 
    mutate(id = paste0(state,', ',county)) |> 
    filter(duplicated(id))
  # Update shape name to be generalized name for use in functions.
  counties = counties |> 
    dplyr::rename(shape_name = county)
  
  
  # Listen for a click on the leaflet map; if the current zoom was 
  # all of the Contiguous USA or a state, snag that state that was clicked.
  # If the current zoom was already state, snag the county.
  observeEvent(input$leafmap_click, {
    
    clicked_state_name = str_remove(str_extract(input$leafmap_shape_click$id,'.*_'),"_")
    clicked_county_name = str_remove(str_extract(input$leafmap_shape_click$id,'_.*'),"_")
    
    req(!is.na(input$leafmap_shape_click$id))
    if(input$state_chosen != clicked_state_name){
      updatePickerInput(
        session = session,
        'county_chosen',
        selected = 'All'
      )
      
      leafletProxy('leafmap') |> 
        clearGroup('provider_pins')
      
      updatePickerInput(
        session = session,
        'state_chosen',
        selected = clicked_state_name
        )
    } else {
      updatePickerInput(
        session = session,
        'county_chosen',
        selected = clicked_county_name
      )
    }
  })
  
  # Labels for the summary boxes. These respond to which state and/or county
  # are seleceted, if any.
  state_or_national_scale = reactiveVal('National Scale Summary')
  selected_county_label = reactiveVal('County Summary')
  
  output$state_or_national_scale = renderText(state_or_national_scale())
  output$selected_county_label = renderText(selected_county_label())
  
  # Update county choices based on selected state.
  observeEvent(input$state_chosen, {

    # If 'Contiguous USA' is re-selected,
    # clear county_chosen.
    if(input$state_chosen == 'Contiguous USA'){
      updatePickerInput(session = session,
                        'county_chosen',
                        selected = 'All')
    }
    
    # If a state is chosen, do the following...
    req(input$state_chosen != 'Contiguous USA')
    
    state_or_national_scale(paste0(input$state_chosen," Summary"))
    
    shinyWidgets::updatePickerInput(
      session = session,
      'county_chosen',
      choices = c("All", counties[counties$state == input$state_chosen,]$shape_name)
    )
  })

  # Once a county is selected, snag the NPI records with zip codes in that county.
  providers_in_county = reactive({
    req(input$county_chosen != 'All')

    selected_county_label(input$county_chosen)
    
    npidata_zip |> 
      dplyr::filter(state == input$state_chosen,
                    county == input$county_chosen) |> 
      dplyr::group_by(shortzip) |> 
      dplyr::summarise(number_providers = sum(n, na.rm=T)) |> 
      dplyr::left_join(
        zipcodeR::zip_code_db |> 
          dplyr::select(shortzip = zipcode,
                        lat, lng)
      )
  })
  
  # We give the user a number of preset options of
  # healthcare provider types for Grouping 1;
  # these preset options are called 'bundles' throughout this code.
  selected_bundle_choices = reactive({
    group_bundles |>
      # Select the column indicated by user input 'group_1_bundle'.
      dplyr::select(!!sym(input$group_1_bundle)) |>
      # Remove rows with just NA for value.
      filter(!is.na(!!sym(input$group_1_bundle))) |>
      # Apply the same corrections to the labels
      mutate(!!sym(input$group_1_bundle) := str_replace_all(!!sym(input$group_1_bundle), ',', '<br>')) |> 
      mutate(!!sym(input$group_1_bundle) := str_replace_all(!!sym(input$group_1_bundle), '\\(', '<br>(')) |> 
      mutate(!!sym(input$group_1_bundle) := str_replace_all(!!sym(input$group_1_bundle), 'Medicine ', 'Medicine<br>')) |> 
      # Extract vector of values from column.
      dplyr::pull(!!sym(input$group_1_bundle))
  })
  
  # Define an intermediate version of the dataset based on user choice
  # of 'bundle' for Group 1. We use this intermediate dataset in the 
  # server 'back-end'
  group_one_intermediate = reactive({
    
    npidata_sum |> 
      filter(Classification %in% selected_bundle_choices()) |> 
      # Format some Classification names to better fit inside a drop-down box.
      mutate(Classification = str_replace_all(Classification, ',', '<br>')) |> 
      mutate(Classification = str_replace_all(Classification, '\\(', '<br>(')) |> 
      mutate(Classification = str_replace_all(Classification, 'Medicine ', 'Medicine<br>'))
    
  })
  
  # Define a reactive of the healthcare provider classification types
  # that are within the selected bundle for Group 1.
  group_one_classification_options = reactive({
    
    group_one_intermediate() |> 
      dplyr::select(Classification) |> 
      distinct() |> 
      dplyr::pull(Classification)
  })
  
  # Grouping 2 Classification options - fixed to MH/SUD Providers,
  # so this object is not reactive.
  group_two_classification_options = group_bundles |> 
    # Drop rows with NA for value.
    get_clean_values()
  
  # A reactively rendered piece of UI: allows the user
  # to refine exactly which classifications to include in 
  # their selection after having chosen a bundle.
  output$classification_one_choice_ui = renderUI({
    # req(length(input$classification_one_choice) == length(group_one_classification_options()))
    dol_pickerInput(inputId = 'classification_one_choice',
                    choices = group_one_classification_options())
  })
  
  # Extra reactive to catch the momentary lull where the 
  # selected values haven't changed but the options for group 1 
  # have... this otherwise leads to an app crash.
  group_one_reactive_choice = reactiveVal(NULL)
  
  # When the user updates the classification one choice, store
  # that choice in this reactiveVal.
  observe({
    group_one_reactive_choice(input$classification_one_choice)
  })
  
  # As above, this is a reactively rendered UI piece for 
  # fine-tuning the second group's list of included
  # classifications.
  output$classification_two_choice_ui = renderUI({
    dol_pickerInput(inputId = 'classification_two_choice',
                    choices = group_two_classification_options)
    
  })
  
  # A reactive that stores various metrics of provider types 
  # at the spatial scale for Group 1 in the comparison.
  
  shapes_one = reactive({
    req(!is.null(group_one_reactive_choice()))

    # Join the selected healthcare provider data table 
    # to the counties spatial object.
    intermediate = counties |> 
      join_healthcare_data_to_shapes(
        # Isolate the reactivity of group_one_intermediate to prevent repeated
        # invalidation of shapes_one reactive.
        group_dat_interm = isolate(group_one_intermediate()),
        group_dat_choice = group_one_reactive_choice()#,
        # ent_type = input$entity_type
      ) |> 
      calculate_population_based_metrics(
        chosen_variable = input$variable_to_map
      )
    
    # If the selected variable to map is people per provider,
    # ensure that the value is presented as NA for a value of 0 or NA.
    if(input$variable_to_map == 'people_per_provider'){
      intermediate[intermediate$var_chosen == 0 | is.na(intermediate$var_chosen),]$var_chosen = NA
    }
    
    return(intermediate)
  })
  
  # As above, a reactive that stores various metrics of provider types 
  # at the ZCTA spatial scale for Group 1 in the comparison. 
  # In this mock-up, the spatial scale is St. Paul
  shapes_two = reactive({
    
    req(input$classification_two_choice)
    
    intermediate = counties |> 
      join_healthcare_data_to_shapes(
        group_dat_interm = npidata_sum |> filter(Grouping == group_two),
        group_dat_choice = input$classification_two_choice#,
        # ent_type = input$entity_type
      ) |> 
      calculate_population_based_metrics(
        chosen_variable = input$variable_to_map
      )
    
    if(input$variable_to_map == 'people_per_provider'){
      intermediate[intermediate$var_chosen == 0 | is.na(intermediate$var_chosen),]$var_chosen = NA
    }
    
    return(intermediate)
  })
  
  # Calculate summary values for group 1 and 2;
  # Step 1: calculate the summary number
  # Step 2: Add text to summary
  summary_value_group_one_state = reactive({
    req(!is.null(input$state_chosen) & !is.null(shapes_one()) & !is.null(input$variable_to_map))
    calc_summary_value(input$state_chosen,
                       'All',
                       shapes_one(), 
                       input$variable_to_map, 
                       var_chosen)
  })
  
  summary_value_group_one_county = reactive({
    req(!is.null(input$state_chosen) & !is.null(shapes_one()) & !is.null(input$variable_to_map))
    calc_summary_value(input$state_chosen,
                       input$county_chosen,
                       shapes_one(), 
                       input$variable_to_map, 
                       var_chosen)
  })
  
  # Add text...
  output$group1_summary_value_state = renderText({
    add_text_to_summary(input$variable_to_map, summary_value_group_one_state())
  })
  
  output$group1_summary_value_county = renderText({
    if(input$county_chosen == 'All'){
      add_text_to_summary(input$variable_to_map, 'No County Selected')
    } else {
    add_text_to_summary(input$variable_to_map, summary_value_group_one_county())
    }
  })
  
  summary_value_group_two_state = reactive({
    calc_summary_value(input$state_chosen,
                       'All',
                       shapes_two(), 
                       input$variable_to_map, 
                       var_chosen)
  })
  
  summary_value_group_two_county = reactive({
    calc_summary_value(input$state_chosen,
                       input$county_chosen,
                       shapes_two(), 
                       input$variable_to_map, 
                       var_chosen)
  })
  
  # Add text...
  output$group2_summary_value_state = renderText({
    add_text_to_summary(input$variable_to_map, summary_value_group_two_state())
  })
  
  output$group2_summary_value_county = renderText({
    if(input$county_chosen == 'All'){
      add_text_to_summary(input$variable_to_map, 'No County Selected')
    } else {
    add_text_to_summary(input$variable_to_map, summary_value_group_two_county())
    }
  })
  
  ratio_summary_value_state = reactive({
    req(summary_value_group_one_state(),summary_value_group_two_state())
    if(summary_value_group_two_state() == 0) return( "Please select Group 2 Classification")
    # Calculate total N for group 2.
      paste0(
        round(
          summary_value_group_one_state() / summary_value_group_two_state(),
          3)
      )
  })
  
  ratio_summary_value_county = reactive({
    req(summary_value_group_one_county(),summary_value_group_two_county())
    if(summary_value_group_two_county() == 0) return( "Please select Group 2 Classification")
    # Calculate total N for group 2.
    if(input$county_chosen == 'All'){
      'No County Selected'
    } else {
      paste0(
      round(
        summary_value_group_one_county() / summary_value_group_two_county(),
        3)
    )
    }
  })
  
  output$ratio_summary_value_state = renderText({
    ratio_summary_value_state()
  })
  
  output$ratio_summary_value_county = renderText({
    ratio_summary_value_county()
  })
  
  # Select which ZCTA choropleth to map, based on 
  # user selection of radio buttons.
  shape_to_map = reactive({
    select_shape_to_map(input$group_to_map,
                       shapes_one(),shapes_two(),
                       input$classification_two_choice)
  })
  
  shape_always_ratio = reactive({
    select_shape_to_map('ratio',
                       shapes_one(),shapes_two(),
                       input$classification_two_choice)
  })
  
  # Keep track of what the minimum and maximum values of 
  # the selected variable are across both groups. We use this
  # to structure the map's colour palette.
  min_universal_num = reactive({
    req(shapes_one(),shapes_two())
    min(c(min(shapes_one()$var_chosen,na.rm=T),
          min(shapes_two()$var_chosen,na.rm=T)))
  })
  
  max_universal_num = reactive({
    req(shapes_one(),shapes_two())
    max(c(max(shapes_one()$var_chosen,na.rm=T),
          max(shapes_two()$var_chosen,na.rm=T)))
  })
  
  min_ratio_num = reactive({
    req(shapes_one(),shapes_two())
    min(c(min(shape_always_ratio()$var_chosen,na.rm=T),
          min(shape_always_ratio()$var_chosen,na.rm=T)))
  })
  
  max_ratio_num = reactive({
    req(shapes_one(),shapes_two())
    max(c(max(shape_always_ratio()$var_chosen,na.rm=T),
          max(shape_always_ratio()$var_chosen,na.rm=T)))
  })
  
  # See which state has been selected
  map_zoom_to_apply = reactive({
    req(!is.null(input$state_chosen))
    if(input$state_chosen == 'Contiguous USA'){
      dplyr::tibble(
        lat = 39.059, 
        lng = -100.12,
        zoom = 4
      )
    } else {
      if(input$county_chosen == 'All'){
        # No county selected.
        if(input$state_chosen == 'Alaska'){
          dplyr::tibble(
            lat = 65.57, 
            lng = -151.45,
            zoom = 3
          )
        } else {
          this_state = state_c |> 
            dplyr::filter(state_name == input$state_chosen)
          
          state_coords = this_state |> 
            sf::st_coordinates() |> 
            as.data.frame()
          
          dplyr::tibble(
            lat = state_coords$Y, 
            lng = state_coords$X,
            zoom = this_state$zoom_factor
          )
        }
      } else {
        # County selected!
        county_coords = sf::st_coordinates(
          sf::st_centroid(
            counties |> 
              dplyr::filter(state == input$state_chosen) |> 
              dplyr::filter(shape_name == input$county_chosen)
          )
        ) |> 
          as.data.frame()
        
        dplyr::tibble(
          lat = county_coords$Y, 
          lng = county_coords$X,
          zoom = case_when(
            input$state_chosen == 'Alaska' ~ 6,
            # input$state_chosen %in% c("New York") ~ 10,
            T ~ 7)
        )
      }
    }
  })
  
  # Calculate the colour palette for the leaflet map.
  my_pal = reactive({
    calc_leaflet_col_palette(legend = F,
                             g_to_map = input$group_to_map,
                             shape_to_map(),
                             min_value = min_universal_num(),
                             max_value = max_universal_num(),
                             input$variable_to_map)
  })
  
  my_pal_always_ratio = reactive({
    calc_leaflet_col_palette(legend = F,
                             g_to_map = 'ratio',
                             shape_always_ratio(),
                             min_value = min_ratio_num(),
                             max_value = max_ratio_num(),
                             input$variable_to_map)
  })
  
  # Calculate the legend colour palette for the leaflet map
  # (this palette is reversed from the one above so that the
  # colours are presented in a logical order in the legend)
  my_pal_legend = reactive({
    calc_leaflet_col_palette(legend = T,
                             g_to_map = input$group_to_map,
                             shape_to_map(),
                             min_value = min_universal_num(),
                             max_value = max_universal_num(),
                             input$variable_to_map)
  })
  
  my_pal_legend_ratio = reactive({
    calc_leaflet_col_palette(legend = T,
                             g_to_map = 'ratio',
                             shape_always_ratio(),
                             min_value = min_ratio_num(),
                             max_value = max_ratio_num(),
                             input$variable_to_map)
  })
  
  variable_label = reactive({
    calc_var_label(input$group_to_map,input$variable_to_map,shape_to_map())
  })
  
  variable_label_ratio = reactive({
    calc_var_label('ratio',input$variable_to_map,shape_always_ratio())
  })
  
  # First, set up the (mostly) static components of the map.
  # This code section runs when the user changes which classifications
  # to display.
  output$leafmap = renderLeaflet({
    req(!is.null(min_universal_num()), !is.null(group_one_reactive_choice()))

    # # Pull out select variables we want to trigger re-rendering; others, isolate.
    # selected_group_to_map = reactive(input$group_to_map)
    # selected_variable_to_map = reactive(input$variable_to_map)
    
    m = #isolate(
      initialize_dol_leaflet(state_c, state_b, counties,
                             min_universal_num, max_universal_num,
                             my_pal_legend, legend_label, input$group_to_map,
                             input$variable_to_map, shape_to_map,
                             variable_label, my_pal)
    #)
    
    # If a certain state has been chosen, 
    # set the view of the map to that state...
    isolate(
      if(!is.null(input$state_chosen)){
        m = m |> 
        setView(
          lng = map_zoom_to_apply()$lng,
          lat = map_zoom_to_apply()$lat,
          zoom = map_zoom_to_apply()$zoom
        )
      }
    )
    # And add in the state's borders in a golden colour.
    isolate(
      if(input$state_chosen != 'Contiguous USA'){
        m = m |> 
          clearGroup('highlighted') |> 
          addPolylines(
            group = 'highlighted',
            color = 'gold',
            stroke = 2,
            fillColor = 'transparent',
            data = state_b |> 
              filter(state_name == input$state_chosen) |> 
              sf::st_cast("MULTILINESTRING")
          )
      } else {
        m = m |> 
          clearGroup('highlighted')
      }
    )
      m
  })
  
  legend_label = reactive({
    render_legend_label(input$group_to_map,input$variable_to_map)
  })
  
  legend_label_ratio = reactive({
    render_legend_label('ratio',input$variable_to_map)
  })
  
  # Reactive text label for summary box.
  output$sum_box_label = renderText(stringr::str_replace_all(legend_label(),"<br>"," "))

  
  # Set up the reactive parts of the leaflet map
  # with a 'proxy' call. This code section runs when the user 
  # changes which state is selected.

  observe({
    req(!is.null(input$state_chosen))
    
      m = leafletProxy('leafmap')
      
      # If a county is not (yet) selected, change view.
      # If a county is selected, we don't want to change view, it's too jarring.
      # if(input$county_chosen == 'All'){
        m = m |> 
          setView(
          lng = map_zoom_to_apply()$lng,
          lat = map_zoom_to_apply()$lat,
          zoom = map_zoom_to_apply()$zoom
        )
      # }
      
      if(input$state_chosen != 'Contiguous USA'){
        if(input$county_chosen == 'All'){
        m = m |> 
          clearGroup('highlighted') |>
          clearGroup('provider_pins') |> 
          addPolylines(
            group = 'highlighted',
            color = 'gold',
            stroke = 2,
            fillColor = 'transparent',
            data = state_b |> 
              filter(state_name == input$state_chosen) |> 
              sf::st_cast("MULTILINESTRING")
          )
        } else {
          m = m |> 
            clearGroup('highlighted') |> 
            clearGroup('provider_pins') |> 
            addPolylines(
              group = 'highlighted',
              color = 'gold',
              stroke = 2,
              fillColor = 'transparent',
              data = counties |> 
                filter(state == input$state_chosen,
                       shape_name == input$county_chosen) |> 
                sf::st_cast("MULTILINESTRING")
            )
        }
      } else {
        m = m |> 
          clearGroup('highlighted')
      }
      
      # Add in providers by their zip code as pins
      if(input$county_chosen != 'All'){
        m = m |> 
          clearGroup('provider_pins') |> 
          addCircleMarkers(
            label = ~paste0("Zipcode ",shortzip,": ",number_providers),
            # color = ~colorBin('Spectral',
            #                   providers_in_county()$number_providers,
            #                   3)(number_providers),
            # fillColor = ~colorBin('Spectral',
            #                       providers_in_county()$number_providers,
            #                       3)(number_providers),
            color = 'darkred',
            fillColor = 'darkred',
            fillOpacity = 0.5,
            radius = ~case_when(
              number_providers <= max(number_providers)*.33 ~ 4,
              number_providers > max(number_providers)/3 & number_providers <= max(number_providers)*.66 ~ 8,
              number_providers > max(number_providers)*.66 ~ 12,
            ),
            data = providers_in_county(),
            group = 'provider_pins'
          )
      }
      m
  })
  
  
  output$report_pdf <- downloadHandler(
    filename <- paste0("Healthcare Provider Comparison Tool ",format(Sys.time(), "%Y-%b-%d"),".pdf"),
    
    
    content = function(file) {
      
      withProgress(message = 'Report generation in progress',
                   detail = 'Please wait a moment',
                   value = 0, {
                     
                     src <- normalizePath(c('pdf_report_markdown.Rmd')) # SEE HERE
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, c('pdf_report_markdown.Rmd'), overwrite = TRUE) # SEE HERE
                     # save map in tempfile
                     map_path <- paste0(tempdir(), "/map.png")
                     incProgress(1/2, message = 'Generating Report...')
                     cat("About to print out map")
                     mapshot(mapdown(), file = map_path,
                             cliprect = "viewport",
                             remove_controls = c("zoomControl", "layersControl", "homeButton",
                                                 "drawToolbar", "easyButton"),
                             selfcontained = F)
                     params <- list(
                       state_selected = input$state_chosen,
                       county_selected = input$county_chosen,
                       group_one_bundle = input$group_1_bundle,
                       group_one_items = group_one_classification_options(),
                       group_two_items = input$classification_two_choice,
                       # group_to_map = input$group_to_map,
                       group_to_map = 'ratio',
                       var_to_map = input$variable_to_map,
                       # var_to_map = 'prop_in_bcbs',
                       # entity_types_included = input$entity_type,
                       mean_provs_in_network_group_one_state = summary_value_group_one_state(),
                       mean_provs_in_network_group_one_county = summary_value_group_one_county(),
                       mean_provs_in_network_group_two_state = summary_value_group_two_state(),
                       mean_provs_in_network_group_two_county = summary_value_group_two_county(),
                       mean_provs_in_network_ratio_state = ratio_summary_value_state(),
                       mean_provs_in_network_ratio_county = ratio_summary_value_county(),
                       Map = map_path
                     )
                     
                     Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
                     out <- rmarkdown::render('pdf_report_markdown.Rmd', 
                                              output_file = file,
                                              params = params, 
                                              envir = new.env(parent = globalenv())
                     )
                     incProgress(1/2, message = 'Saved PDF report.')
                     
                     file.rename(out, file)
                   })  #End of withProgress function.
    } #End of downloadHandler's content section.
  )
  
  # map that will be downloaded
  mapdown <- reactive({
    # leaflet() |> 
    #   addTiles() |> 
    #   addPolygons(
    #     data = state_b
    #   )
 

    m = initialize_dol_leaflet(state_c, state_b, shape_always_ratio,
                               min_ratio_num, max_ratio_num,
                               my_pal_legend_ratio, legend_label_ratio,
                               # input$group_to_map,
                               'ratio',
                               input$variable_to_map, shape_always_ratio,
                               variable_label_ratio, my_pal_always_ratio,
                               set_view = data.frame(
                                 zoom = (input$leafmap_zoom),
                                 lat = input$leafmap_center$lat,
                                 lng = input$leafmap_center$lng),
                           is_interactive = F,
                           input$state_chosen)
    
    isolate(
      if(!is.null(input$state_chosen)){
        m = m |> 
          setView(
            lng = map_zoom_to_apply()$lng,
            lat = map_zoom_to_apply()$lat,
            zoom = map_zoom_to_apply()$zoom + 1
          )
      }
    )
    
    m
  })
}

shinyApp(app_ui, server)