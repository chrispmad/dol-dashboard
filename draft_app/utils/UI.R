if(webshot::is_phantomjs_installed() == F){
  webshot::install_phantomjs(force = T)
}

Sys.setenv(OPENSSL_CONF="/dev/null")

library(shiny)
library(shinyWidgets)
# library(shinyalert)
library(sf)
library(tidyverse)
library(bslib)
library(leaflet)
library(webshot)
library(mapview)
# library(shinyjs)
library(shinyFeedback)
library(readxl)

# Excel file for Group 1 bundles.
group_bundles = read_excel('www/Grouping-Specialties.xlsx')

# Choose theme for dashboard #
my_theme = bslib::bs_theme(bootswatch = 'flatly',
                           heading_font = 'Arial',
                           # base_font = 'Source Sans Pro',
                           base_font = 'Arial',
                           # primary = "#EA80FC", 
                           # secondary = "#48DAC6",
                           # "summary_card" = "('bg': #3a3384)",
                           "sidebar-bg" = '#ADD8E7')

summary_widgets = layout_column_wrap(
  width = 1/2,
  card(
    h4(textOutput('state_or_national_scale'), style='margin-bottom:-0.75rem;'),
    h6(textOutput('sum_box_label_state', inline=TRUE), style = 'text-align:center;font-weight:bold;margin-bottom:-0.5rem;'),
    h6("Group 1 ",textOutput("group1_summary_value_state", inline = TRUE),
       HTML("<br><hr style='margin:4px;'>"),
       "Group 2 ",textOutput("group2_summary_value_state", inline = TRUE),
       HTML("<br><hr style='margin:4px;'>"),
       "Ratio: ",textOutput("ratio_summary_value_state", inline = TRUE),
       style = 'text-align:center;margin:0px;font-size:medium;'),
    height = '155px',
    style = 'background:#c6c5da;'
  ),
  card(
    h5(textOutput('selected_county_label'), style='margin-bottom:-0.75rem;'),
    h6(textOutput('sum_box_label_county', inline=TRUE), style = 'text-align:center;font-weight:bold;margin-bottom:-0.5rem;'),
    h6("Group 1 ",textOutput("group1_summary_value_county", inline = TRUE),
       HTML("<br><hr style='margin:4px;'>"),
       "Group 2 ",textOutput("group2_summary_value_county", inline = TRUE),
       HTML("<br><hr style='margin:4px;'>"),
       "Ratio: ",textOutput("ratio_summary_value_county", inline = TRUE),
       style = 'text-align:center;margin:0px;font-size:medium;'),
    height = '155px',
    style = 'background:#c9bbce;'
  ), 
  
)
report_button = downloadButton(
  'report_pdf',
  'Generate Report',
  icon = shiny::icon('file-invoice')
)

state_chooser = shinyWidgets::pickerInput(
  'state_chosen',
  label = 'State',
  multiple = F,
  choices = c('Contiguous USA', c(state.name[c(1:8)],'District of Columbia',state.name[c(9:50)])),
  selected = 'Contiguous USA',
  options = list(
    `live-search` = TRUE)
)

county_chooser = shinyWidgets::pickerInput(
  'county_chosen',
  label = 'County',
  multiple = F,
  choices = 'All',
  options = list(
    `live-search` = TRUE)
)

variable_to_map = selectInput(
  inputId = 'variable_to_map',
  label = 'Which Variable to Map?',
  choices = c("Number of Providers" = 'n',
              'People per Provider' = 'people_per_provider',
              'Providers / Population Density' = 'providers_per_pop_density_sq_mile',
              '% of Providers in BCBS' = 'prop_in_bcbs'),
  selected = 'n')

bundle_chooser = div(
  selectInput(
    'group_1_bundle',
    label = '',
    multiple = F,
    choices = names(group_bundles |> dplyr::select(-`MH/SUD Providers`)),
    selected = 'All Medical/Surgical'
  ),
  style = 'margin-bottom:-2rem'
)

group_to_map = shiny::radioButtons(
  inputId = 'group_to_map',
  label = 'Which Group to Map?',
  choices = c('Group 1' = 'group1',
              'Group 2' = 'group2',
              'Ratio (Group 1 : Group 2)' = 'ratio'),
  selected = 'group1',
  inline = T
)

# entity_type_chooser = checkboxGroupButtons(
#   'entity_type',
#   'Entity Type',
#   choices = c("Individuals" = "1",
#               'Organizations' = '2'),
#   selected = '1'
# )

data_recency_bit = div(
  # top = '91vh', left = '105vh',
  p('NPI dataset coverage: 2005-05-23 to 2023-08-13'),
  style = 'font-size:small; left: 72%; top: 1%; width:25%; position:absolute;'
)

the_sidebar = sidebar(
  title = h4(str_to_title('Healthcare Provider Comparison Tool'), style = 'text-align:center;margin-bottom:5px;margin-top:-0.5rem'),
  width = '30%',
  bg = '#E4F2F7',
  HTML("<hr style = margin-top:0px>"),
  # h6("Zoom to:", style = 'margin-bottom:-1rem'),
  state_chooser,
  county_chooser,
  h6("Group 1 - All Medical/Surgical Providers", style = 'margin-top:-0.5rem;margin-bottom:-1rem'),
  bundle_chooser,
  uiOutput('classification_one_choice_ui'),
  h6("Group 2 - MH/SUD Providers", style = 'margin-bottom:-0.5rem'),
  uiOutput('classification_two_choice_ui'),
  HTML("<hr style = margin-top:0px;>"),
  group_to_map,
  variable_to_map,
  # entity_type_chooser,
  report_button
  # leaflet_colour_palette_chooser
)

main_bit = div(
  data_recency_bit,
  card(
    card_body(
      leafletOutput('leafmap', height = 500),
      summary_widgets
      #,
      # report_buttons
    )
  )
  # DT::DTOutput('dat_table')
)

# This was in the HTML style tags.
#  for selectize-input: height: 20px; width: 120px;
# .label.control-label, .selectize-control.single{ display: table-cell; text-align: 
#     left; vertical-align: middle; } 
# .form-group{ display: table-row;}
app_ui = page_fillable(
  shinyFeedback::useShinyFeedback(),
  
  tags$style(HTML("

.selectize-input {background-color:#48DAC6 !important; }
.shiny-input-container {height: 70px;}
.dropdown-toggle {background-color:#fffbffff !important;}
.leaflet-interactive[stroke='#e03'] {
                  display: none;
                  }")),
  
  theme = my_theme,
  title = 'Healthcare Provider Search Tool',
  layout_sidebar(
    sidebar = the_sidebar,
    main_bit
  )
)