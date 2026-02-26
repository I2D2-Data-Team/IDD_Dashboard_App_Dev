# Set left side bar UI
shiny_sidebar_ui <- function() {
  sidebar(
    # selectizeInput(inputId = "GEOGRAPHY_SELECT", label = strong("Select Geography"), choices = NULL, selected = NULL, multiple = FALSE),

    selectizeInput(inputId = "GEOGRAPHY_SELECT", label = strong("Select Geography"),
                   choices = list(
                     "Iowa Counties"        = "IA-county",
                     "ECI Areas"            = "ECI-area",
                     "HHS Regions"          = "HHS-region",
                     "Head Start Grantees"  = "HS-grantee",
                     "MIECHV"               = "MIECHV-area"
                   ),
                   selected = 'IA-county', multiple = FALSE),

    selectizeInput(inputId = "LOCATION_SELECT", label = strong("Select Location"), choices = NULL, selected = NULL, multiple = TRUE),
    helpText(HTML("NOTE: Selected location(s) will be automatically shown across all indicators. To avoid cluttered plots number of selection is limited to  <strong>7</strong>."),
             style="margin-left: 15px; font-size: 12px; color:#0097CD; white-space: normal; max-width: 185px;"),
    br(),
    strong("Adjust Map"),
    shiny::checkboxInput('MAP_COUNTY_OUTLINES', "Show Outline of Selected Locations", value = TRUE),
    shiny::checkboxInput('MAP_COUNTY_LABELS', "Show Names of Selected Locations", value = FALSE),
    br(),
    strong("Adjust Figures"),
    checkboxInput(inputId = "ADD_STATEWIDE", label = "Include Statewide Data", value = TRUE),
    checkboxInput(inputId = "ADD_VALUE_LABELS", label = "Add Value Labels", value = FALSE),
    br(),
    strong("Data Type"),
    # # show option for data type only for Child Demographics panel
    # conditionalPanel(
    #   condition = "input.MEASURE == 'Child Demographics' || 'Household Characteristics'",
    #   radioButtons(inputId = "DATA_TYPE", label = NULL,
    #                choices = list("Percentage" = 'percent', "Count" = 'count'),
    #                selected = "percent"),
    # ),
    radioButtons(inputId = "DATA_TYPE", label = NULL,
                 choices = list("Percentage/Rate" = 'percent', "Count" = 'count'),
                 selected = "percent"),
    br()
  )
}