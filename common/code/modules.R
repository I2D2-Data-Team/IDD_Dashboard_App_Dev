# Read base map of selected geographies ----------------------------------------
# Notice: this module does not have/need UI function - headless module
# It accepts reactive input from GEOGRAPHY_SELECT, and reads corresponding data  
load_base_map_server <- function(id, reactive_trigger) {
  moduleServer(id, function(input, output, session) {
    selected.base.map <- reactive({
      # require that the trigger has a value to prevent error at startup
      req(reactive_trigger())
      # read correct sf data file based on selected geography
      switch(reactive_trigger(),
             "IA-county"   = read_rds("../common/data/map_IA-county.rds"),
             "ECI-area"    = read_rds("../common/data/map_ECI-areas.rds"),
             "HHS-region"  = read_rds("../common/data/map_HHS-regions.rds"),
             "HS-grantee"  = read_rds("../common/data/map_HS-grantess.rds"),
             "MIECHV-area" = read_rds("../common/data/map_MIECHV-areas.rds")
      )
    })
    # return data as reactive
    return(selected.base.map)
  }
  )
}

# Update drop-down list of geographies -----------------------------------------
update_geo_dropdown_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(parent_session,
                         inputId = 'GEOGRAPHY_SELECT',
                         choices = list(
                           "Iowa Counties"        = "IA-county",
                           "ECI Areas"            = "ECI-area",
                           "HHS Regions"          = "HHS-region",
                           "Head Start Grantees"  = "HS-grantee",
                           "MIECHV"               = "MIECHV-area"
                         ), 
                         options = list(maxItems = 1L, placeholder = "IA-county"),
                         server = TRUE)
  })
}

# Update drop-down list for locations ------------------------------------------
# Notice: this module does not have/need UI function - headless module
# It accepts reactive input from GEOGRAPHY_SELECT, and loads corresponding locations 
load_geo_locations_server <- function(id, reactive_trigger) {
  moduleServer(id, function(input, output, session) {
    selected.locations <- reactive({
      # require that the trigger has a value to prevent error at startup
      req(reactive_trigger())
      # read correct sf data file based on selected geography
      switch(reactive_trigger(),
             "IA-county"   = ia_county_droplist,
             "ECI-area"    = eci_area_droplist,
             "HHS-region"  = hhs_region_droplist,
             "HS-grantee"  = hs_grantee_droplist,
             "MIECHV-area" = miechv_droplist
      )
    })
    # return data as reactive
    return(selected.locations)
  }
  )
}

# Update drop-down list of geo locations ---------------------------------------
update_location_dropdown_server <- function(id, locations, reactive_trigger, parent_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      reactive_trigger(),
      updateSelectizeInput(session = parent_session,
                           inputId = 'LOCATION_SELECT',
                           choices = locations(),
                           options = list(maxItems = 7L, placeholder = 'Select Location(s)'),
                           server = TRUE)
    )
  })
}

# Compute year range for indicators --------------------------------------------
get_year_range_server <- function(id, indicator_trigger, data) {
  moduleServer(id, function(input, output, session) {
    year.range <- reactive({
      req(indicator_trigger(), data())
      req(nrow(data()) > 0)
      # compute year range of current indicator
      my_year_range <-
        data() %>%
        filter(fips == 19) %>%
        distinct(year) %>%
        range()
    })
    return(year.range)
  })
}

# Compose tooltip language -----------------------------------------------------
compose_tooltip_language <- function(id, data, years, fig = 1) {
  moduleServer(id, function(input, output, session) {
    renderUI({
      # get correct year labels and range
      if (fig == 2) {
        my_years_label <- "Years Shown:"
        my_years_shown <-  paste(years(), collapse = "-")
      } else {
        my_years_label <- "Year Shown:"
        my_years_shown <- years()[[2]]
      }
      # compose a formatted tooltip 
      span(
        h5(
          strong(data()$title[fig]),
          tooltip(bs_icon("info-circle"), 
                  tags$b("Description:"), data()$tool_tip_text[fig], tags$br(),
                  tags$b("Numerator:"), data()$numerator[fig], data()$num_source[fig], tags$br(),
                  tags$b("Denominator:"), data()$denominator[fig], data()$den_source[fig], tags$br(),
                  tags$b(my_years_label), my_years_shown, 
                  placement = "bottom"),
          style="text-align: center;"
        )
      )
    })
  })
}


# Build container with data source info ------------------------------------
build_data_source_container_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3(strong("Data")),
        tags$hr(style = "margin: 0; padding: 0; height: 10px; border-top: 2px solid;"),
        uiOutput(ns("display_info")),
        p("For additional info, see the ", 
          a("IA Data Drive Manual", 
            href = "https://i2d2.iastate.edu/wp-content/uploads/2025/04/IA-Data-Drive-Manual-v3.3.pdf",
            target = "_blank", rel = "noopener noreferrer"))
      )
    )
  )
}

build_data_source_container_server <- function(id, metadata) {
  moduleServer(id, function(input, output, session) {
    # Render the content that goes to display_info
    output$display_info <- renderUI({
      df <- metadata() %>%
        select(source, data, link, min_year, max_year, date_obtained, notes)
      
      # If no data is found return a silent message
      if (nrow(df) == 0) return(
        tags$p("No metadata available.")
      )
      # handle when indicator have multiple sources
      ui_elements <- pmap(df, function(source, data, link, min_year, max_year, date_obtained, notes) {
        # Define the HTML for ONE item
        tags$div(
          class = "metadata-container",
          tagList(
            p(strong("Source: "), source, a(data, href = link, target = "_blank", rel = "noopener noreferrer")),
            p(strong("Available Years: "), min_year, "-", max_year),
            p(strong("Date Obtained: "), date_obtained),
            if (!is.na(notes)) {
              p(strong("Note: "), notes)
            },
            br()
          )
        )
      })
      # IMPORTANT: Convert the list of tags into a Shiny tagList
      do.call(tagList, ui_elements) 
    })
  }
  )
}

