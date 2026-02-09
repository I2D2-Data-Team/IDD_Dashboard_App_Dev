# LOAD Libraries ---------------------------------------------------------------
library(bslib)
library(shiny)
library(bsicons)
library(waiter)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(sf)

data_select_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3(strong("Data")),
        tags$hr(style = "margin: 0; padding: 0; height: 10px; border-top: 2px solid;"), # Styling should go to CSS file
        p(strong("Source: "), "SENCUS ACS", a("Table 123", href = "https://www.census.gov/programs-surveys/acs/data.html")),
        p(strong("Available Years: "), "2018-2025"),
        p(strong("Date Obtained: "), "January 29, 2026"),
        p(strong("Note: "), "Some notes about indicator"),
        br(),
        p("For additional info, see the ", a("IA Data Drive Manual", href = "https://i2d2.iastate.edu/wp-content/uploads/2025/04/IA-Data-Drive-Manual-v3.3.pdf"))
      )
    )
  )
}

data_select_server <- function(id, INDICATOR, DATA) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      data <- reactive({
        df <- DATA %>%
          my_indicators_data %>% 
          # select indicator of interest
          filter(indicator_name == INDICATOR) %>%
          # limit number of digits printed 
          mutate(value = round(value, 3)) %>%
          # rename column for grouping multi-level indicator
          {if (INDICATOR == "childcare_availability")
            mutate(., provider_type = group_level)
            else if (INDICATOR == "mom_edu")
              mutate(., education_level = group_level)
            else if (INDICATOR == "pre_school_enrolled")
              mutate(., academic_year = year_academic)
            else . } %>%
          # select only relevant variables
          select(
            any_of(
              c(`Variable Name` = "variable_label", 
                `FIPS Code` = "fips",
                `County Name` = "county_name", 
                `Year` = "year", 
                `Academic Year` = "academic_year",
                `Provider Type` = "provider_type", 
                `Homeless Type` = "homeless_type",
                `Income Level` = "income_level",
                `Children Present` = "children_present",
                `Education Level` = "education_level",
                `Individual Risk` = "individual_risk",
                `Cumulative Risk` = "cumulative_risk",
                `Value` = "value",
                `Variable Type` = "variable_type")))
        
        return(df)
      })
      
    }
  )
}




# START UI ---------------------------------------------------------------------
ui <- page_sidebar(
  # Set Bootstrap to version 5
  theme = bslib::bs_theme(version = 5), 
  
  # Measure shiny performance in a console, see: https://github.com/Appsilon/shiny.tictoc
  tags$script(src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"),
  
  # Add CSS styling
  includeCSS("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_App_Dev/refs/heads/main/common/Code/www/style.css"),
  
  # Add titile for browser
  window_title = "DEV Iowa Data Drive",
  
  # Add favicon to browser
  tags$head(
    tags$link(rel = "shortcut icon", href = "https://i2d2.iastate.edu/wp-content/uploads/2020/08/favicon-96x96-1.png")
  ),
  
  # Add loader to the whole dashabord and set style for individual figures
  useWaiter(),
  waiterPreloader(html = spin_5(), color = "grey"),
  
  # CREATE Left-Side-Bar UI ----------------------------------------------------
  sidebar = sidebar(
    selectizeInput(inputId = "GEOGRAPHY_SELECT", label = strong("Select Geography"), choices = NULL, selected = NULL, multiple = FALSE),
    selectizeInput(inputId = "LOCATION_SELECT", label = strong("Select Location"), choices = NULL, selected = NULL, multiple = TRUE),
    helpText(HTML("NOTE: Selected location(s) will be automatically shown across all indicators. To avoid cluttered plots number of selection is limited to  <strong>7</strong>."),
             style="margin-left: 15px; font-size: 12px; color:#0097CD; white-space: normal; max-width: 185px;"),
    br(),
    strong("Adjust Map"),
    shiny::checkboxInput('MAP_COUNTY_OUTLINES', "Show Outline of Selected Counties", value = TRUE),
    shiny::checkboxInput('MAP_COUNTY_LABELS', "Show Names of Selected Counties", value = FALSE),
    br(),
    strong("Adjust Figures"),
    checkboxInput(inputId = "ADD_STATEWIDE", label = "Include Statewide Data", value = TRUE),
    checkboxInput(inputId = "ADD_VALUE_LABELS", label = "Add Value Labels", value = FALSE),
    # show option for data type only for Child Demographics panel
    conditionalPanel(
      condition = "input.MEASURE == 'Child Demographics' || 'Household Characteristics'",
      br(),
      strong("Data Type"),
      radioButtons(inputId = "DATA_TYPE", label = NULL,
                   choices = list("Percentage" = 'percent', "Count" = 'count'),
                   selected = "percent"),
    ),
    br()
  ),
  
  # CREATE Tab Panels for each Measure -----------------------------------------
  navset_card_pill(
    id = "MEASURE",
    
    ## .. Child Demographics UI ------------------------------------------------
    
    nav_panel(
      title = "Child Demographics",
      fluidRow(
        column(
          width = 8,
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = "DEM_INDICATOR", 
                label = strong("Indicator"),
                choices = c(
                  "Child Age",
                  "Child Race and Ethnicity"
                ), 
                width = "100%"
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "DEM_SUBSET", 
                label = strong("Subset"),
                choices = c(""), 
                width = "100%"
              )
            ),
            column(
              width = 3,
              # show option for data type only for Child Race and Ethnicity indicator
              conditionalPanel(
                condition = "input.DEM_INDICATOR == 'Child Race and Ethnicity'",
                selectInput(
                  inputId = "DEM_GROUP", 
                  label = strong("Age"),
                  choices = c("0-4", "5-9", "10-14", "15-17"), 
                  selected = "0-4",
                  width = "100%"
                )
              )
            )
          )
        )
      ),
      br(),
      # bottom section for displaying data
      fluidRow(
        column(
          width = 6,
          uiOutput("DEM_FIG_NAME_1"),
          withWaiter(
            shiny::plotOutput("DEM_MAP"),
            html = spin_3k(), color = "white"
          ),
          p("Select Colors"),
          # select color palette
          selectInput(
            inputId = "DEM_MAP_COL",
            label = NULL, #"Chose Color Palette for Map",
            choices = map_color_choices
          ),
          shiny::downloadButton("DEM_MAP_DOWNLOAD", label = "Download the Map")
        ), 
        column(
          width = 6, 
          uiOutput("DEM_FIG_NAME_2"),
          withWaiter(
            shiny::plotOutput("DEM_TREND"),
            html = spin_3k(), color = "white"
          ),
          p("Select Years"),
          shiny::sliderInput("DEM_TREND_YEARS", label = NULL, 
                             min = 2020,
                             max = 2024,
                             value = c(2020, 2024),
                             step = 1,
                             sep = "",
                             ticks = FALSE,
                             width = '50%'),
          shiny::downloadButton("DEM_TREND_DOWNLOAD", label = "Download the Trend Line")
        )
      ), 
      br(),
      fluidRow(
        column(
          width = 12,
          uiOutput("DEM_FIG_NAME_3"),
          shiny::plotOutput("DEM_BAR"),
          shiny::downloadButton("DEM_BAR_DOWNLOAD", label = "Download the Bar Chart")
        )
      ),
      br(),
      data_select_ui(id = "GIO"),
      br()
    ),
    
    ## .. Household Characteristics UI -----------------------------------------
    
    nav_panel(
      title = "Household Characteristics",
      fluidRow(
        column(
          width = 10,
          fluidRow(
            column(
              width = 5,
              selectInput(
                inputId = "HSE_INDICATOR", 
                label = strong("Indicator"),
                choices = c(
                  "Household Type",
                  "Working Parents",
                  "First Time Mother",
                  "Language Spoken at Home",
                  "Technology Access",
                  "Transportation",
                  "Work Schedules"
                ), 
                width = "100%"
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "HSE_SUBSET", 
                label = strong("Subset"),
                choices = c(""), 
                width = "100%"
              )
            ),
            column(
              width = 3,
              uiOutput("HSE_DYNAMIC_FILTER_UI")
            )
          )
        )
      ),
      br(),
      # bottom section for displaying data
      fluidRow(
        column(
          width = 6, 
          uiOutput("HSE_FIG_NAME_1"),
          withWaiter(
            shiny::plotOutput("HSE_MAP"),
            html = spin_3k(), color = "white"
          ),
          p("Select Colors"),
          # select color palette
          selectInput(
            inputId = "HSE_MAP_COL",
            label = NULL, #"Chose Color Palette for Map",
            choices = map_color_choices,
            selected = "colors_red_gradient"
          ),
          shiny::downloadButton("HSE_MAP_DOWNLOAD", label = "Download the Map")
        ), 
        column(
          width = 6, 
          uiOutput("HSE_FIG_NAME_2"),
          withWaiter(
            shiny::plotOutput("HSE_TREND"),
            html = spin_3k(), color = "white"
          ),
          p("Select Years"),
          shiny::sliderInput("HSE_TREND_YEARS", label = NULL, 
                             min = 2020,
                             max = 2024,
                             value = c(2020, 2024),
                             step = 1,
                             sep = "",
                             ticks = FALSE,
                             width = '50%'),
          shiny::downloadButton("HSE_TREND_DOWNLOAD", label = "Download the Trend Line")
        )
      ), 
      
      
      # ......................TEST HERE ................-----------
      # shiny::plotOutput("FIG"),
      # shiny::verbatimTextOutput("TEXT"),
      # shiny::textOutput("TEXT1"),
      # shiny::tableOutput("TABLE1"),
      # shiny::textOutput("TEXT2"),
      # shiny::tableOutput("TABLE2"),
      
      
      br(),
      conditionalPanel(
        condition = "input.HSE_INDICATOR == 'First Time Mother'",
        fluidRow(
          column(
            width = 6,
            uiOutput("HSE_FIG_NAME_3B"),
            shiny::plotOutput("HSE_FIG3B"),
            shiny::downloadButton("HSE_FIG3B_DOWNLOAD", label = "Download the Bar Chart")
          ),
          column(
            width = 6,
            uiOutput("HSE_FIG_NAME_4B"),
            shiny::plotOutput("HSE_FIG4B"),
            shiny::downloadButton("HSE_FIG4B_DOWNLOAD", label = "Download the Line Chart")
          )
        )
      ),
      conditionalPanel(
        condition = "input.HSE_INDICATOR != 'First Time Mother'",
        fluidRow(
          column(
            width = 12,
            uiOutput("HSE_FIG_NAME_3"),
            shiny::plotOutput("HSE_BAR"),
            shiny::downloadButton("HSE_BAR_DOWNLOAD", label = "Download the Bar Chart")
          )
        )
      )
    )
    # END of Tab Panels ----------------------------------------------------------
  )
)

server <- function(input, output, session) {
  
  # UPDATE DROP-DOWNS ----------------------------------------------------------
  
  ## > Update RIGHT-SIDE-BAR Drop-Down Lists -----------------------------------
  
  ### ··· Create GEOGRAPHY drop-down list --------------------------------------
  update_geo_dropdown_server("GIO.select.geography", parent_session = session)
  
  ### ··· Load drop-down data for LOCATION -------------------------------------
  dropdown_data.locations <- 
    load_geo_locations_server("GIO.load.locations", reactive(input$GEOGRAPHY_SELECT))
  
  ### ··· Create LOCATION drop-down list ---------------------------------------
  update_location_dropdown_server("GIO.update.locations", 
                                  dropdown_data.locations, 
                                  reactive(input$GEOGRAPHY_SELECT),
                                  parent_session = session)
  
  
  ## > Update Indicator SUBSET Drop-Down List ----------------------------------

  ### ··· Update DEM SUBSET drop-down selection choices ------------------------
  observeEvent(input$DEM_INDICATOR, {
    if (input$DEM_INDICATOR == "Child Age") {
      my_choices <- subset.dem.age
    } else if (input$DEM_INDICATOR == "Child Race and Ethnicity") {
      my_choices <- subset.dem.rac
    } else {
      my_choices <- "none"
    }
    updateSelectInput(
      session = session,
      inputId = "DEM_SUBSET",
      choices = my_choices
    )
  })

  ### ··· Update HSE SUBSET drop-down selection choices -------------------------
  observeEvent(input$HSE_INDICATOR, {
    if (input$HSE_INDICATOR == "Household Type") {
      my_choices <- subset.hse.typ
    } else if (input$HSE_INDICATOR == "Working Parents") {
      my_choices <- subset.hse.wrk
    } else if (input$HSE_INDICATOR == "Language Spoken at Home") {
      my_choices <- subset.hse.lng
    } else if (input$HSE_INDICATOR == "Technology Access") {
      my_choices <- subset.hse.tch
    } else if (input$HSE_INDICATOR == "Transportation") {
      my_choices <- subset.hse.trn
    } else {
      my_choices <- "none"
    }
    updateSelectInput(
      session = session,
      inputId = "HSE_SUBSET",
      choices = my_choices
    )
  })
  
  
  ## > Create Indicator GROUP Drop-Down List -----------------------------------
  
  ### ··· Add HSE GROUP drop-down selection choices for Household Type ---------
  output$HSE_DYNAMIC_FILTER_UI <- renderUI({
    req(input$HSE_INDICATOR)
    # switch group drop-down list based on indicator and subset selected
    if (input$HSE_INDICATOR == "Household Type") {
      my_hse_choices <- group.hse.typ
    } else if (input$HSE_INDICATOR == "Technology Access") {
      req(input$HSE_SUBSET)
      if (input$HSE_SUBSET == "Internet Access") {
        my_hse_choices <- group.hse.tch.int
      } else if (input$HSE_SUBSET == "Computing Device") {
        my_hse_choices <- group.hse.tch.cmp
      } else {
        my_hse_choices <- "none"
      }
    } else {
      my_hse_choices <- "none"
    }
    # switch group name based on indicator selected
    if (input$HSE_INDICATOR == "Household Type") {
      my_hse_name = "Age"
    } else {
      my_hse_name = "Type"
    }
    # generate drop-down UI
    if (input$HSE_INDICATOR %in% c("Household Type", "Technology Access")) {
      selectInput(
        inputId = "HSE_GROUP", 
        label = strong(my_hse_name),
        choices = my_hse_choices,
        width = "100%"
      )
    }
  })

  
  ## > Update TREND LINE SLIDER for Year-Range ---------------------------------
  
  ### ··· Get YEAR-RANGE for selected DEM indicator ----------------------------
  year_range.dem <- get_year_range_server("GIO.years.dem", reactive(input$DEM_INDICATOR), data.dem)
  
  ### ··· Get YEAR-RANGE for selected HSE indicator ----------------------------
  year_range.hse <- get_year_range_server("GIO.years.hse", reactive(input$HSE_INDICATOR), data.hse)
  
  ### ··· Update year-range for DEM SLIDER -------------------------------------
  observeEvent(year_range.dem(), {
    updateSliderInput(session, "DEM_TREND_YEARS",
                      min = year_range.dem()[[1]],
                      max = year_range.dem()[[2]],
                      value = select_my_year_range(year_range.dem(), 5))
  })
  
  ### ··· Update year-range for HSE SLIDER -------------------------------------
  observeEvent(year_range.hse(), {
    updateSliderInput(session, "HSE_TREND_YEARS",
                      min = year_range.hse()[[1]],
                      max = year_range.hse()[[2]],
                      value = select_my_year_range(year_range.hse(), 5))
  })
  
  
  ## > Update FIGURE TITLES ----------------------------------------------------
  ### ··· Get name of active indicator 
  current_indicator <- reactive({
    req(input$MEASURE)
    switch(input$MEASURE,
           "Child Demographics" = input$DEM_INDICATOR,
           "Household Characteristics" = input$HSE_INDICATOR)
  })
  
  ### ··· Get data type of active indicator 
  current_indicator_type <- reactive({
    req(input$MEASURE)
    req(current_indicator())
    type <-
      metadata.fig.types %>%
      filter(measure == input$MEASURE,
             indicator == current_indicator()) %>%
      pull(data_type)
    return(type)
  })
  
  ### ··· Get metadata for selected DEM indicator 
  fig_titles <- reactive({
    req(current_indicator(), input$MEASURE)
    if (input$MEASURE == "Household Characteristics" && current_indicator() == "Technology Access") {
      req(input$HSE_SUBSET)
      titles <- 
        metadata.fig.titles %>% 
        filter(measure == input$MEASURE, indicator == current_indicator(), subsets == input$HSE_SUBSET) %>%
        select(measure, indicator, figure, title, tool_tip_text, numerator, num_source, denominator, den_source)
    } else {
      titles <- 
        metadata.fig.titles %>% 
        filter(measure == input$MEASURE, indicator == current_indicator()) %>%
        select(measure, indicator, figure, title, tool_tip_text, numerator, num_source, denominator, den_source)
    }
    return(titles)
  })
  
  ### ··· Render figure titles 
  output$DEM_FIG_NAME_1 <- compose_tooltip_language("GIO.fig1.tooltip", fig_titles, years = year_range.dem, fig = 1)
  output$DEM_FIG_NAME_2 <- compose_tooltip_language("GIO.fig2.tooltip", fig_titles, years = reactive(input$DEM_TREND_YEARS), fig = 2)
  output$DEM_FIG_NAME_3 <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.dem, fig = 3)
  output$HSE_FIG_NAME_1 <- compose_tooltip_language("GIO.fig1.tooltip", fig_titles, years = year_range.hse, fig = 1)
  output$HSE_FIG_NAME_2 <- compose_tooltip_language("GIO.fig2.tooltip", fig_titles, years = reactive(input$HSE_TREND_YEARS), fig = 2)
  output$HSE_FIG_NAME_3 <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.hse, fig = 3)
  output$HSE_FIG_NAME_3B <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.hse, fig = 3)
  output$HSE_FIG_NAME_4B <- compose_tooltip_language("GIO.fig4.tooltip", fig_titles, years = reactive(input$HSE_TREND_YEARS), fig = 2) # assign fig = 2 to show year range
  
  
  # PLOT figures ---------------------------------------------------------------
  
  ## > Get list of selected geographies 
  list_selected.geos <- reactive({
    if (input$ADD_STATEWIDE) {
      selected.geos <- c(19, input$LOCATION_SELECT)
    } else {
      selected.geos <- input$LOCATION_SELECT
    }
    return(selected.geos)
  })
  
  ## > Get base map of selected geographies
  base.map.geos <- load_base_map_server("GCH.load.base.map", reactive(input$GEOGRAPHY_SELECT))
  
  ## > DEM indicators ----------------------------------------------------------
  ## > Create plots for DEM indicators
  
  ### ··· DATA -----------------------------------------------------------------
  
  ### ··· Read data for selected DEM indicator for Fig 1 and 2
  data.dem.1 <- reactive({
    req(input$DEM_INDICATOR, input$GEOGRAPHY_SELECT)
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$DEM_INDICATOR,
      "Child Age" =                 read_my_csv(dim_dir, geo_dir, "dem_age_fig1_2_3"),
      "Child Race and Ethnicity" =  read_my_csv(dim_dir, geo_dir, "dem_rac_fig1_2_3")
    )
  })
  
  ### ··· Read data for selected DEM indicator for Fig 3
  data.dem.3 <- reactive({
    # req(input$DEM_INDICATOR, input$GEOGRAPHY_SELECT) -- no need since already required by data.dem.1
    req(data.dem.1())   
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$DEM_INDICATOR,
      "Child Age" =                 data.dem.1(),
      "Child Race and Ethnicity" =  data.dem.1()
    )
  })
  
  ### ··· Get data for selected DEM indicator 
  data.dem <- reactive({
    req(input$DEM_SUBSET, input$DEM_GROUP, data.dem.1())
    switch(
      input$DEM_INDICATOR,
      "Child Age" = 
        (data.dem.1() %>% filter(subset_level == input$DEM_SUBSET)),
      "Child Race and Ethnicity" = 
        (data.dem.1() %>% filter(subset_level == input$DEM_SUBSET, group_level == input$DEM_GROUP))
      )
  })
  
  ### ··· Get data for selected DEM indicator 
  data.dem.group <- reactive({
    req(input$DEM_GROUP, data.dem.3())
    switch(
      input$DEM_INDICATOR,
      "Child Age" = 
        (data.dem.3() %>% mutate(group_level = subset_level)),
      "Child Race and Ethnicity" = 
        (data.dem.3() %>% filter(group_level == input$DEM_GROUP) %>% mutate(group_level = subset_level))
      )
  })
  
  ### ··· MAP ------------------------------------------------------------------
  ### ··· Get subset of selected DEM data for map
  map_data.dem.subset <- reactive({
    req(input$DATA_TYPE)
    req(nrow(data.dem()) > 0)
    req(year_range.dem())
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.dem() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.dem()
    }
    
    data <- 
      my_data %>%
      filter(year == year_range.dem()[[2]]) %>%
      filter(fips != "19") %>%
      mutate(fips = as.integer(fips)) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Plot map for selected DEM indicator
  map.dem <- reactive({
    req(map_data.dem.subset(), base.map.geos(), list_selected.geos())
    req(nrow(map_data.dem.subset()) > 0)
    plot_map_view(DATA = map_data.dem.subset(), 
                  BASE_MAP = base.map.geos(),
                  LOCATIONS = list_selected.geos(),
                  DATA_TYPE = input$DATA_TYPE, #"percent",
                  OUTLINES = input$MAP_COUNTY_OUTLINES,
                  LABELS = input$MAP_COUNTY_LABELS,
                  COL = input$DEM_MAP_COL)
  })
  
  ### ··· Render map for selected DEM indicator
  output$DEM_MAP <- renderPlot({
    map.dem()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[1], "for the state of Iowa")
  })
  )
  
  ### ··· Download map for selected DEM indicator
  output$DEM_MAP_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = map.dem() +
               labs(title = paste(fig_titles()$title[1]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    caption = sprintf(#"**Source:** IA Data Drive.\n**Data:** %s.\n**Year:** %s.\n**Downloaded on:** %s",
                      "**Source:** IA Data Drive.<br>**Data:** %s.<br>**Year:** %s.<br>**Downloaded on:** %s",
                      "INDICATOR_SOURCE", 2022,
                      format(Sys.Date(), "%B %d, %Y")),
                    alt = "map") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5),
                 plot.caption = element_markdown(size = 9, hjust = 0, margin = margin(l = 10), lineheight = 1)
               ), 
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  ### ··· TREND ----------------------------------------------------------------
  ### ··· Get subset of selected DEM data for trend line
  trend_data.dem.subset <- reactive({
    req(input$DEM_TREND_YEARS, input$DATA_TYPE)
    req(list_selected.geos(), dropdown_data.locations())
    req(nrow(data.dem()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.dem() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.dem()
    }
    
    data <- 
      my_data %>%
      filter(between(year, input$DEM_TREND_YEARS[1], input$DEM_TREND_YEARS[2])) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))
      )) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Render trend line for selected DEM indicator
  trend.dem <- reactive({
    req(trend_data.dem.subset(), current_indicator_type())
    req(nrow(trend_data.dem.subset()) > 0)
    plot_trend_view(DATA = trend_data.dem.subset(),
                    # DATA_TYPE =  current_indicator_type(),
                    DATA_TYPE =  input$DATA_TYPE,
                    LOCATIONS = input$LOCATION_SELECT, 
                    STATEWIDE = input$ADD_STATEWIDE, 
                    LABELS = input$ADD_VALUE_LABELS)
  })
  
  
  ### ··· LINE -----------------------------------------------------------------
  ### ··· Get subset of selected DEM data for line graph
  line_data.dem.subset <- reactive({
    req(input$DATA_TYPE)
    req(year_range.dem())
    req(nrow(data.dem.group()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.dem.group() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.dem.group()
    }
    
    if (input$DEM_INDICATOR == "Child Age") {
      my_groups <- subset.dem.age
    } else {
      my_groups <- subset.dem.rac
    }
    
    data <- 
      my_data %>%
      filter(between(year, input$DEM_TREND_YEARS[1], input$DEM_TREND_YEARS[2])) %>%
      filter(fips %in% 19) %>%
      mutate(group_level = factor(group_level, levels = my_groups)) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Create trend line for selected DEM indicator
  line.dem <- reactive({
    req(line_data.dem.subset(), current_indicator_type())
    req(nrow(line_data.dem.subset()) > 0)
    
    if (input$DEM_INDICATOR == "Child Age") {
      my_groups <- subset.dem.age
    } else {
      my_groups <- subset.dem.rac
    }
    
    plot_line_view(DATA = line_data.dem.subset(),
                   # DATA_TYPE =  current_indicator_type(),
                   DATA_TYPE =  input$DATA_TYPE,
                   LOCATIONS = 19,
                   STATEWIDE = TRUE,
                   GROUPS = my_groups,
                   LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render trend line for selected DEM indicator
  output$DEM_TREND <- renderPlot({
    line.dem()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[4], "for the state of Iowa")
  })
  )
  
  ### ··· Download trend line for selected DEM indicator
  output$DEM_TREND_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = line.dem() +
               labs(title = paste(fig_titles()$title[2]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "trend line") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 10, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  
  
  ### ··· BAR ------------------------------------------------------------------
  ### ··· Get subset of selected DEM data for bar chart
  bar_data.dem.subset <- reactive({
    req(input$DATA_TYPE)
    req(year_range.dem(), list_selected.geos(), dropdown_data.locations())
    req(nrow(data.dem.group()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.dem.group() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.dem.group()
    }
    
    data <- 
      my_data %>%
      filter(year == year_range.dem()[[2]]) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))),
             group = "none")
    
    if (input$DEM_INDICATOR %in% c("Child Age")) {
      data <-
        data %>%
        mutate(group = "none",
               group_level = factor(group_level, levels = subset.dem.age))
    } else if (input$DEM_INDICATOR %in% c("Child Race and Ethnicity")) {
      data <-
        data %>%
        mutate(group = ifelse(str_detect(group_level, "(H|h)ispanic"), "Ethnicity", "Race"),
               group = factor(group, levels = c("Race", "Ethnicity")),
               group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic")))
    }
    return(data)
  })
  
  ## ··· Render bar chart for selected DEM indicator
  bar.dem <- reactive({
    req(bar_data.dem.subset(), current_indicator_type())
    req(nrow(bar_data.dem.subset()) > 0)
    plot_bar_view(DATA = bar_data.dem.subset(),
                  # DATA_TYPE =  current_indicator_type(),
                  DATA_TYPE =  input$DATA_TYPE,
                  LOCATIONS = input$LOCATION_SELECT,
                  STATEWIDE = input$ADD_STATEWIDE,
                  LABELS = input$ADD_VALUE_LABELS,
                  FACET = input$DEM_INDICATOR %in% c("Child Race and Ethnicity"))
  })
  
  ### ··· Render bar chart for selected DEM indicator
  output$DEM_BAR <- renderPlot({
    bar.dem()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[3], "for the state of Iowa")
  })
  )
  
  ### ··· Download chart for selected DEM indicator
  output$DEM_BAR_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = bar.dem() +
               labs(title = paste(fig_titles()$title[3]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "bar chart") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  

  ## > HSE indicators ----------------------------------------------------------
  ## > Create plots for HSE indicators
  
  ### ··· DATA -----------------------------------------------------------------
  
  ### ··· Read data for selected HSE indicator for Fig 1 and 2
  data.hse.1 <- reactive({
    req(input$HSE_INDICATOR, input$GEOGRAPHY_SELECT)
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$HSE_INDICATOR,
      "Household Type" = 
        # need to combine individual age data from fig3 with all age from fig1
        (bind_rows(read_my_csv(dim_dir, geo_dir, "hse_typ_fig1_2") %>% mutate(group_level = '0-5'), 
                   read_my_csv(dim_dir, geo_dir, "hse_typ_fig3"))),
      "Working Parents" =           read_my_csv(dim_dir, geo_dir, "hse_wrk_fig1_2_3"),
      "First Time Mother" =         read_my_csv(dim_dir, geo_dir, "hse_ftm_fig1_2"),
      "Language Spoken at Home" =   read_my_csv(dim_dir, geo_dir, "hse_lng_fig1_2"),
      "Technology Access" =         read_my_csv(dim_dir, geo_dir, "hse_tch_fig1_2_3"),
      "Transportation" =            read_my_csv(dim_dir, geo_dir, "hse_trn_fig1_2"),
      "Work Schedules" =            read_my_csv(dim_dir, geo_dir, "hse_sch_fig1_2_3")
    )
  })
  
  ### ··· Read data for selected HSE indicator for Fig 3
  data.hse.3 <- reactive({
    # req(input$HSE_INDICATOR, input$GEOGRAPHY_SELECT)  -- no need since already required by data.hse.1
    req(data.hse.1())
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$HSE_INDICATOR,
      "Household Type" =            data.hse.1(),
      "Working Parents" =           data.hse.1(),
      "First Time Mother" =         read_my_csv(dim_dir, geo_dir, "hse_ftm_fig3_4"),
      "Language Spoken at Home" =   read_my_csv(dim_dir, geo_dir, "hse_lng_fig3"),
      "Technology Access" =         data.hse.1(),
      "Transportation" =            read_my_csv(dim_dir, geo_dir, "hse_trn_fig3"),
      "Work Schedules" =            data.hse.1()
    )
  })
  
  ### ··· Get data for selected HSE indicator 
  data.hse <- reactive({
    req(input$HSE_SUBSET, input$HSE_GROUP, data.hse.1())
    switch(
      input$HSE_INDICATOR,
      "Household Type" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET, group_level == input$HSE_GROUP)),
      "Working Parents" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET)),
      "First Time Mother" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET)),
      "Language Spoken at Home" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET)),
      "Technology Access" =
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET, group_level == input$HSE_GROUP)),
      "Transportation" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET)),
      "Work Schedules" = 
        (data.hse.1() %>% filter(subset_level == input$HSE_SUBSET))
      )
  })
  
  ### ··· Get data for selected HSE indicator 
  data.hse.group <- reactive({
    req(input$HSE_SUBSET, input$HSE_GROUP, data.hse.3())
    switch(input$HSE_INDICATOR,
           "Household Type" = 
             (data.hse.3() %>% filter(group_level == input$HSE_GROUP) %>% mutate(group_level = subset_level)),
           "Working Parents" = 
             (data.hse.3() %>% mutate(group_level = subset_level)),
           "First Time Mother" = 
             (data.hse.3() %>% filter(subset_level == input$HSE_SUBSET)),
           "Language Spoken at Home" = 
             (data.hse.3() %>% mutate(group = group_level, group_level = subset_level)),
           "Technology Access" = 
             (data.hse.3() %>% filter(subset_level == input$HSE_SUBSET)),
           "Transportation" = 
             (data.hse.3() %>% filter(subset_level == input$HSE_SUBSET)),
           "Work Schedules" = 
             (data.hse.3() %>% mutate(group_level = subset_level)))
  })
  
  ### ··· MAP ------------------------------------------------------------------
  ### ··· Get subset of selected HSE data for map
  map_data.hse.subset <- reactive({
    req(input$DATA_TYPE)
    req(nrow(data.hse()) > 0)
    req(year_range.hse())
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.hse() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.hse()
    }
    
    data <- 
      my_data %>%
      filter(year == year_range.hse()[[2]]) %>%
      filter(fips != "19") %>%
      mutate(fips = as.integer(fips)) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Plot map for selected HSE indicator
  map.hse <- reactive({
    req(map_data.hse.subset(), base.map.geos(), list_selected.geos())
    req(nrow(map_data.hse.subset()) > 0)
    plot_map_view(DATA = map_data.hse.subset(), 
                  BASE_MAP = base.map.geos(),
                  LOCATIONS = list_selected.geos(),
                  DATA_TYPE = input$DATA_TYPE,
                  # DATA_TYPE = current_indicator_type(),
                  OUTLINES = input$MAP_COUNTY_OUTLINES,
                  LABELS = input$MAP_COUNTY_LABELS,
                  COL = input$HSE_MAP_COL)
  })
  
  ### ··· Render map for selected HSE indicator
  output$HSE_MAP <- renderPlot({
    map.hse()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[1], "for the state of Iowa")
  })
  )
  
  ### ··· Download map for selected HSE indicator
  output$HSE_MAP_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = map.hse() +
               labs(title = paste(fig_titles()$title[1]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    caption = sprintf(#"**Source:** IA Data Drive.\n**Data:** %s.\n**Year:** %s.\n**Downloaded on:** %s",
                                      "**Source:** IA Data Drive.<br>**Data:** %s.<br>**Year:** %s.<br>**Downloaded on:** %s",
                                      "INDICATOR_SOURCE", 2022,
                                      format(Sys.Date(), "%B %d, %Y")),
                    alt = "map") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5),
                 plot.caption = element_markdown(size = 9, hjust = 0, margin = margin(l = 10), lineheight = 1)
               ), 
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  ### ··· TREND ----------------------------------------------------------------
  ### ··· Get subset of selected HSE data for trend line
  trend_data.hse.subset <- reactive({
    req(input$HSE_TREND_YEARS, input$DATA_TYPE)
    req(list_selected.geos(), dropdown_data.locations())
    req(nrow(data.hse()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.hse() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.hse()
    }
    
    data <- 
      my_data %>%
      filter(between(year, input$HSE_TREND_YEARS[1], input$HSE_TREND_YEARS[2])) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))
      )) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Render trend line for selected HSE indicator
  trend.hse <- reactive({
    req(trend_data.hse.subset(), current_indicator_type())
    req(nrow(trend_data.hse.subset()) > 0)
    plot_trend_view(DATA = trend_data.hse.subset(),
                    # DATA_TYPE =  current_indicator_type(),
                    DATA_TYPE =  input$DATA_TYPE,
                    LOCATIONS = input$LOCATION_SELECT, 
                    STATEWIDE = input$ADD_STATEWIDE, 
                    LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render trend line for selected HSE indicator
  output$HSE_TREND <- renderPlot({
    trend.hse()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[2], "for the state of Iowa")
  })
  )
  
  ### ··· Download trend line for selected HSE indicator
  output$HSE_TREND_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = trend.hse() +
               labs(title = paste(fig_titles()$title[2]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "trend line") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 10, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  
  ### ··· LINE -----------------------------------------------------------------
  ### ··· Get subset of selected HSE data for line graph
  line_data.hse.subset <- reactive({
    req(input$HSE_INDICATOR %in% c("First Time Mother"))
    req(input$DATA_TYPE)
    req(year_range.hse())
    req(nrow(data.hse.group()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.hse.group() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.hse.group()
    }
    
    data <- 
      my_data %>%
      filter(between(year, input$HSE_TREND_YEARS[1], input$HSE_TREND_YEARS[2])) %>%
      filter(fips %in% 19) %>%
      mutate(group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic"))) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Render trend line for First Time Mother - fig 4
  trend.hse.ftm <- reactive({
    req(line_data.hse.subset(), current_indicator_type())
    req(nrow(line_data.hse.subset()) > 0)
    plot_line_view(DATA = line_data.hse.subset(),
                  # DATA_TYPE =  current_indicator_type(),
                  DATA_TYPE =  input$DATA_TYPE,
                  LOCATIONS = 19,
                  STATEWIDE = TRUE,
                  GROUPS = c("White", "Black", "Asian", "Other", "Hispanic"),
                  LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render trend line for First Time Mother - fig 4
  output$HSE_FIG4B <- renderPlot({
    trend.hse.ftm()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[4], "for the state of Iowa")
  })
  )
  
  ### ··· Download trend line for First Time Mother - fig 4
  output$HSE_FIG4B_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = trend.hse.ftm() +
               labs(title = paste(fig_titles()$title[4]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "trend line") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 10, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  
  ### ··· BAR ------------------------------------------------------------------
  ### ··· Get subset of selected HSE data for bar chart
  bar_data.hse.subset <- reactive({
    req(input$DATA_TYPE)
    req(year_range.hse(), list_selected.geos(), dropdown_data.locations())
    req(nrow(data.hse.group()) > 0)
    req(input$HSE_SUBSET)
    
    if (input$DATA_TYPE == "count") {
      my_data <- 
        data.hse.group() %>%
        mutate(index = count)
    } else {
      my_data <- 
        data.hse.group()
    }
    
    data <- 
      my_data %>%
      filter(year == year_range.hse()[[2]]) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))
                           ))
    
    if (input$HSE_INDICATOR %in% c("Household Type")) {
      data <-
        data %>%
        mutate(group = "none",
               group_level = factor(group_level, levels = subset.hse.typ))
    } else if (input$HSE_INDICATOR %in% c("Working Parents")) {
      data <-
        data %>%
        mutate(group = "none",
               group_level = factor(group_level, levels = subset.hse.wrk))
    } else if (input$HSE_INDICATOR %in% c("Language Spoken at Home")) {
      data <-
        data %>%
        mutate(group = factor(group, levels = group.hse.lng),
               group_level = factor(group_level, levels = subset.hse.lng))
    } else if (input$HSE_INDICATOR %in% c("First Time Mother")) {
      data <-
        data %>%
        mutate(group = ifelse(str_detect(group_level, "(H|h)ispanic"), "Ethnicity", "Race"),
               group = factor(group, levels = c("Race", "Ethnicity")),
               group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic")))
    } else if (input$HSE_INDICATOR %in% c("Technology Access")) {
      # req(input$HSE_SUBSET)
      if (input$HSE_SUBSET == "Internet Access") {
        data <-
          data %>%
          mutate(group = "none",
                 group_level = factor(group_level, levels = group.hse.tch.int))
      } else {
        data <-
          data %>%
          mutate(group = "none",
                 group_level = factor(group_level, levels = group.hse.tch.cmp))
      }
    } else if (input$HSE_INDICATOR %in% c("Transportation")) {
      data <-
        data %>%
        mutate(group = "none",
               group_level = factor(group_level, levels = group.hse.trn))
    } else if (input$HSE_INDICATOR %in% c("Work Schedules")) {
      data <-
        data %>%
        mutate(group = "none",
               group_level = factor(group_level, levels = c("none"), labels = c("")))
    }
    return(data)
  })
  
  ## ··· Render bar chart for selected HSE indicator
  bar.hse <- reactive({
    req(bar_data.hse.subset(), current_indicator_type())
    req(nrow(bar_data.hse.subset()) > 0)
    if (input$HSE_INDICATOR %in% c("Transportation")) {
      plot_bar_view2(DATA = bar_data.hse.subset(),
                     # DATA_TYPE =  current_indicator_type(),
                     DATA_TYPE =  input$DATA_TYPE,
                     LOCATIONS = input$LOCATION_SELECT,
                     STATEWIDE = input$ADD_STATEWIDE,
                     LABELS = input$ADD_VALUE_LABELS,
                     FACET = FALSE)
    } else if ((input$HSE_INDICATOR == "Technology Access" && input$HSE_SUBSET == "Computing Device")) {
      plot_bar_view2(DATA = bar_data.hse.subset(),
                     # DATA_TYPE =  current_indicator_type(),
                     DATA_TYPE =  input$DATA_TYPE,
                     LOCATIONS = input$LOCATION_SELECT,
                     STATEWIDE = input$ADD_STATEWIDE,
                     LABELS = input$ADD_VALUE_LABELS,
                     FACET = FALSE)
    } else {
      plot_bar_view(DATA = bar_data.hse.subset(),
                    # DATA_TYPE =  current_indicator_type(),
                    DATA_TYPE =  input$DATA_TYPE,
                    LOCATIONS = input$LOCATION_SELECT,
                    STATEWIDE = input$ADD_STATEWIDE,
                    LABELS = input$ADD_VALUE_LABELS,
                    FACET = input$HSE_INDICATOR %in% c("Language Spoken at Home", "First Time Mother"))
    }
    
  })
  
  ### ··· Render bar chart for selected HSE indicator
  output$HSE_BAR <- renderPlot({
    bar.hse()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[3], "for the state of Iowa")
  })
  )
  
  ### ··· Download chart for selected HSE indicator
  output$HSE_BAR_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = bar.hse() +
               labs(title = paste(fig_titles()$title[3]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "bar chart") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  ### ··· Render bar chart for First Time Mother
  output$HSE_FIG3B <- renderPlot({
    bar.hse()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[3], "for the state of Iowa")
  })
  )
  
  ### ··· Download chart for selected HSE indicator
  output$HSE_FIG3B_DOWNLOAD <- downloadHandler(
    filename = "cip.png", #file_name(),
    content = function(file){
      ggsave(file, 
             plot = bar.hse() +
               labs(title = paste(fig_titles()$title[3]),
                    # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
                    alt = "bar chart") +
               theme(
                 plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
               ), 
             width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    }
  )

  
  ### ............. TESTING SPACE .................. -----------------------
  
  # output$TEXT <- renderPrint({
  #   str(map_data.hse.subset())
  # })
  
  # output$TEXT1 <- renderText({
  #   paste("input$DATA_TYPE = ", input$DATA_TYPE, " Nrow = ", nrow(map_data.hse.subset()), " GEO List = ", input$HSE_MAP_COL)
  #   # nrow(map_data.hse.subset())
  # })
  # output$TABLE1 <- renderTable({
  #   map_data.hse.subset() %>% head(10) %>% select(-count, - dimension)
  #   # bar_data.hse.subset() %>% head(7)
  # })
 
  # output$TEXT2 <- renderText({
  #   "Data raw filtered by FIPS and YEAR"
  # })
  # output$TABLE2 <- renderTable({
  #   fig_titles() 
  #   # data.hse.group() %>% filter(fips < 19003, year > 2022) %>% head(5)
  # })

  # output$FIG <- renderPlot({
  #   plot_map_view(DATA = map_data.hse.subset(), 
  #                 DATA_TYPE =  current_indicator_type(),
  #                 LOCATIONS = input$LOCATION_SELECT, 
  #                 OUTLINES = input$MAP_COUNTY_OUTLINES,
  #                 LABELS = input$MAP_COUNTY_LABELS)
  # })
  # 
  
  
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
