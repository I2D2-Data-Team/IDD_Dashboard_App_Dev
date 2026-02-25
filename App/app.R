# LOAD Libraries ---------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)


# # TESTING <<<<<<<<<<<<<<< -------------------------------------------------
# i2d2_logo_fixed <- 
#   magick::image_read("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/main/common/www/I2D2_Logo_short.png") %>% 
#   grid::rasterGrob(x = unit(0.98, "npc"), y = unit(-0.2, "npc"),
#                    just = c("right", "bottom"), 
#                    width = unit(2.2, "cm"))
# 
# 
# 
# format_figs_download <- function(fig, fig_title, fig_source_data, figure_number) {
#   my_title <- fig_title() %>% filter(figure == figure_number) %>% pull(title)
#   my_source <- fig_source_data()
#   url <- "https:// iadatadrive.i2d2.iastate.edu"
#   
#   download_fig <- 
#     fig +
#     labs(title = my_title,
#          # subtitle = "subtitle goes here",
#          caption = sprintf(
#            "<br>**Source:** I2D2, IA Data Drive, %s<br>**Data:** %s.<br>**Year:** %s<br>**Downloaded on:** %s",
#            url, my_source$source, my_source$year, my_source$date
#          ),
#          # tag = "Designed by Giorgi Chighladze",
#          alt = "Iowa figures") +
#     annotation_custom(i2d2_logo_fixed) +   
#     theme(
#       plot.title = element_textbox_simple(size = 33, face = "bold", halign = 0.45, vjust = 0.5, lineheight = 1.5),
#       plot.caption = element_markdown(size = 10, hjust = 0, margin = margin(l = 20), lineheight = 1.3),
#       plot.tag.position = c(0.99, 0.19),
#       plot.tag = element_text(hjust = 1, vjust = 1, size = 9, face = "bold.italic", color = "grey99"),
#       plot.margin = margin(t = 15, b = 15, l = 10, r = 10, unit = "pt")
#     ) +
#     coord_cartesian(clip = "off")
#   
#   return(download_fig)
# }
# # TESTING <<<<<<<<<<<<<<< -------------------------------------------------



# START UI ---------------------------------------------------------------------
ui <- page_sidebar(
  # Set Bootstrap to version 5
  theme = bslib::bs_theme(version = 5), 
  
  # Add title for browser
  window_title = "DEV Iowa Data Drive",
  
  # Add shiny settings
  load_shiny_dashboard_setting(),
  
  
  # CREATE Left-Side-Bar UI ----------------------------------------------------
  sidebar = shiny_sidebar_ui(),
  
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
      build_data_source_container_ui(id = "DEM_DATA_SOURCE"),
      br()
    ),
    
    ## .. Birth Risk UI --------------------------------------------------------
    
    nav_panel(
      title = "Birth Risks",
      fluidRow(
        column(
          width = 8,
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = "RSK_INDICATOR", 
                label = strong("Indicator"),
                choices = c(
                  "Birth to Teenage Mother",
                  # "Birth to Unmarried Mother",
                  # "Inadequate Prenatal Care",
                  # "Low Maternal Education at Birth",
                  # "Medicaid or WIC Receipt at Birth",
                  # "Preterm or Low Birth Weight",
                  # "Prenatal Tobacco Exposure",
                  "Cumulative Birth Risks"
                ), 
                width = "100%"
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "RSK_SUBSET", 
                label = strong("Subset"),
                choices = c(""), 
                width = "100%"
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
          uiOutput("RSK_FIG_NAME_1"),
          withWaiter(
            shiny::plotOutput("RSK_MAP"),
            html = spin_3k(), color = "white"
          ),
          p("Select Colors"),
          # select color palette
          selectInput(
            inputId = "RSK_MAP_COL",
            label = NULL, #"Chose Color Palette for Map",
            choices = map_color_choices
          ),
          shiny::downloadButton("RSK_MAP_DOWNLOAD", label = "Download the Map")
        ), 
        column(
          width = 6, 
          uiOutput("RSK_FIG_NAME_2"),
          withWaiter(
            shiny::plotOutput("RSK_TREND"),
            # shiny::imageOutput("RSK_TREND_IMG"), 
            html = spin_3k(), color = "white"
          ),
          p("Select Years"),
          shiny::sliderInput("RSK_TREND_YEARS", label = NULL, 
                             min = 2020,
                             max = 2024,
                             value = c(2020, 2024),
                             step = 1,
                             sep = "",
                             ticks = FALSE,
                             width = '50%'),
          shiny::downloadButton("RSK_TREND_DOWNLOAD", label = "Download the Trend Line")
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
      fluidRow(
        column(
          width = 6,
          uiOutput("RSK_FIG_NAME_3"),
          shiny::plotOutput("RSK_BAR"),
          shiny::downloadButton("RSK_BAR_DOWNLOAD", label = "Download the Bar Chart")
        ),
        column(
          width = 6,
          uiOutput("RSK_FIG_NAME_4"),
          shiny::plotOutput("RSK_LINE"),
          shiny::downloadButton("RSK_LINE_DOWNLOAD", label = "Download the Line Chart")
        )
      ),
      br(),
      conditionalPanel(
        condition = "input.RSK_INDICATOR == 'Cumulative Birth Risks'",
        fluidRow(
          column(
            width = 6,
            uiOutput("RSK_FIG_NAME_5"),
            shiny::plotOutput("RSK_BAR_STACKED"),
            shiny::downloadButton("RSK_BAR_STACKED_DOWNLOAD", label = "Download the Bar Chart")
          ),
          column(
            width = 6,
            uiOutput("RSK_FIG_NAME_6"),
            shiny::plotOutput("RSK_PIE"),
            shiny::downloadButton("RSK_PIE_DOWNLOAD", label = "Download the Pie Chart")
          ) 
        )
      )
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
                  # "Working Parents",
                  # "First Time Mother",
                  "Language Spoken at Home",
                  "Technology Access",
                  # "Transportation",
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
      ),
      
      br(),
      build_data_source_container_ui(id = "HSE_DATA_SOURCE"),
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
  
  ### ··· Update RSK SUBSET drop-down selection choices ------------------------
  observeEvent(input$RSK_INDICATOR, {
    if (input$RSK_INDICATOR == "Birth to Teenage Mother") {
      my_choices <- subset.rsk.btm
    } else if (input$RSK_INDICATOR == "Birth to Unmarried Mother") {
      my_choices <- subset.rsk.bum
    } else if (input$RSK_INDICATOR == "Inadequate Prenatal Care") {
      my_choices <- subset.rsk.ipc
    } else if (input$RSK_INDICATOR == "Low Maternal Education at Birth") {
      my_choices <- subset.rsk.lme
    } else if (input$RSK_INDICATOR == "Medicaid or WIC Receipt at Birth") {
      my_choices <- subset.rsk.mow
    } else if (input$RSK_INDICATOR == "Preterm or Low Birth Weight") {
      my_choices <- subset.rsk.plw
    } else if (input$RSK_INDICATOR == "Prenatal Tobacco Exposure") {
      my_choices <- subset.rsk.pte
    } else if (input$RSK_INDICATOR == "Cumulative Birth Risks") {
      my_choices <- subset.rsk.cum
    } else {
      my_choices <- "none"
    }
    updateSelectInput(
      session = session,
      inputId = "RSK_SUBSET",
      choices = my_choices
    )
  })
  

  
  ## > Update TREND LINE SLIDER for Year-Range ---------------------------------
  
  ### ··· Get YEAR-RANGE for selected DEM indicator ----------------------------
  year_range.dem <- get_year_range_server("GIO.years.dem", reactive(input$DEM_INDICATOR), data.dem)
  
  ### ··· Get YEAR-RANGE for selected HSE indicator ----------------------------
  year_range.hse <- get_year_range_server("GIO.years.hse", reactive(input$HSE_INDICATOR), data.hse)
  
  ### ··· Get YEAR-RANGE for selected RSK indicator ----------------------------
  year_range.rsk <- get_year_range_server("GIO.years.rsk", reactive(input$RSK_INDICATOR), data.rsk)
  
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
  
  ### ··· Update year-range for RSK SLIDER -------------------------------------
  observeEvent(year_range.rsk(), {
    updateSliderInput(session, "RSK_TREND_YEARS",
                      min = year_range.rsk()[[1]],
                      max = year_range.rsk()[[2]],
                      value = select_my_year_range(year_range.rsk(), 5))
  })
  
  
  ## > Update FIGURE TITLES ----------------------------------------------------
  ### ··· Get name of active indicator 
  current_indicator <- reactive({
    req(input$MEASURE)
    switch(input$MEASURE,
           "Birth Risks" = input$RSK_INDICATOR,
           "Child Demographics" = input$DEM_INDICATOR,
           "Household Characteristics" = input$HSE_INDICATOR)
  })
  
  ### ··· Get data type of active indicator 
  # current_indicator_type <- reactive({
  #   req(current_indicator())
  #   type <-
  #     metadata.fig.types %>%
  #     filter(measure == input$MEASURE,
  #            indicator == current_indicator()) %>%
  #     pull(data_type)
  #   return(type)
  # })
  current_indicator_type <- reactive({
    req(current_indicator())
    req(input$DATA_TYPE)
    type <-
      metadata.fig.types %>%
      filter(measure == input$MEASURE,
             indicator == current_indicator()) %>%
      pull(data_type)
    if (input$DATA_TYPE == 'count') {
      type <- "count"
    }
    return(type)
  })
  
  ### ··· Get metadata for selected DEM indicator 
  fig_titles <- reactive({
    req(current_indicator())
    if (input$MEASURE == "Household Characteristics" && current_indicator() == "Technology Access") {
      req(input$HSE_SUBSET)
      titles <- 
        metadata.fig.titles %>% 
        filter(measure == input$MEASURE, indicator == current_indicator(), subsets == input$HSE_SUBSET)
    } else {
      titles <- 
        metadata.fig.titles %>% 
        filter(measure == input$MEASURE, indicator == current_indicator())
    }
    output <- 
      titles %>%
      select(measure, indicator, figure, title, tool_tip_text, numerator, num_source, denominator, den_source)
    return(output)
  })
  
  ### ··· Render figure titles 
  output$DEM_FIG_NAME_1 <- compose_tooltip_language("GIO.fig1.tooltip", fig_titles, years = year_range.dem, fig = 1)
  output$DEM_FIG_NAME_2 <- compose_tooltip_language("GIO.fig2.tooltip", fig_titles, years = reactive(input$DEM_TREND_YEARS), fig = 2)
  output$DEM_FIG_NAME_3 <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.dem, fig = 3)
  output$HSE_FIG_NAME_1 <- compose_tooltip_language("GIO.fig1.tooltip", fig_titles, years = year_range.hse, fig = 1)
  output$HSE_FIG_NAME_2 <- compose_tooltip_language("GIO.fig2.tooltip", fig_titles, years = reactive(input$HSE_TREND_YEARS), fig = 2)
  output$HSE_FIG_NAME_3 <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.hse, fig = 3)
  output$HSE_FIG_NAME_3B <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.hse, fig = 3)
  output$HSE_FIG_NAME_4B <- compose_tooltip_language("GIO.fig4.tooltip", fig_titles, years = reactive(input$HSE_TREND_YEARS), fig = 4) # assign fig = 2 to show year range
  output$RSK_FIG_NAME_1   <- compose_tooltip_language("GIO.fig1.tooltip", fig_titles, years = year_range.rsk, fig = 1)
  output$RSK_FIG_NAME_2   <- compose_tooltip_language("GIO.fig2.tooltip", fig_titles, years = reactive(input$RSK_TREND_YEARS), fig = 2)
  output$RSK_FIG_NAME_3   <- compose_tooltip_language("GIO.fig3.tooltip", fig_titles, years = year_range.rsk, fig = 3)
  output$RSK_FIG_NAME_4   <- compose_tooltip_language("GIO.fig4.tooltip", fig_titles, years = reactive(input$RSK_TREND_YEARS), fig = 2)
  output$RSK_FIG_NAME_5   <- compose_tooltip_language("GIO.fig5.tooltip", fig_titles, years = year_range.rsk, fig = 5)
  output$RSK_FIG_NAME_6   <- compose_tooltip_language("GIO.fig6.tooltip", fig_titles, years = year_range.rsk, fig = 6)
  
  ## > Create DATA SOURCE INFO -------------------------------------------------
  
  ### ··· Get source for active indicator 
  current_indicator_source <- reactive({
    req(current_indicator())
    source <-
      metadata.fig.sources %>%
      filter(measure == input$MEASURE,
             indicator == current_indicator())
    return(source)
  })
  
  ### ... Render indicator data source info 
  build_data_source_container_server("DEM_DATA_SOURCE", current_indicator_source)
  build_data_source_container_server("HSE_DATA_SOURCE", current_indicator_source)
  build_data_source_container_server("RSK_DATA_SOURCE", current_indicator_source)
  
  ### ··· Format data source for figures
  current_indicator_source_fig <- get_current_indicator_source_fig("GIO.get.sources", current_indicator_source)

  
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
  }) %>% debounce(100)
  
  ### ··· Plot map for selected DEM indicator
  map.dem <- reactive({
    print("Step 1: creating DEM map")
    req(map_data.dem.subset(), base.map.geos(), list_selected.geos())
    req(nrow(map_data.dem.subset()) > 0)
    print("Step 1: creating DEM map 2")
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
    # print("Step 2: rendering DEM map")
    map.dem()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[1], "for the state of Iowa")
  })
  )
  
  ### ··· Download map for selected DEM indicator
  output$DEM_MAP_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[1], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <- 
        format_map_download(map.dem(), fig_titles, current_indicator_source_fig)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
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
  }) %>% debounce(100)
  
  ### ··· Create trend line for selected DEM indicator
  line.dem <- reactive({
    # print("Step 1: creating DEM trend")
    req(line_data.dem.subset(), current_indicator_type())
    req(nrow(line_data.dem.subset()) > 0)
    
    if (input$DEM_INDICATOR == "Child Age") {
      my_groups <- subset.dem.age
    } else {
      my_groups <- subset.dem.rac
    }
    
    # print("Step 1: creating DEM trend 2")
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
    filename = function() { paste0("IDD - ", fig_titles()$title[2], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(line.dem(), fig_titles, current_indicator_source_fig, 2)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
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
    filename = function() { paste0("IDD - ", fig_titles()$title[3], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(bar.dem(), fig_titles, current_indicator_source_fig, 3)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  

  ## > RSK indicators ----------------------------------------------------------
  ## > Create plots for RSK indicators
  
  ### ··· DATA -----------------------------------------------------------------
  
  ### ··· Read data for selected RSK indicator for Fig 1 and 2
  data.rsk.1 <- reactive({
    req(input$RSK_INDICATOR, input$GEOGRAPHY_SELECT)
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$RSK_INDICATOR,
      "Birth to Teenage Mother" =           read_my_csv("Health", geo_dir, "rsk_btm_fig1_2"),
      # "Birth to Unmarried Mother" =         read_my_csv("Health", geo_dir, "rsk_bum_fig1_2"),
      # "Inadequate Prenatal Care" =          read_my_csv("Health", geo_dir, "rsk_ipc_fig1_2"),
      # "Low Maternal Education at Birth" =   read_my_csv("Health", geo_dir, "rsk_lme_fig1_2"),
      # "Medicaid or WIC Receipt at Birth" =  read_my_csv("Health", geo_dir, "rsk_mow_fig1_2"),
      # "Preterm or Low Birth Weight" =       read_my_csv("Health", geo_dir, "rsk_plw_fig1_2"),
      # "Prenatal Tobacco Exposure" =         read_my_csv("Health", geo_dir, "rsk_pte_fig1_2"),
      "Cumulative Birth Risks" =            read_my_csv("Health", geo_dir, "rsk_cum_fig1_2")
    )
  })
  
  ### ··· Read data for selected RSK indicator for Fig 3
  data.rsk.3 <- reactive({
    # req(input$RSK_INDICATOR, input$GEOGRAPHY_SELECT) -- no need since already required by data.rsk.1
    req(data.rsk.1())
    geo_dir <- input$GEOGRAPHY_SELECT
    switch(
      input$RSK_INDICATOR,
      "Birth to Teenage Mother" =           read_my_csv("Health", geo_dir, "rsk_btm_fig3_4"),
      # "Birth to Unmarried Mother" =         read_my_csv("Health", geo_dir, "rsk_bum_fig3_4"),
      # "Inadequate Prenatal Care" =          read_my_csv("Health", geo_dir, "rsk_ipc_fig3_4"),
      # "Low Maternal Education at Birth" =   read_my_csv("Health", geo_dir, "rsk_lme_fig3_4"),
      # "Medicaid or WIC Receipt at Birth" =  read_my_csv("Health", geo_dir, "rsk_mow_fig3_4"),
      # "Preterm or Low Birth Weight" =       read_my_csv("Health", geo_dir, "rsk_plw_fig3_4"),
      # "Prenatal Tobacco Exposure" =         read_my_csv("Health", geo_dir, "rsk_pte_fig3_4"),
      "Cumulative Birth Risks" =            read_my_csv("Health", geo_dir, "rsk_cum_fig3_4")
    )
  })
  
  ### ··· Get data for selected RSK indicator
  data.rsk <- reactive({
    req(input$RSK_SUBSET, data.rsk.1)
    data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)
    # switch(
    #   input$RSK_INDICATOR,
    #   "Birth to Teenage Mother" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Birth to Unmarried Mother" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Inadequate Prenatal Care" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Low Maternal Education at Birth" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Medicaid or WIC Receipt at Birth" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Preterm or Low Birth Weight" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Prenatal Tobacco Exposure" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Cumulative Birth Risks" = 
    #     (data.rsk.1() %>% filter(subset_level == input$RSK_SUBSET))
    #   )
  })
  
  ### ··· Get data for selected RSK indicator
  data.rsk.group <- reactive({
    req(input$RSK_SUBSET, data.rsk.3())
    data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)
    # switch(
    #   input$RSK_INDICATOR,
    #   "Birth to Teenage Mother" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Birth to Unmarried Mother" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Inadequate Prenatal Care" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Low Maternal Education at Birth" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Medicaid or WIC Receipt at Birth" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Preterm or Low Birth Weight" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Prenatal Tobacco Exposure" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET)),
    #   "Cumulative Birth Risks" = 
    #     (data.rsk.3() %>% filter(subset_level == input$RSK_SUBSET))
    #   )
  })
  
  ### ··· MAP ------------------------------------------------------------------
  ### ··· Get subset of selected RSK data for map
  map_data.rsk.subset <- reactive({
    req(input$DATA_TYPE)
    req(nrow(data.rsk()) > 0)
    req(year_range.rsk())
    
    if (input$DATA_TYPE == "count") {
      my_data <-
        data.rsk() %>%
        mutate(index = count)
    } else {
      my_data <-
        data.rsk()
    }
    
    data <-
      my_data %>%
      filter(year == year_range.rsk()[[2]]) %>%
      filter(fips != "19") %>%
      mutate(fips = as.integer(fips)) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  }) %>% debounce(100)
  
  ### ··· Plot map for selected RSK indicator
  map.rsk <- reactive({
    req(map_data.rsk.subset(), base.map.geos(), list_selected.geos(), current_indicator_type())
    req(nrow(map_data.rsk.subset()) > 0)
    plot_map_view(DATA = map_data.rsk.subset(),
                  BASE_MAP = base.map.geos(),
                  LOCATIONS = list_selected.geos(),
                  DATA_TYPE = current_indicator_type(),
                  OUTLINES = input$MAP_COUNTY_OUTLINES,
                  LABELS = input$MAP_COUNTY_LABELS,
                  COL = input$RSK_MAP_COL)
  })
  
  ### ··· Render map for selected RSK indicator
  output$RSK_MAP <- renderPlot({
    map.rsk()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[1], "for the state of Iowa")
  })
  )
  
  ### ··· Download map for selected RSK indicator
  output$RSK_MAP_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[1], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <- 
        format_map_download(map.rsk(), fig_titles, current_indicator_source_fig)
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  ### ··· TREND ----------------------------------------------------------------
  ### ··· Get subset of selected RSK data for trend line
  trend_data.rsk.subset <- reactive({
    req(input$RSK_TREND_YEARS, input$DATA_TYPE)
    req(list_selected.geos(), dropdown_data.locations())
    req(nrow(data.rsk()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <-
        data.rsk() %>%
        mutate(index = count)
    } else {
      my_data <-
        data.rsk()
    }
    
    data <-
      my_data %>%
      filter(between(year, input$RSK_TREND_YEARS[1], input$RSK_TREND_YEARS[2])) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))
      )) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Render trend line for selected RSK indicator
  trend.rsk <- reactive({
    req(trend_data.rsk.subset(), current_indicator_type())
    req(nrow(trend_data.rsk.subset()) > 0)
    plot_trend_view(DATA = trend_data.rsk.subset(),
                    DATA_TYPE =  current_indicator_type(),
                    LOCATIONS = input$LOCATION_SELECT,
                    STATEWIDE = input$ADD_STATEWIDE,
                    LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render trend line for selected RSK indicator
  output$RSK_TREND <- renderPlot({
    plot.legend.format <-
      format_figure_legend_fit(session$clientData$output_RSK_TREND_width,
                               dropdown_data.locations,
                               input$LOCATION_SELECT,
                               input$ADD_STATEWIDE)
    trend.rsk() +
      guides(color = guide_legend(ncol = plot.legend.format$n.col)) +
      theme(legend.text = element_text(size = plot.legend.format$font.size))
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[2], "for the state of Iowa")
  })
  )

  
  # output$RSK_TREND_IMG <- renderImage({
  #   width <- round(session$clientData$output_RSK_TREND_width)
  #   height <- round(session$clientData$output_RSK_TREND_height)
  #   if (is.null(width) || width == 0) width <- 1000
  #   if (is.null(height) || height == 0) height <- 350
  #   dynamic_res <- 72 - (1000 - width) %/% 30
  #   outfile <- tempfile(fileext = '.png')
  #   png(outfile, width = width, height = height, res = dynamic_res)
  #   print(
  #     trend.rsk() +
  #       guides(color = guide_legend(ncol = 2))
  #   )
  #   dev.off()
  #   list(
  #     src = outfile,
  #     contentType = 'image/png',
  #     width = width,
  #     height = height,
  #     alt = paste("This plot shows", fig_titles()$title[2], "for the state of Iowa")
  #   )
  # }, deleteFile = TRUE
  # )
  
  ### ··· Download trend line for selected RSK indicator
  output$RSK_TREND_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[2], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(trend.rsk(), fig_titles, current_indicator_source_fig, 2)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  
  ### ··· LINE -----------------------------------------------------------------
  ### ··· Get subset of selected RSK data for line graph
  line_data.rsk.subset <- reactive({
    req(input$DATA_TYPE)
    req(year_range.rsk())
    req(nrow(data.rsk.group()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <-
        data.rsk.group() %>%
        mutate(index = count)
    } else {
      my_data <-
        data.rsk.group()
    }
    
    data <-
      my_data %>%
      filter(between(year, input$RSK_TREND_YEARS[1], input$RSK_TREND_YEARS[2])) %>%
      filter(fips %in% 19) %>%
      mutate(group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic"))) %>%
      mutate(index = ifelse(index == -9999, NA_real_, index))
    return(data)
  })
  
  ### ··· Render line chart for fig 4
  line.rsk <- reactive({
    req(line_data.rsk.subset(), current_indicator_type())
    req(nrow(line_data.rsk.subset()) > 0)
    plot_line_view(DATA = line_data.rsk.subset(),
                   DATA_TYPE =  current_indicator_type(),
                   LOCATIONS = 19,
                   STATEWIDE = TRUE,
                   GROUPS = c("White", "Black", "Asian", "Other", "Hispanic"),
                   LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render line chart for fig 4
  output$RSK_LINE <- renderPlot({
    line.rsk()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[4], "for the state of Iowa")
  })
  )
  
  ### ··· Download line chart for fig 4
  output$RSK_LINE_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[4], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(line.rsk(), fig_titles, current_indicator_source_fig, 4)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # filename = "cip.png", #file_name(),
    # content = function(file){
    #   ggsave(file,
    #          plot = line.rsk() +
    #            labs(title = paste(fig_titles()$title[4]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "line chart") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ),
    #          width = 10, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
  )
  
  
  ### ··· BAR ------------------------------------------------------------------
  ### ··· Get subset of selected RSK data for bar chart
  bar_data.rsk.subset <- reactive({
    req(input$DATA_TYPE)
    req(year_range.rsk(), list_selected.geos(), dropdown_data.locations())
    req(nrow(data.rsk.group()) > 0)
    
    if (input$DATA_TYPE == "count") {
      my_data <-
        data.rsk.group() %>%
        mutate(index = count)
    } else {
      my_data <-
        data.rsk.group()
    }
    
    data <-
      my_data %>%
      filter(year == year_range.rsk()[[2]]) %>%
      filter(fips %in% list_selected.geos()) %>%
      mutate(fips = factor(fips,
                           levels = c("Statewide" = 19, dropdown_data.locations()),
                           labels = names(c("Statewide" = 19, dropdown_data.locations()))),
             group = "none") %>%
      mutate(group = ifelse(str_detect(group_level, "(H|h)ispanic"), "Ethnicity", "Race"),
             group = factor(group, levels = c("Race", "Ethnicity")),
             group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic")))
    
    return(data)
  })
  
  ## ··· Render bar chart for selected RSK indicator
  bar.rsk <- reactive({
    req(bar_data.rsk.subset(), current_indicator_type())
    req(nrow(bar_data.rsk.subset()) > 0)
    plot_bar_view(DATA = bar_data.rsk.subset(),
                  DATA_TYPE =  current_indicator_type(),
                  LOCATIONS = input$LOCATION_SELECT,
                  STATEWIDE = input$ADD_STATEWIDE,
                  LABELS = input$ADD_VALUE_LABELS,
                  FACET = TRUE)
  })
  
  ### ··· Render bar chart for selected RSK indicator
  output$RSK_BAR <- renderPlot({
    plot.legend.format <-
      format_figure_legend_fit(session$clientData$output_RSK_BAR_width,
                               dropdown_data.locations,
                               input$LOCATION_SELECT,
                               input$ADD_STATEWIDE)
    bar.rsk() +
      guides(fill = guide_legend(ncol = plot.legend.format$n.col)) +
      theme(legend.text = element_text(size = plot.legend.format$font.size))
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[3], "for the state of Iowa")
  })
  )
  
  ### ··· Download chart for selected RSK indicator
  output$RSK_BAR_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[3], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(bar.rsk(), fig_titles, current_indicator_source_fig, 3)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
  )
  
  
  ### ··· STACKED BAR ----------------------------------------------------------
  ### ··· Get data for statewide Cumulative Birth Risk 
  data.rsk.cum <- reactive({
    req(input$DATA_TYPE)
    req(data.rsk.3())
    req(input$RSK_INDICATOR == "Cumulative Birth Risks")
    
    my_data <-
      data.rsk.3() %>%
      filter(fips == 19) %>%
      filter(subset_level != "3 or More Risks") %>%
      filter(year == year_range.rsk()[[2]]) %>%
      mutate(group = ifelse(str_detect(group_level, "(H|h)ispanic"), "Ethnicity", "Race"),
             group = factor(group, levels = c("Race", "Ethnicity")),
             group_level = factor(group_level, levels = c("White", "Black", "Asian", "Other", "Hispanic")))
    
    if (input$DATA_TYPE == "count") {
      data <-
        my_data %>%
        mutate(index = count)
    } else {
      data <-
        my_data 
    }
    
    return(data)
    
  })
  
  ## ··· Render stacked bar chart for selected RSK indicator
  bar.stacked.rsk <- reactive({
    req(data.rsk.cum(), current_indicator_type())
    req(nrow(data.rsk.cum()) > 0)
    plot_bar_stacked_view(DATA = data.rsk.cum(),
                          DATA_TYPE =  current_indicator_type(),
                          LABELS = input$ADD_VALUE_LABELS,
                          FACET = TRUE)
  })
  
  ### ··· Render stacked bar chart for selected RSK indicator
  output$RSK_BAR_STACKED <- renderPlot({
    bar.stacked.rsk()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[5], "for the state of Iowa")
  })
  )
  
  ### ··· Download stacked bar chart for selected RSK indicator
  output$RSK_BAR_STACKED_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[5], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(bar.stacked.rsk(), fig_titles, current_indicator_source_fig, 5)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # filename = "cip.png", #file_name(),
    # content = function(file){
    #   ggsave(file,
    #          plot = bar.stacked.rsk() +
    #            labs(title = paste(fig_titles()$title[5]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "bar chart") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ),
    #          width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
  )
  
  ### ··· PIE CHART ------------------------------------------------------------
  ## ··· Render stacked bar chart for selected RSK indicator
  pie.rsk <- reactive({
    req(data.rsk.cum(), current_indicator_type())
    req(nrow(data.rsk.cum()) > 0)
    plot_pie_view(DATA = data.rsk.cum(), LABELS = input$ADD_VALUE_LABELS)
  })
  
  ### ··· Render stacked bar chart for selected RSK indicator
  output$RSK_PIE <- renderPlot({
    pie.rsk()
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[6], "for the state of Iowa")
  })
  )
  
  ### ··· Download stacked bar chart for selected RSK indicator
  output$RSK_PIE_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[6], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(pie.rsk(), fig_titles, current_indicator_source_fig, 6)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # filename = "cip.png", #file_name(),
    # content = function(file){
    #   ggsave(file,
    #          plot = pie.rsk() +
    #            labs(title = paste(fig_titles()$title[6]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "bar chart") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ),
    #          width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
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
      # "Working Parents" =           read_my_csv(dim_dir, geo_dir, "hse_wrk_fig1_2_3"),
      # "First Time Mother" =         read_my_csv(dim_dir, geo_dir, "hse_ftm_fig1_2"),
      "Language Spoken at Home" =   read_my_csv(dim_dir, geo_dir, "hse_lng_fig1_2"),
      "Technology Access" =         read_my_csv(dim_dir, geo_dir, "hse_tch_fig1_2_3"),
      # "Transportation" =            read_my_csv(dim_dir, geo_dir, "hse_trn_fig1_2"),
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
      # "Working Parents" =           data.hse.1(),
      # "First Time Mother" =         read_my_csv(dim_dir, geo_dir, "hse_ftm_fig3_4"),
      "Language Spoken at Home" =   read_my_csv(dim_dir, geo_dir, "hse_lng_fig3"),
      "Technology Access" =         data.hse.1(),
      # "Transportation" =            read_my_csv(dim_dir, geo_dir, "hse_trn_fig3"),
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
  }) %>% debounce(100)
  
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
    filename = function() { paste0("IDD - ", fig_titles()$title[1], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <- 
        format_map_download(map.hse(), fig_titles, current_indicator_source_fig)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
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
    plot.legend.format <-
      format_figure_legend_fit(session$clientData$output_HSE_TREND_width,
                               dropdown_data.locations,
                               input$LOCATION_SELECT,
                               input$ADD_STATEWIDE)
    trend.hse() +
      guides(color = guide_legend(ncol = plot.legend.format$n.col)) +
      theme(legend.text = element_text(size = plot.legend.format$font.size))
  },
  alt = reactive({
    paste("This plot shows", fig_titles()$title[2], "for the state of Iowa")
  })
  )
  
  ### ··· Download trend line for selected HSE indicator
  output$HSE_TREND_DOWNLOAD <- downloadHandler(
    filename = function() { paste0("IDD - ", fig_titles()$title[2], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(trend.hse(), fig_titles, current_indicator_source_fig, 2)
      # Set arguments for downloaded figure 
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
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
    filename = function() { paste0("IDD - ", fig_titles()$title[4], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(trend.hse.ftm(), fig_titles, current_indicator_source_fig, 4)
      # Set arguments for downloaded figure
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # 
    # content = function(file){
    #   ggsave(file, 
    #          plot = trend.hse.ftm() +
    #            labs(title = paste(fig_titles()$title[4]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "trend line") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ), 
    #          width = 10, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
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
    filename = function() { paste0("IDD - ", fig_titles()$title[3], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(bar.hse(), fig_titles, current_indicator_source_fig, 3)
      # Set arguments for downloaded figure
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # 
    # content = function(file){
    #   ggsave(file, 
    #          plot = bar.hse() +
    #            labs(title = paste(fig_titles()$title[3]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "bar chart") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ), 
    #          width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
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
    filename = function() { paste0("IDD - ", fig_titles()$title[3], ".png") },
    content  = function(file){
      # Add source info to map and some styling
      download_fig <-
        format_figs_download(bar.hse(), fig_titles, current_indicator_source_fig, 3)
      # Set arguments for downloaded figure
      ggsave(file, 
             plot = download_fig,
             width = 10, height = 8, scale = 1.25, dpi = 150, bg = "white")
    }
    # content = function(file){
    #   ggsave(file, 
    #          plot = bar.hse() +
    #            labs(title = paste(fig_titles()$title[3]),
    #                 # subtitle = "subtitle goes here", caption = "this is caption", tag = "ECI Indicators from IDD",
    #                 alt = "bar chart") +
    #            theme(
    #              plot.title = element_text(size = 20, face = "bold", hjust = 0.45, vjust = 0.5)
    #            ), 
    #          width = 12, height = 6, scale = 1.25, dpi = 150, bg = "white")
    # }
  )

  
  ### ............. TESTING SPACE .................. -----------------------
  
  # output$TEXT <- renderPrint({
  #   str(dropdown_data.locations())
  # })
  
  # output$TEXT1 <- renderText({
  #   paste("input$DATA_TYPE = ", input$DATA_TYPE, " Nrow = ", nrow(map_data.hse.subset()), " GEO List = ", input$HSE_MAP_COL)
  #   # nrow(map_data.hse.subset())
  # })
  # output$TABLE1 <- renderTable({
  #   dropdown_data.locations()
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
