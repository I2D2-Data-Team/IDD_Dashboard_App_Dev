
# BUILD STATEWIDE SUMMARY -------------------------------------------------

statewide_summary_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("STATEWIDE_AVE"))
  )
}

statewide_summary_Server <- function(id, DATA) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$STATEWIDE_AVE <-
        renderUI({
          req(DATA())
          ns <- session$ns
          div(class = "d-grid gap-2", 
              style = "background-color:LightGray; left: 0px; width:80%; padding: 10px;",
              h5(strong("Statewide Indicator"), style = "text-align: center;",),
              fluidRow(
                column(width = 6, style = "text-align: right;",
                       h5(DATA()$year, "Average:")
                ),
                column(width = 6, style = "text-align: left;",
                       h5(strong(DATA()$label_index))
                )
              ),
              fluidRow(
                column(width = 6, style = "text-align: right;", 
                       h5((DATA()$year - 1), "-", DATA()$year, "Change:")
                ),
                column(width = 6, style = "text-align: left;",
                       h5(strong(DATA()$label_change, class = DATA()$text_color))
                )
              )
          )
        })
      
    }
  )
}
