library(shiny)
library(bslib)
library(bsicons)
library(waiter)

# Set shiny head settings
load_shiny_dashboard_setting <- function() {
  tagList(
    tags$head(
      # Measure shiny performance in a console, see: https://github.com/Appsilon/shiny.tictoc
      tags$script(src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"),
      
      # Add CSS styling
      includeCSS("https://raw.githubusercontent.com/I2D2-Data-Team/IDD_Dashboard_App_Dev/refs/heads/main/common/www/style.css"),
      
      
      # Add favicon to browser
      tags$head(
        tags$link(rel = "shortcut icon", href = "https://i2d2.iastate.edu/wp-content/uploads/2020/08/favicon-96x96-1.png")
      ),
      
      # Add loader to the whole dashboard and set style for individual figures
      useWaiter(),
      waiterPreloader(html = spin_5(), color = "grey")
    )
  )
}
