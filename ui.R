library(bslib)
library(shiny)
library(shinyjs)

source("R/crosscut.R")
source("R/display.R")
source("R/grooves.R")
source("R/land-scan.R")
source("R/sliders.R")
source("R/upload.R")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  useShinyjs(),
  
  includeCSS("www/styles.css"),
  
  title = "BulletPreprocessor",
  
  # Sidebar ----
  sidebar = sidebar(
    
    # Upload bullet land ----
    uploadUI("upload1"),
    accordion(
      accordion_panel(
        "Crosscut",
        crosscutUI("crosscut1")
      ),
      accordion_panel(
        "Grooves",
        groovesUI("grooves1"),
      )
    )
    
  ),
  
  navset_card_underline(
    title = "Visualizations",
    id = "main_tabs",
    nav_panel("Land with Crosscut", landScanUI("land_scan1")),
    nav_panel("Grooves", groovesTabUI("grooves1"))
  )
 
)