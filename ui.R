library(shiny)
library(bslib)

source("R/land-scan.R")
source("R/upload.R")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  title = "BulletPreprocessor",
  
  # Sidebar ----
  sidebar = sidebar(
    
    # Upload bullet land ----
    uploadUI("upload1"),
    accordion(
      accordion_panel(
        "Crosscut",
        actionButton("default_crosscut_button", "Get default crosscut"),
        uiOutput("crosscutUI")
      ),
      accordion_panel(
        "Grooves",
        actionButton("default_grooves_button", "Get default grooves"),
      )
    )
    
  ),
  
  navset_card_underline(
    title = "Visualizations",
    nav_panel("Land with Crosscut", landScanUI("land_scan1")),
    nav_panel("Crosscut Data", uiOutput("crosscut_table_display")),
    nav_panel("Grooves", verbatimTextOutput("grooves_verbatim"))
  )
 
)