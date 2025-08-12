library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  title = "BulletPreprocessor",
  
  # Sidebar ----
  sidebar = sidebar(
    
    # Upload bullet land ----
    fileInput("land_upload", "Upload a land", accept = ".x3p", multiple = TRUE),
    accordion(
      accordion_panel(
        "Crosscut",
        actionButton("default_crosscut_button", "Get default crosscut"),
        uiOutput("crosscutUI")
      )
    )
    
  ),
  
  uiOutput("land_display"),
  uiOutput("crosscut_table_display")
)