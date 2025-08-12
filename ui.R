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
        actionButton("crosscut", "Get default crosscut"),
        verbatimTextOutput("crosscut")
      )
    )
    
  ),
  
  uiOutput("land_display")
)