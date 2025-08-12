library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  title = "BulletPreprocessor",
  
  # Sidebar ----
  sidebar = sidebar(
    
    # Upload bullet land ----
    fileInput("land_upload", "Upload a land", accept = ".x3p", multiple = TRUE),
    
  ),
  
  uiOutput("land_display")
)