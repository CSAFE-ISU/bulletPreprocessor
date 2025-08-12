library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  title = "BulletPreprocessor",
  
  # Sidebar ----
  sidebar = sidebar(
    
    # Upload bullet land ----
    fileInput("land_upload", "Upload a land", accept = ".x3p", multiple = TRUE),
    textOutput("barrel_name"),
    textOutput("bullet_name"),
    textOutput("land_name")
    
  ),
  
  uiOutput("land_display")
)