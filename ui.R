library(bslib)
library(shiny)
library(shinyFiles)
library(shinyjs)

source("R/crosscut.R")
source("R/display.R")
source("R/grooves.R")
source("R/land-scan.R")
source("R/signal.R")
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
    accordion(
      multiple = FALSE,
      open = FALSE,
      accordion_panel(
        "Get Started",
        uploadUI("upload1")
      ),
      accordion_panel(
        "Crosscut",
        crosscutUI("crosscut1")
      ),
      accordion_panel(
        "Grooves",
        groovesUI("grooves1"),
      ),
      accordion_panel(
        "Signal",
        signalUI("signal1")
      ),
      accordion_panel(
        "Download",
        downloadUI("download1")
      )
    ),
    resetUI("reset1")
    
  ),
  
  navset_card_underline(
    title = "Visualizations",
    id = "main_tabs",
    nav_panel("Land with Crosscut", landScanUI("land_scan1")),
    nav_panel("Grooves", groovesTabUI("grooves1")),
    nav_panel("Signal", signalTabUI("signal1"))
  )
 
)