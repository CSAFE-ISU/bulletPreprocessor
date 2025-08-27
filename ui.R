library(bslib)
library(shiny)
library(shinyFiles)
library(shinyjs)

source("R/config.R")
source("R/crosscut.R")
source("R/display.R")
source("R/download.R")
source("R/profile.R")
source("R/helpers.R")
source("R/land-scan.R")
source("R/reset.R")
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
      id = "acc",
      multiple = FALSE,
      open = FALSE,
      accordion_panel(
        "Get Started",
        uploadUI("upload1")
      ),
      accordion_panel(
        "Crosscut and Grooves",
        crosscutUI("crosscut1")
      ),
      accordion_panel(
        "Profile",
        profileUI("profile1"),
      ),
      accordion_panel(
        "Signal",
        signalUI("signal1")
      )
    ),
    downloadUI("download1"),
    resetUI("reset1")
    
  ),
  
  navset_card_underline(
    title = "Visualizations",
    id = "main_tabs",
    nav_panel("Land with Crosscut and Grooves", landScanUI("land_scan1")),
    nav_panel("Profile", profileTabUI("profile1")),
    nav_panel("Signal", signalTabUI("signal1"))
  )
 
)
