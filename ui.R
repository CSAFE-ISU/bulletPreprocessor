library(bslib)
library(shiny)
library(shinyFiles)
library(shinyjs)

source("R/config.R")
source("R/crosscut-grooves.R")
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
    
    uploadUI("upload1"),
    resetUI("reset1"),
    crosscutGroovesUI("crosscut1"),
    signalUI("signal1"),
    downloadUI("download1")

  ),
  
  navset_card_underline(
    title = "Visualizations",
    id = "main_tabs",
    nav_panel("Land with Crosscut and Grooves", landScanUI("land_scan1")),
    nav_panel("Signal", signalTabUI("signal1"))
  )
 
)
