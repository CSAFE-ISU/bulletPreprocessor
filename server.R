library(bslib)
library(bulletxtrctr)
library(dplyr)
library(ggplot2)
library(rgl)
library(shinyFiles)
library(shinyjs)
library(stringr)
library(x3ptools)

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


# Increase maximum file upload size ----
options(shiny.maxRequestSize = app_config$file_params$max_file_size)

# TRUE prevents popups. FALSE allows popups ----
options(rgl.useNULL = app_config$display_params$rgl_popups_null)


# Server ----
server <- function(input, output, session) {
  
  # Reactive object to store land data ----
  land <- reactiveValues(
    barrel = NULL,  # barrel name
    bullet = NULL,  # bullet name
    crosscut = NULL,  # crosscut y value
    left_scan = NULL,  # left groove location
    right_scan = NULL,  # right groove location
    ccdata = NULL,  # data frame of crosscut x-y coordinates and surface height
    df = NULL,  # data frame of source and x3p from read_bullet()
    grooves = NULL,  # named vector of left and right groove x values
    left_groove = NULL, # left groove x value. Easier to access than grooves[[1]][1]
    right_groove = NULL, # right groove x value. Easier to access than grooves[[1]][2]
    land = NULL,  # land name
    resolution = NULL,  # resolution of x3p scan
    sigs = NULL,  # data frame of signal info
    study = NULL,  # name of bullet study
    x3p_dims = NULL,  # dimensions of x3p
    output_df = NULL  # data frame for download
  )
  
  # Reactive object to store button status: TRUE = on, FALSE = OFF ----
  buttons <- reactiveValues(
    upload = TRUE,
    signal = FALSE,
    reset = FALSE
  )
  
  # Load land ----
  uploadServer("upload1", land, buttons)

  # Display land ----
  landScanServer("land_scan1", land, buttons) 
  
  # Crosscut and Grooves ----
  crosscutServer("crosscut1", land, buttons)
  
  # Signal ----
  signalServer("signal1", land, buttons, main_session = session)
  
  # Download ----
  downloadServer("download1", land, buttons)
  
  # Reset ----
  resetServer("reset1", land, buttons, main_session = session)

}
