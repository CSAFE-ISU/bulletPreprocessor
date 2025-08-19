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
source("R/grooves.R")
source("R/land-scan.R")
source("R/signal.R")
source("R/sliders.R")
source("R/upload.R")


# Increase maximum file upload size
options(shiny.maxRequestSize = app_config$file_params$max_file_size)

# TRUE prevents popups. FALSE allows popups.
options(rgl.useNULL = app_config$display_params$rgl_popups_null)


# Server ----
server <- function(input, output, session) {
  
  # Reactive object to store land data ----
  land <- reactiveValues(
    barrel = NULL,  # barrel name
    bullet = NULL,  # bullet name
    crosscut = NULL,  # crosscut y value
    ccdata = NULL,  # data frame of crosscut x-y coordinates and surface height
    df = NULL,  # data frame of source and x3p from read_bullet()
    grooves = NULL,  # named vector of left and right groove x values
    land = NULL,  # land name
    resolution = NULL,  # resolution of x3p scan
    sigs = NULL,  # data frame of signal info
    study = app_config$ui_params$default_study,  # name of bullet study
    x3p_dims = NULL  # dimensions of x3p
  )
  
  # Reactive object to store button status: TRUE = on, FALSE = OFF ----
  buttons <- reactiveValues(
    crosscut = FALSE,
    crosscut_slider = FALSE,
    save_crosscut = FALSE,
    grooves = FALSE,
    save_grooves = FALSE,
    grooves_sliders = FALSE,
    signal = FALSE,
    download = FALSE
  )
  
  # Load land ----
  uploadServer("upload1", land, buttons)

  # Display land ----
  landScanServer(
    id = "land_scan1", 
    land_rv = land, 
    crosscut_value = reactive({ifelse(is.null(crosscut_value), NA, crosscut_value())})
  ) 
  
  # Crosscut ----
  crosscut_value <- crosscutServer(
    id = "crosscut1", 
    land_rv = land, 
    buttons_rv = buttons
  )
  
  # Grooves ----
  groovesServer(
    "grooves1",
    land_rv = land, 
    buttons_rv = buttons, 
    main_session = session
  )
  
  # Signal ----
  signalServer(
    "signal1",
    land_rv = land, 
    buttons_rv = buttons, 
    main_session = session
  )
  
  # Download ----
  downloadServer(
    "download1",
    land_rv = land, 
    buttons_rv = buttons
  )

}
