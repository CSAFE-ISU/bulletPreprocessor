library(bslib)
library(bulletxtrctr)
library(dplyr)
library(ggplot2)
library(rgl)
library(shinyFiles)
library(shinyjs)
library(stringr)
library(x3ptools)

source("R/crosscut.R")
source("R/display.R")
source("R/grooves.R")
source("R/land-scan.R")
source("R/signal.R")
source("R/sliders.R")
source("R/upload.R")


# Increase maximum file upload size
options(shiny.maxRequestSize = 150*1024^2)

# Force RGL to use null device to prevent pop-ups
options(rgl.useNULL = TRUE)


# Server ----
server <- function(input, output, session) {
  
  # Reactive object to store land data ----
  land <- reactiveValues(
    barrel = NULL,  # barrel name
    bullet = NULL,  # bullet name
    crosscut = NULL,  # crosscut y value
    ccdata = NULL,  # data frame of crosscut x-y coordinates and surface height
    df = NULL,  # data frame of source and x3p from read_bullet()
    folder = NULL,  # filepath to bullet folder in bullet_replicate_results. Will always be NULL in app because filepath is to temp directory.
    grooves = NULL,  # named vector of left and right groove x values
    land = NULL,  # land name
    resolution = NULL,  # resolution of x3p scan
    sigs = NULL,  # data frame of signal info
    source = NULL,  # filepath of x3p file in bullet_replicate_results. Will always be NULL in app because filepath is to temp directory.
    study = NULL,  # name of bullet study
    x3p_dims = NULL  # dimensions of x3p
  )
  
  # Reactive object to store button status: TRUE = on, FALSE = OFF ----
  buttons <- reactiveValues(
    crosscut = FALSE,
    grooves = FALSE,
    signal = FALSE
  )
  
  # Load land ----
  uploadServer("upload1", land, buttons)

  # Display land ----
  landScanServer("land_scan1", land) 
  
  # Crosscut ----
  crosscutServer("crosscut1", land, buttons)
  
  # Grooves ----
  groovesServer("grooves1", land, buttons, main_session = session)
  
  # Download ----
  signalServer("signal1", land, buttons, main_session = session)

}