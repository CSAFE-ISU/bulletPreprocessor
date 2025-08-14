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
    barrel_name = NULL,
    bullet_name = NULL,
    crosscut = NULL,
    crosscut_df = NULL,
    df = NULL,
    grooves = NULL,
    land_name = NULL,
    x3p_dims = NULL
  )
  
  # Reactive object to store button status: TRUE = on, FALSE = OFF ----
  buttons <- reactiveValues(
    crosscut = FALSE,
    grooves = FALSE
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
  signalServer("signal1", land)

}