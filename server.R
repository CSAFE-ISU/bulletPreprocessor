library(bslib)
library(bulletxtrctr)
library(dplyr)
library(ggplot2)
library(rgl)
library(shinyjs)
library(x3ptools)

source("R/helpers.R")
source("R/land-scan.R")
source("R/upload.R")


# Increase maximum upload size
options(shiny.maxRequestSize = 150*1024^2)

# Force RGL to use null device to prevent pop-ups
options(rgl.useNULL = TRUE)


# Server ----
server <- function(input, output) {
  
  # Reactive object to store bullet data ----
  land <- reactiveValues(
    df = NULL,
    barrel_name = NULL,
    bullet_name = NULL,
    land_name = NULL,
    crosscut = NULL
  )
  
  buttons <- reactiveValues(
    crosscut = FALSE,
    grooves = FALSE
  )
  
  # Read bullet ----
  uploadServer("upload1", land, buttons)

  # Display land ----
  landScanServer("land_scan1", land) 
  
  # Crosscut ----
  crosscutServer("crosscut1", land, buttons)
  
  # Grooves ----
  groovesServer("grooves1", land, buttons)

}