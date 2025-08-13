library(bslib)
library(bulletxtrctr)
library(ggplot2)
library(rgl)
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
  
  # Read bullet ----
  uploadServer("upload1", land)

  # Render land ----
  landScanServer("land_scan1", land) 
  
  # Crosscut ----
  crosscutServer("crosscut1", land)
  
  # Get default grooves ----
  observeEvent(input$default_grooves_button, {
    land$grooves <- cc_locate_grooves(land$df$ccdata[[1]], method = "middle", adjust = 30, return_plot = FALSE)
    land$left_groove <- land$grooves[[1]][1]
    land$right_groove <- land$grooves[[1]][2]
  })
  
  # 
  output$grooves_verbatim <- renderPrint({
    req(land$grooves)
    land$left_groove
  })
  
  
}