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
  
  # Get default crosscut ----
  observeEvent(input$default_crosscut_button, {
    land$crosscut <- land$df$x3p[[1]] %>% x3p_crosscut_optimize(ylimits = c(150, NA))
  })
  
  # Update crosscut on land display with slider ----
  observeEvent(input$crosscut_slider, {
    land$crosscut <- input$crosscut_slider
  })
  
  # Store crosscut data
  observeEvent(input$finalize_crosscut_button, {
    # Use lapply to place crosscut data frame in a list so it can be assigned to a column
    land$df$ccdata <- lapply(land$df$x3p, function(x) x3p_crosscut(x, y = land$crosscut))
  })
  
  # Display crosscut data after finalized ----
  output$final_crosscut_df <- renderTable({
    req("ccdata" %in% colnames(land$df))
    land$df$ccdata[[1]]
  })

  # Display crosscut slider ----
  output$crosscutUI <- renderUI({
    req(land$crosscut)
    
    # Store land dimensions
    land$x3p_dims <- dim(land$df$x3p[[1]])
    
    # renderUI requires tagList to render multiple inputs
    tagList(
      br(),
      sliderInput(
        inputId = "crosscut_slider", 
        label = "Crosscut location",
        min = 0, 
        max = land$x3p_dims[2], 
        value = land$crosscut
      ),
      actionButton(inputId = "finalize_crosscut_button", label = "Finalize crosscut")
    )
  })
  
  # Display crosscut table in card ----
  output$crosscut_table_display <- renderUI({
    req(!is.null(land$df) & "ccdata" %in% colnames(land$df))
    make_table_card(table_id = "final_crosscut_df", header_title = "Crosscut Data")
  })
  
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