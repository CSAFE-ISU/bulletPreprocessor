crosscutUI <- function(id) {
  tagList(
    actionButton(NS(id, "default_crosscut_button"), "Get default crosscut"),
    uiOutput(NS(id, "crosscutUI"))
  )
}

crosscutServer <- function(id, land_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Get default crosscut ----
    observeEvent(input$default_crosscut_button, {
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% x3p_crosscut_optimize(ylimits = c(150, NA))
    })
    
    # Update crosscut on land display with slider ----
    observeEvent(input$crosscut_slider, {
      land_rv$crosscut <- input$crosscut_slider
    })
    
    # Store crosscut data ----
    observeEvent(input$finalize_crosscut_button, {
      # Use lapply to place crosscut data frame in a list so it can be assigned to a column
      land_rv$df$ccdata <- lapply(land_rv$df$x3p, function(x) x3p_crosscut(x, y = land_rv$crosscut))
    })
    
    # Display crosscut slider ----
    output$crosscutUI <- renderUI({
      req(land_rv$crosscut)
      
      # Store land dimensions
      land_rv$x3p_dims <- dim(land_rv$df$x3p[[1]])
      
      # renderUI requires tagList to render multiple inputs
      tagList(
        br(),
        sliderInput(
          inputId = session$ns("crosscut_slider"), # Important: use session$ns() to namespace the ID" 
          label = "Crosscut location",
          min = 0, 
          max = land_rv$x3p_dims[2], 
          value = land_rv$crosscut
        ),
        actionButton(inputId = session$ns("finalize_crosscut_button"), 
                     label = "Finalize crosscut")
      )
    })
    
  })
}