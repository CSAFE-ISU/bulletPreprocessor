slidersUI <- function(id) {
  tagList(
    uiOutput(NS(id, "slider")),
  )
}

slidersServer <- function(id, land_rv, arg_name, label, max_value) {
  moduleServer(id, function(input, output, session) {
    
    # Update value in land reactive values ----
    observeEvent(input$slider, {
      land_rv[[arg_name]] <- input$slider
    })
    
    # Decrease button ----
    observeEvent(input$decrease_button, {
      current_val <- input$slider
      proposed_val <- current_val - app_config$ui_params$slider_step_size
      new_val <- max(0, proposed_val)  # Ensure we don't go below minimum
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Large decrease button ----
    observeEvent(input$large_decrease_button, {
      current_val <- input$slider
      new_val <- max(0, current_val - app_config$ui_params$slider_large_step)  # Ensure we don't go below minimum
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Increase button ----
    observeEvent(input$increase_button, {
      current_val <- input$slider
      new_val <- min(max_value, current_val + app_config$ui_params$slider_step_size)  # Ensure we don't go above maximum
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Large increase button ----
    observeEvent(input$large_increase_button, {
      current_val <- input$slider
      new_val <- min(max_value, current_val + app_config$ui_params$slider_large_step)  # Ensure we don't go above maximum
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Display slider ----
    output$slider <- renderUI({
      # Only render slider valid values exist ----
      if (any(is.null(max_value), 
              is.null(land_rv[[arg_name]]), 
              is.na(max_value), 
              is.na(land_rv[[arg_name]]))) {
        return(NULL)
      }
      
      # renderUI requires tagList to render multiple inputs
      tagList(
        br(),
        sliderInput(
          inputId = session$ns("slider"), # Important: use session$ns() to namespace the ID" 
          label = label,
          min = 0, 
          max = max_value, 
          value = land_rv[[arg_name]]
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          actionButton(session$ns("large_decrease_button"), "<<", class = "increase-decrease-button"),
          actionButton(session$ns("decrease_button"), "<", class = "increase-decrease-button"),
          actionButton(session$ns("increase_button"), ">", class = "increase-decrease-button"),
          actionButton(session$ns("large_increase_button"), ">>", class = "increase-decrease-button")
        )
      )
    })
    
  })
}
