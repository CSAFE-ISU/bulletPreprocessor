# Slider Module UI
sliderModuleUI <- function(id, label = "Select Value") {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("slider"),
      label = label,
      min = 0,
      max = 1000,  # This will be updated in the server function
      value = 0  # This will be updated in the server function
    )
  )
}

# Slider Module Server - Updated to handle reactive initial values
sliderModuleServer <- function(id, initial_value, max_value) {
  moduleServer(id, function(input, output, session) {
    # Update slider with provided parameters when they change
    observe({
      
      init_val <- if(is.reactive(initial_value)) {
        initial_value() 
      } else {
        initial_value
      }
      max_val <- if(is.reactive(max_value)) {
        max_value() 
      } else {
        max_value
      }
      
      # Only update if initial_value is not NULL/NA
      if (!is.null(init_val) && !is.na(init_val)) {
        updateSliderInput(
          session = session,
          inputId = "slider",
          max = max_val,
          value = init_val
        )
      }
    })
    
    # Return the reactive slider value
    return(reactive({ input$slider }))
  })
}

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
