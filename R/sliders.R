# Slider Module UI
sliderUI <- function(id, label = "Select Value") {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("slider"),
      label = label,
      min = 0,
      max = 1000,  # This will be updated in the server function
      value = 0  # This will be updated in the server function
    ),
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      actionButton(ns("large_decrease_button"), "<<", class = "increase-decrease-button"),
      actionButton(ns("decrease_button"), "<", class = "increase-decrease-button"),
      actionButton(ns("increase_button"), ">", class = "increase-decrease-button"),
      actionButton(ns("large_increase_button"), ">>", class = "increase-decrease-button")
    )
  )
}

# Slider Module Server - Updated to handle reactive initial values
sliderServer <- function(id, initial_val, max_val) {
  moduleServer(id, function(input, output, session) {
    
    # Update slider initial and max values ----
    observe({
      
      init_val <- if(is.reactive(initial_val)) {
        initial_val() 
      } else {
        initial_val
      }
      
      max_val <- if(is.reactive(max_val)) {
        max_val() 
      } else {
        max_val
      }
      
      # Update if initial_val is not NULL/NA ----
      if (!is.null(init_val) && !is.na(init_val)) {
        updateSliderInput(
          session = session,
          inputId = "slider",
          max = max_val,
          value = init_val
        )
      }
    })
    
    # Decrease button ----
    observeEvent(input$decrease_button, {
      new_val <- change_slider_value(
        current_val = input$slider,
        step = -app_config$ui_params$slider_step_size,
        max_val = max_val
      )
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Large decrease button ----
    observeEvent(input$large_decrease_button, {
      new_val <- change_slider_value(
        current_val = input$slider,
        step = -app_config$ui_params$slider_large_step,
        max_val = max_val
      )
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Increase button ----
    observeEvent(input$increase_button, {
      new_val <- change_slider_value(
        current_val = input$slider,
        step = app_config$ui_params$slider_step_size,
        max_val = max_val
      )
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Large increase button ----
    observeEvent(input$large_increase_button, {
      new_val <- change_slider_value(
        current_val = input$slider,
        step = app_config$ui_params$slider_large_step,
        max_val = max_val
      )
      updateSliderInput(session, "slider", value = new_val)
    })
    
    # Return the reactive slider value
    return(reactive({ input$slider }))
  })
}
