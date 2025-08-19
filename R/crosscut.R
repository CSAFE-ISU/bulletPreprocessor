crosscutUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("crosscut_button"), "Get crosscut"),
    slidersUI(ns("crosscut_slider"))
  )
}

crosscutServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Disable crosscut button when app starts ----
    disable("crosscut_button")
    
    # Switch the crosscut button on or off ----
    observe({
      if (buttons_rv$crosscut) {
        enable("crosscut_button")
      } else {
        disable("crosscut_button")
      }
    })
    
    # Display and update the crosscut ----
    observeEvent(input$crosscut_button, {
      req(land_rv$df)
      
      # Get default crosscut ----
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% 
        x3p_crosscut_optimize(ylimits = app_config$proc_params$crosscut_ylimits)
      
      # Crosscut slider ----
      slidersServer(
        id = "crosscut_slider", 
        land_rv = land_rv,
        arg_name = "crosscut", 
        label = "Crosscut",
        max_value = land_rv$x3p_dims[2]
      )
      
      # Enable grooves button
      buttons_rv$grooves <- TRUE
      
      showNotification(
        "Starting crosscut location found. Adjust with slider if needed.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    })
    
  })
}
