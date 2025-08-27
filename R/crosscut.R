crosscutUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("crosscut_button"), "Get crosscut and grooves"),
    slidersUI(ns("crosscut_slider")),
    slidersUI(ns("left_scan_slider")),
    slidersUI(ns("right_scan_slider"))
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
      req(land_rv$df$x3p)
      req(land_rv$x3p_dims[2])
      
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
      
      # Get crosscut profile ----
      land_rv$ccdata <- x3p_crosscut(x = land_rv$df$x3p[[1]], y = land_rv$crosscut)
      
      # Get default grooves ----
      # Store left and right grooves individually to make them easier to update
      # in the sliders module
      land_rv$grooves <- cc_locate_grooves(
        land_rv$ccdata, 
        method = app_config$proc_params$grooves_method, 
        adjust = app_config$proc_params$grooves_adjust, 
        return_plot = FALSE
      )
      land_rv$left_scan <- land_rv$grooves[[1]][1]
      land_rv$right_scan <- land_rv$grooves[[1]][2]
      
      # Left and right groove sliders ----
      slidersServer(
        id = "left_scan_slider", 
        land_rv = land_rv,
        arg_name = "left_scan", 
        label = "Left groove",
        max_value = land_rv$x3p_dims[1]
      )
      slidersServer(
        id = "right_scan_slider", 
        land_rv = land_rv, 
        arg_name = "right_scan", 
        label = "Right groove",
        max_value = land_rv$x3p_dims[1]
      )
      
      # # Enable grooves button ----
      # buttons_rv$grooves <- TRUE
      
      showNotification(
        "Starting crosscut location found. Adjust with slider if needed.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    })
    
  })
}
