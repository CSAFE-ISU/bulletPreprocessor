crosscutUI <- function(id) {
  ns <- NS(id)
  tagList(
    slidersUI(ns("crosscut_slider")),
    slidersUI(ns("left_scan_slider")),
    slidersUI(ns("right_scan_slider"))
  )
}

crosscutServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Display and update the crosscut ----
    observeEvent(land_rv$df, {
      req(land_rv$df$x3p)
      req(land_rv$x3p_dims[2])
      
      # Get default crosscut ----
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% 
        x3p_crosscut_optimize(ylimits = app_config$proc_params$crosscut_ylimits)
      
      # Get crosscut profile data ----
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
      
      # Crosscut slider ----
      # The crosscut slider updates land_rv$crosscut to the current slider
      # value. The rgl widget in the landScanServer updates the crosscut on the
      # displayed scan when land_rv$crosscut changes.
      slidersServer(
        id = "crosscut_slider", 
        land_rv = land_rv,
        arg_name = "crosscut", 
        label = "Crosscut",
        max_value = land_rv$x3p_dims[2]
      )
      
      # Left and right groove sliders ---- 
      # The left and right groove sliders update land_rv$left_scan and
      # land_rv$right_scan, respectively. The rgl widget in the landScanServer
      # updates the crosscut on the displayed scan when land_rv$left_scan or
      # land_rv$right_scan change.
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
      
      showNotification(
        "Starting crosscut and grooves found. Adjust with sliders if needed.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    })
    
    # Update land_rv$grooves ----
    # Left_scan and right_scan are updated by sliders module, but
    # land_rv$grooves is not.
    observeEvent(c(land_rv$left_scan, land_rv$right_scan), {
      req(land_rv$grooves)
      
      land_rv$grooves[[1]][1] <- land_rv$left_scan
      land_rv$grooves[[1]][2] <- land_rv$right_scan
    })
    
    # Update crosscut data ----
    
    # Crosscut data is calculated at the default crosscut location when an x3p
    # is uploaded. This updates the crosscut data when the crosscut location
    # changes.
    observeEvent(land_rv$crosscut, {
      req(land_rv$df)
      req(land_rv$df$x3p)

      land_rv$ccdata <- x3p_crosscut(x = land_rv$df$x3p[[1]], y = land_rv$crosscut)
    })
    
  })
}
