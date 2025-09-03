crosscutGroovesUI <- function(id) {
  ns <- NS(id)
  tagList(
    slidersUI(ns("crosscut_slider")),
    slidersUI(ns("left_scan_slider")),
    slidersUI(ns("right_scan_slider"))
  )
}

crosscutGroovesTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("land_scanUI"))
  )
}

crosscutGroovesServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate and update the crosscut ----
    observeEvent(land_rv$upload_confirmed, {
      req(land_rv$df)
      req(land_rv$df$x3p)
      req(land_rv$x3p_dims[1])
      req(land_rv$x3p_dims[2])
      
      # Get default crosscut ----
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% 
        x3p_crosscut_optimize(ylimits = app_config$proc_params$crosscut_ylimits)
      validate(
        need(is.numeric(land_rv$crosscut), "Crosscut must be numeric"),
        need(land_rv$crosscut > 0, "Crosscut must be a postive number"),
        need(land_rv$crosscut < land_rv$x3p_dims[2], "Crosscut must be less than x3p height")
      )
      
      # Get crosscut profile data at default crosscut ----
      land_rv$ccdata <- x3p_crosscut(x = land_rv$df$x3p[[1]], y = land_rv$crosscut)
      validate(
        need(is.data.frame(land_rv$ccdata), "ccdata must be a data frame"),
        need(nrow(land_rv$ccdata) > 0, "ccdata must have more than one row")
      )
      
      # Get default grooves ----
      # Store left and right grooves individually to make them easier to update
      # in the sliders module
      land_rv$grooves <- cc_locate_grooves(
        land_rv$ccdata, 
        method = app_config$proc_params$grooves_method, 
        adjust = app_config$proc_params$grooves_adjust, 
        return_plot = FALSE
      )
      validate(
        need(land_rv$grooves, "grooves must be truthy"),
        need(is.list(land_rv$grooves), "grooves must be a list"),
        need(length(land_rv$grooves[[1]]) == 2, "grooves must be a list containing a vector of length 2")
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
        min_value = 0,
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
        min_value = 0,
        max_value = floor(land_rv$x3p_dims[1] / 3)
      )
      slidersServer(
        id = "right_scan_slider", 
        land_rv = land_rv, 
        arg_name = "right_scan", 
        label = "Right groove",
        min_value = floor(land_rv$x3p_dims[1] / 3),
        max_value = floor(2 * land_rv$x3p_dims[1] / 3)
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
      req(land_rv$x3p_dims[1])
      validate(
        need(is.numeric(land_rv$left_scan), "Left groove must be numeric"),
        need(land_rv$left_scan >= 0, "Left groove greater than or equal to zero"),
        need(is.numeric(land_rv$right_scan), "Right groove must be numeric"),
        need(land_rv$right_scan > land_rv$left_scan , "Right groove must be greater than left groove"),
        need(land_rv$right_scan <= land_rv$x3p_dims[1], "Right groove must be less than or equal to x3p width")
      )
      validate(
        need(land_rv$grooves, "grooves must be truthy"),
        need(is.list(land_rv$grooves), "grooves must be a list"),
        need(length(land_rv$grooves[[1]]) == 2, "grooves must be a list containing a vector of length 2")
      )
      
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
    
    # Render land scan ----
    output$land_scan <- renderRglwidget({
      req(land_rv$upload_confirmed)
      req(land_rv$crosscut)
      req(land_rv$df)
      req(land_rv$df$x3p)
      
      # Clear any existing RGL scenes
      try({
        rgl::clear3d()
        rgl::gc3d()  # Force RGL garbage collection
      }, silent = TRUE)
      
      # Create a temporary copy and process it to avoid modifying original
      temp_x3p <- land_rv$df$x3p[[1]]
      
      # Create plot
      x3p_plot <- temp_x3p %>%
        x3p_add_hline(  # crosscut
          yintercept = land_rv$crosscut, 
          size = app_config$display_params$crosscut_size,
          color = app_config$display_params$crosscut_color
        ) %>%
        x3p_add_vline(  # left groove
          xintercept = land_rv$left_scan,
          size = app_config$display_params$groove_size,
          color = app_config$display_params$groove_left_color
        ) %>%
        x3p_add_vline(  # right groove
          xintercept = land_rv$right_scan,
          size = app_config$display_params$groove_size,
          color = app_config$display_params$groove_right_color
        ) %>%
        x3p_sample(m=app_config$display_params$scan_sample_rate)
      
      # Render plot to RGL device immediately
      x3p_plot %>%
        x3p_image(
          size = app_config$display_params$scan_size,
          zoom = app_config$display_params$scan_zoom
        )
      
      # Clean up R objects. NOTE: Setting x3p_plot to NULL does not change the
      # plot in the RGL device
      x3p_plot <- NULL
      temp_x3p <- NULL
      
      # Force garbage collection after processing
      gc(verbose = FALSE)
      
      # Capture the already-rendered plot in the RGL device
      rglwidget()
    })
    
    output$land_scanUI <- renderUI({
      req(land_rv$upload_confirmed)
      req(land_rv$barrel)
      req(land_rv$bullet)
      req(land_rv$land)
      
      card(
        card_header(
          class = app_config$display_params$card_header_class, 
          paste(land_rv$barrel, land_rv$bullet, land_rv$land, "- Crosscut and Grooves")
        ),
        fill = TRUE,
        full_screen = app_config$display_params$card_full_screen,
        rglwidgetOutput(session$ns("land_scan"), width = "auto"),
      )
    })
    
  })
}
