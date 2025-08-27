profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("profile_button"), "Preview profile")
  )
}

profileTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    displayPlotCardUI(ns("profile_plot"))
  )
}

profileServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable profile button when app starts ----
    disable("profile_button")
    
    # Switch the profile button on or off ----
    observe({
      if (buttons_rv$profile) {
        enable("profile_button")
      } else {
        disable("profile_button")
      }
    })
    
    # Display the profile ----
    observeEvent(input$profile_button, {
      
      showNotification(
        "Final crosscut location saved.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
      
      # Switch to grooves tab after calculating grooves ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Profile")
      }
      
      # Enable signal button ----
      buttons_rv$signal <- TRUE
      
      showNotification(
        "Starting groove locations found. Adjust with sliders if needed.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    })
    
    # Create reactive plot function ----
    profile_plot_reactive <- reactive({
      req(land_rv$ccdata)
      req(land_rv$left_scan)
      req(land_rv$right_scan)
      validate(need(land_rv$left_scan < land_rv$right_scan, "Left groove must be less than right groove"))
      
      plot_grooves(land_rv$ccdata, 
                   left_groove = land_rv$left_scan,
                   right_groove = land_rv$right_scan)
    })
    
    # Display plot in card ----
    displayPlotCardServer("profile_plot", 
                          plot_reactive = profile_plot_reactive, 
                          header_title = "Grooves")
    
  })
}
