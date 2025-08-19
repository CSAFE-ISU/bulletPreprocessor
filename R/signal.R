signalUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("signal_button"), "Get signal")
  )
}

signalTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    displayPlotCardUI(ns("signal_plot"))
  )
}

signalServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable button when app starts ----
    disable("signal_button")
    
    # Switch the button on or off ----
    observe({
      if (buttons_rv$signal) {
        enable("signal_button")
      } else {
        disable("signal_button")
      }
    })
    
    # Get signal in data frame ----
    observeEvent(input$signal_button, {
      
      # Get signal ----
      land_rv$sigs <- bulletxtrctr::cc_get_signature(
        ccdata = land_rv$ccdata, 
        grooves = land_rv$grooves, 
        span1 = app_config$proc_params$signal_span1, 
        span2 = app_config$proc_params$signal_span2
      )
      
      showNotification(
        "Final groove locations saved.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
      
      # Switch to signal tab after extracting signal ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Signal")
      }
      
      make_output_df(land_rv = land_rv)
      
      showNotification(
        "Signal extracted.", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
      
    })
    
    # Create reactive plot function ----
    signal_plot_reactive <- reactive({
      req(land_rv$sigs)
      plot_signal(land_rv$sigs)
    })
    
    # Display plot in card ----
    displayPlotCardServer(
      "signal_plot", 
      plot_reactive = signal_plot_reactive, 
      header_title = "Signal"
    )
    
  })
}
