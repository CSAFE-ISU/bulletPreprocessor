signalUI <- function(id) {
  tagList(
    selectInput(
      NS(id, "study"), 
      label = "Choose bullet study", 
      choices = c("Hamby 44", "Houston Group 1", "Houston Group 2", "Houston Group 3", "Phoenix"), 
      selected = "Houston Group 1"),
    actionButton(NS(id, "signal_button"), "Get signal")
  )
}

signalTabUI <- function(id) {
  tagList(
    displayPlotCardUI(NS(id, "signal_plot"))
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
      
      # Store study ----
      land_rv$study <- input$study
      
      # Get signal ----
      land_rv$sigs <- bulletxtrctr::cc_get_signature(
        ccdata = land_rv$ccdata, 
        grooves = land_rv$grooves, 
        span1 = 0.75, 
        span2 = 0.03
      )
      
      # Switch to signal tab after extracting signal ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Signal")
      }
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
