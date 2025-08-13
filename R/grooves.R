groovesUI <- function(id) {
  tagList(
    actionButton(NS(id, "grooves_button"), "Get grooves")
  )
}

groovesTabUI <- function(id) {
  tagList(
    plotOutput(NS(id, "grooves"))
  )
}

groovesServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable grooves button when app starts
    disable("grooves_button")
    
    # Switch the grooves button on or off
    observe({
      if (buttons_rv$grooves) {
        enable("grooves_button")
      } else {
        disable("grooves_button")
      }
    })
    
    # Get default grooves ----
    observeEvent(input$grooves_button, {
      
      # Take crosscut ----
      if (is.null(land_rv$crosscut_df)) {
        req(land_rv$crosscut)  # Make sure crosscut exists
        land_rv$crosscut_df <- lapply(land_rv$df$x3p, function(x) x3p_crosscut(x, y = land_rv$crosscut))
      }
      
      # Get grooves ----
      land_rv$grooves <- cc_locate_grooves(
        land_rv$crosscut_df[[1]], 
        method = "middle", 
        adjust = 30, 
        return_plot = FALSE
      )
      
      # Switch to grooves tab after calculating grooves
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Grooves")
      }
    })
    
    # Plot grooves ----
    output$grooves <- renderPlot({
      plot_grooves(land_rv$crosscut_df[[1]], land_rv$grooves[[1]])
    })
    
  })
}
