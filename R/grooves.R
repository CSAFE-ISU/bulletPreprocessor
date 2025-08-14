groovesUI <- function(id) {
  tagList(
    actionButton(NS(id, "grooves_button"), "Get grooves"),
    slidersUI(NS(id, "left_groove_slider")),
    slidersUI(NS(id, "right_groove_slider"))
  )
}

groovesTabUI <- function(id) {
  tagList(
    displayPrintUI(NS(id, "left_groove")),
    displayPrintUI(NS(id, "right_groove")),
    plotOutput(NS(id, "grooves")),
  )
}

groovesServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable grooves button when app starts ----
    disable("grooves_button")
    
    # Switch the grooves button on or off ----
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
      # Store left and right grooves individually to make them easier to update
      # in the sliders module
      grooves <- cc_locate_grooves(
        land_rv$crosscut_df[[1]], 
        method = "middle", 
        adjust = 30, 
        return_plot = FALSE
      )
      land_rv$left_groove <- grooves[[1]][1]
      land_rv$right_groove <- grooves[[1]][2]
      
      # Switch to grooves tab after calculating grooves ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Grooves")
      }
    })
    
    # Left and right groove sliders ----
    observe({
      req(land_rv$crosscut_df)  
      req(land_rv$left_groove)
      req(land_rv$right_groove)
      slidersServer(
        id = "left_groove_slider", 
        land_rv = land_rv,
        arg_name = "left_groove", 
        label = "Left groove",
        max_value = floor(max(land_rv$crosscut_df[[1]]$x, na.rm = TRUE))
      )
      slidersServer(
        id = "right_groove_slider", 
        land_rv = land_rv, 
        arg_name = "right_groove", 
        label = "Right groove",
        max_value = floor(max(land_rv$crosscut_df[[1]]$x, na.rm = TRUE))
      )
    })
    
    # Plot grooves ----
    output$grooves <- renderPlot({
      plot_grooves(land_rv$crosscut_df[[1]], 
                   left_groove = land_rv$left_groove,
                   right_groove = land_rv$right_groove)
    })
    
    # Display left and right groove values ----
    observe({
      req(land_rv$left_groove)
      req(land_rv$right_groove)
      displayPrintServer("left_groove", value = land_rv$left_groove, label = "Left groove")
      displayPrintServer("right_groove", value = land_rv$right_groove, label = "Right groove")
    })
    
  })
}
