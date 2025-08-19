groovesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("grooves_button"), "Get grooves"),
    slidersUI(ns("left_groove_slider")),
    slidersUI(ns("right_groove_slider"))
  )
}

groovesTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    displayPlotCardUI(ns("grooves_plot"))
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
    
    # Display and update grooves ----
    observeEvent(input$grooves_button, {
      
      # Take crosscut ----
      if (is.null(land_rv$ccdata)) {
        req(land_rv$crosscut)
        land_rv$ccdata <- x3p_crosscut(x = land_rv$df$x3p[[1]], y = land_rv$crosscut)
      }
      
      # Get default grooves ----
      # Store left and right grooves individually to make them easier to update
      # in the sliders module
      land_rv$grooves <- cc_locate_grooves(
        land_rv$ccdata, 
        method = app_config$proc_params$grooves_method, 
        adjust = app_config$proc_params$grooves_adjust, 
        return_plot = FALSE
      )
      land_rv$left_groove <- land_rv$grooves[[1]][1]
      land_rv$right_groove <- land_rv$grooves[[1]][2]
      
      # Left and right groove sliders ----
      slidersServer(
        id = "left_groove_slider", 
        land_rv = land_rv,
        arg_name = "left_groove", 
        label = "Left groove",
        max_value = floor(max(land_rv$ccdata$x, na.rm = TRUE))
      )
      slidersServer(
        id = "right_groove_slider", 
        land_rv = land_rv, 
        arg_name = "right_groove", 
        label = "Right groove",
        max_value = floor(max(land_rv$ccdata$x, na.rm = TRUE))
      )
      
      # Switch to grooves tab after calculating grooves ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Grooves")
      }
      
      # Enable signal button
      buttons_rv$signal <- TRUE
    })
    
    # Update land_rv$grooves ----
    # Left_groove and right_groove are updated by sliders module, 
    # but land_rv$grooves is not.
    observe({
      req(land_rv$left_groove)
      req(land_rv$right_groove)
      land_rv$grooves[[1]][1] <- land_rv$left_groove
      land_rv$grooves[[1]][2] <- land_rv$right_groove
    })
    
    # Create reactive plot function ----
    grooves_plot_reactive <- reactive({
      req(land_rv$ccdata)
      req(land_rv$left_groove)
      req(land_rv$right_groove)
      plot_grooves(land_rv$ccdata, 
                   left_groove = land_rv$left_groove,
                   right_groove = land_rv$right_groove)
    })
    
    # Display plot in card ----
    displayPlotCardServer("grooves_plot", 
                          plot_reactive = grooves_plot_reactive, 
                          header_title = "Grooves")
    
  })
}
