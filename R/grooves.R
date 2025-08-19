groovesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(NS(id, "grooves_button"), "Get grooves"),
    # Use conditionalPanel to show/hide based on a condition
    conditionalPanel(
      condition = paste0("output['", ns("grooves_available"), "'] == true"),
      br(),
      sliderUI(ns("left_groove_slider"), label = "Left groove"),
      sliderUI(ns("right_groove_slider"), label = "Right groove"),
      actionButton(ns("save_grooves_button"), "Save grooves")
    )
  )
}

groovesTabUI <- function(id) {
  tagList(
    displayPlotCardUI(NS(id, "grooves_plot"))
  )
}

groovesServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable grooves button when app starts ----
    disable("grooves_button")
    # Should be hidden in conditional panel, but disable just in case
    disable("save_grooves_button")  
    
    # Switch the grooves button on or off ----
    observe({
      if (buttons_rv$grooves) {
        enable("grooves_button")
      } else {
        disable("grooves_button")
      }
    })
    
    # Switch the save grooves button on or off ----
    observe({
      if (buttons_rv$save_grooves) {
        enable("save_grooves_button")
      } else {
        disable("save_grooves_button")
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
      # Need as named list for `cc_get_signature()` on signals tab
      land_rv$grooves <- cc_locate_grooves(
        land_rv$ccdata, 
        method = app_config$proc_params$grooves_method, 
        adjust = app_config$proc_params$grooves_adjust, 
        return_plot = FALSE
      )
      # Also, store values individually to work better with sliders
      land_rv$left_groove <- land_rv$grooves[[1]][1]
      land_rv$right_groove <- land_rv$grooves[[1]][2]
      
      # Switch to grooves tab after calculating grooves ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Grooves")
      }
      
      # Enable signal button
      buttons_rv$signal <- TRUE
      
      # Show grooves sliders ----
      buttons_rv$grooves_sliders <- TRUE
    })
    
    # Output for conditionalPanel condition ----
    output$grooves_available <- reactive({
      buttons_rv$grooves_sliders
    })
    outputOptions(output, "grooves_available", suspendWhenHidden = FALSE)
    
    # Slider modules ----
    left_groove_value <- sliderServer(
      id = "left_groove_slider",
      initial_value = reactive({ifelse(is.null(land_rv$left_groove), 0, land_rv$left_groove)}),
      max_value = reactive({ifelse(is.null(land_rv$ccdata$x), 0, floor(max(land_rv$ccdata$x, na.rm = TRUE)))})
    )
    right_groove_value <- sliderServer(
      id = "right_groove_slider",
      initial_value = reactive({ifelse(is.null(land_rv$right_groove), 500, land_rv$right_groove)}),
      max_value = reactive({ifelse(is.null(land_rv$ccdata$x), 0, floor(max(land_rv$ccdata$x, na.rm = TRUE)))})
    )
    
    # Turn on save button if sliders change ----
    observeEvent(left_groove_value(), {
      buttons_rv$save_grooves <- TRUE
    })
    observeEvent(right_groove_value(), {
      buttons_rv$save_grooves <- TRUE
    })
    
    # Update reactive values ----
    observeEvent(input$save_grooves_button, {
      land_rv$left_groove <- left_groove_value()
      land_rv$right_groove <- right_groove_value()
      land_rv$grooves[[1]][1] <- left_groove_value()
      land_rv$grooves[[1]][2] <- right_groove_value()
      
      # Disable save button ----
      buttons_rv$save_grooves <- FALSE
      
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
    displayPlotCardServer(
      "grooves_plot", 
      plot_reactive = grooves_plot_reactive, 
      header_title = "Grooves"
    )
    
  })
}
