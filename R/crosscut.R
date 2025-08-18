crosscutUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("crosscut_button"), "Get crosscut"),
    # Use conditionalPanel to show/hide based on a condition
    conditionalPanel(
      condition = paste0("output['", ns("crosscut_available"), "'] == true"),
      sliderModuleUI(ns("my_slider"), label = "Choose a value:"),
      br(),
      h4("Current crosscut value:"),
      textOutput(ns("crosscut_value")),
      h4("Current slider value:"),
      textOutput(ns("slider_value"))
    )
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
    
    # Crosscut button ----
    observeEvent(input$crosscut_button, {
      
      # Get default crosscut ----
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% 
        x3p_crosscut_optimize(ylimits = app_config$proc_params$crosscut_ylimits)
      
      # Enable grooves button ----
      buttons_rv$grooves <- TRUE
      
      # Show crosscut slider ----
      buttons_rv$crosscut_slider <- TRUE
    })
    
    # Output for conditionalPanel condition ----
    output$crosscut_available <- reactive({
      buttons_rv$crosscut_slider
    })
    outputOptions(output, "crosscut_available", suspendWhenHidden = FALSE)
    
    # Slider module. Capture current slider value ----
    slider_value <- sliderModuleServer(
      id = "my_slider",
      initial_value = reactive({ifelse(is.null(land_rv$crosscut), 0, land_rv$crosscut)}),
      max_value = land_rv$x3p_dims[2]
    )
    
    # Display crosscut value ----
    output$crosscut_value <- renderText({
      if (is.null(land_rv$crosscut)) {
        "Not set yet"
      } else {
        paste("Crosscut value:", land_rv$crosscut)
      }
    })
    
    # Display current slider value ----
    output$slider_value <- renderText({
      if (is.null(land_rv$crosscut)) {
        "Slider not available yet"
      } else {
        paste("The slider is currently set to:", slider_value())
      }
    })
    
    # Update reactive values
    observe({
      land_rv$crosscut <- slider_value()
    })
    
  })
}
