crosscutUI <- function(id) {
  tagList(
    actionButton(NS(id, "crosscut_button"), "Get crosscut"),
    slidersUI(NS(id, "crosscut_slider"))
  )
}

crosscutServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Disable crosscut button when app starts
    disable("crosscut_button")
    
    # Switch the crosscut button on or off
    observe({
      if (buttons_rv$crosscut) {
        enable("crosscut_button")
      } else {
        disable("crosscut_button")
      }
    })
    
    # Get default crosscut ----
    observeEvent(input$crosscut_button, {
      req(land_rv$df)
      land_rv$crosscut <- land_rv$df$x3p[[1]] %>% 
        x3p_crosscut_optimize(ylimits = app_config$proc_params$crosscut_ylimits)
      
      # Enable grooves button
      buttons_rv$grooves <- TRUE
    })
    
    # Display crosscut slider ----
    observe({
      req(land_rv$x3p_dims) 
      req(land_rv$crosscut)  
      slidersServer(
        id = "crosscut_slider", 
        land_rv = land_rv,
        arg_name = "crosscut", 
        label = "Crosscut",
        max_value = land_rv$x3p_dims[2]
      )
    })
    
  })
}
