groovesUI <- function(id) {
  tagList(
    actionButton(NS(id, "default_grooves_button"), "Get grooves")
  )
}

groovesTabUI <- function(id) {
  tagList(
    plotOutput(NS(id, "grooves"))
  )
}

groovesServer <- function(id, land_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Get default grooves ----
    observeEvent(input$default_grooves_button, {
      
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
      land_rv$left_groove <- land_rv$grooves[[1]][1]
      land_rv$right_groove <- land_rv$grooves[[1]][2]
    })
    
    # Plot grooves ----
    output$grooves <- renderPlot({
      req(land_rv$crosscut_df)
      req(land_rv$grooves)
      plot_grooves(land_rv$crosscut_df[[1]], land_rv$grooves[[1]])
    })
    
  })
}