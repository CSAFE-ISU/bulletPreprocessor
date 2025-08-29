profileTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    displayPlotCardUI(ns("profile_plot"))
  )
}

profileServer <- function(id, land_rv, buttons_rv, profile_header_title) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Create reactive header title ----
    profile_header_title <- reactive({
      req(land_rv$barrel)
      req(land_rv$bullet)
      req(land_rv$land)
      paste(land_rv$barrel, land_rv$bullet, land_rv$land, "- Profile at Crosscut")
    })
    
    # Display plot in card ----
    displayPlotCardServer("profile_plot", 
                          plot_reactive = profile_plot_reactive, 
                          header_title = profile_header_title)
    
  })
}
