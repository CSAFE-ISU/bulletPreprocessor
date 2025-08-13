landScanUI <- function(id) {
  tagList(
    uiOutput(NS(id, "land_display"))
  )
}

# Land Scan Module Server
landScanServer <- function(id, land_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Render land scan ----
    output$land_scan <- renderRglwidget({
      # Clear any existing RGL scenes
      rgl::clear3d()
      
      # Check if we have data to render
      req(land_rv$df)
      req(nrow(land_rv$df) > 0)
      req(!is.null(land_rv$df$x3p))
      req(length(land_rv$df$x3p) > 0)
      req(!is.null(land_rv$df$x3p[[1]]))
      
      if (is.null(land_rv$crosscut)) {
        land_rv$df$x3p[[1]] %>%
          x3p_sample(m=5) %>%
          x3p_image(size = 500, zoom=.4)
      } else {
        land_rv$df$x3p[[1]] %>%
          x3p_add_hline(yintercept = land_rv$crosscut, size = 20, color = "#eeeeee") %>%
          x3p_sample(m=5) %>%
          x3p_image(size = 500, zoom=.4)
      }
      
      rglwidget()
    })
    
    # Display land in card ----
    output$land_display <- renderUI({
      req(!is.null(land_rv$df))
      make_land_card(
        land_id = session$ns("land_scan"), # Important: use session$ns() to namespace the ID
        barrel_name = land_rv$barrel_name, 
        bullet_name = land_rv$bullet_name,
        land_name = land_rv$land_name
      )
    })
    
  })
}