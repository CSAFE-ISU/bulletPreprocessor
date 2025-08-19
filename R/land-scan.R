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
    
      req(land_rv$df)
      req(nrow(land_rv$df) > 0)
      req(!is.null(land_rv$df$x3p))
      req(length(land_rv$df$x3p) > 0)
      req(!is.null(land_rv$df$x3p[[1]]))
      
      if (is.null(land_rv$crosscut)) {
        land_rv$df$x3p[[1]] %>%
          x3p_sample(m=app_config$display_params$scan_sample_rate) %>%
          x3p_image(
            size = app_config$display_params$scan_size, 
            zoom = app_config$display_params$scan_zoom
          )
      } else {
        land_rv$df$x3p[[1]] %>%
          x3p_add_hline(
            yintercept = land_rv$crosscut, 
            size = 20, 
            color = app_config$display_params$crosscut_color
          ) %>%
          x3p_sample(m=app_config$display_params$scan_sample_rate) %>%
          x3p_image(
            size = app_config$display_params$scan_size, 
            zoom = app_config$display_params$scan_zoom
          )
      }
      
      rglwidget()
    })
    
    # Display land in card ----
    output$land_display <- renderUI({
      req(!is.null(land_rv$df))
      card(
        card_header(
          class = app_config$display_params$card_header_class, 
          paste(land_rv$barrel, land_rv$bullet, land_rv$land)
        ),
        full_screen = app_config$display_params$card_full_screen,
        rglwidgetOutput(session$ns("land_scan"), width = "auto"),
      )
    })
    
  })
}
