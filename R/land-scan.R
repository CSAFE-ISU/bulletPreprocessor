landScanUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("land_display"))
  )
}

landScanServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Render land scan ----
    output$land_scan <- renderRglwidget({
      req(land_rv$df)
      req(land_rv$df$x3p)
      
      # Clear any existing RGL scenes
      rgl::clear3d()
      
      if (is.null(land_rv$crosscut)) {
        land_rv$df$x3p[[1]] %>%
          x3p_sample(m=app_config$display_params$scan_sample_rate) %>%
          x3p_image(
            # size = app_config$display_params$scan_size,
            zoom = app_config$display_params$scan_zoom
          )
      } else {
        land_rv$df$x3p[[1]] %>%
          x3p_add_hline(  # crosscut
            yintercept = land_rv$crosscut, 
            size = app_config$display_params$crosscut_size,
            color = app_config$display_params$crosscut_color
          ) %>%
          x3p_add_vline(  # left groove
            xintercept = land_rv$left_scan,
            size = app_config$display_params$groove_size,
            color = app_config$display_params$groove_left_color
          ) %>%
          x3p_add_vline(  # right groove
            xintercept = land_rv$right_scan,
            size = app_config$display_params$groove_size,
            color = app_config$display_params$groove_right_color
          ) %>%
          x3p_sample(m=app_config$display_params$scan_sample_rate) %>%
          x3p_image(
            # size = app_config$display_params$scan_size,
            zoom = app_config$display_params$scan_zoom
          )
      }
      
      rglwidget()
    })
    
    # Display land in card ----
    output$land_display <- renderUI({
      req(land_rv$df)
      req(land_rv$barrel)
      req(land_rv$bullet)
      req(land_rv$land)
      
      tagList(
        layout_column_wrap(
          card(
            card_header(
              class = app_config$display_params$card_header_class, 
              paste(land_rv$barrel, land_rv$bullet, land_rv$land, "- Crosscut and Grooves")
            ),
            fill = TRUE,
            full_screen = app_config$display_params$card_full_screen,
            rglwidgetOutput(session$ns("land_scan"), width = "auto"),
          ),
          profileTabUI(session$ns("profile1"))
        )
      )

    })
    
    profileServer("profile1", land_rv, buttons_rv, session)
    
  })
}
