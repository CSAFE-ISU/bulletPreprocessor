resetUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("reset_buttonUI"))
  )
}

resetServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable button when app starts ----
    disable("reset_button")
    
    # Switch the button on or off ----
    observe({
      if (buttons_rv$reset) {
        enable("reset_button")
      } else {
        disable("reset_button")
      }
    })
    
    observeEvent(input$reset_button, {
      
      # Show confirmation dialog ----
      showModal(modalDialog(
        title = "Confirm Reset",
        "Are you sure you want to reset all data? This action cannot be undone.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_reset"), "Reset")
        )
      ))
      
    })
    
    # Perform reset after confirmation ----
    observeEvent(input$confirm_reset, {
      
      # Delete temp directory if it exists ----
      if (dir.exists(app_config$file_params$temp_dir)) {
        unlink(app_config$file_params$temp_dir, recursive = TRUE)
      }
      
      # Reset all land reactive values to NULL ----
      land_rv$barrel <- NULL
      land_rv$bullet <- NULL
      land_rv$ccdata <- NULL
      land_rv$crosscut <- NULL
      land_rv$df <- NULL
      land_rv$grooves <- NULL
      land_rv$land <- NULL
      land_rv$left_scan <- NULL
      land_rv$output_df <- NULL
      land_rv$resolution <- NULL
      land_rv$right_scan <- NULL
      land_rv$sigs <- NULL
      land_rv$study <- NULL
      land_rv$x3p_dims <- NULL

      # Reset all button states ----
      buttons_rv$reset <- FALSE
      buttons_rv$signal <- FALSE
      buttons_rv$upload <- TRUE

      # Clear RGL scene ----
      try({
        rgl::clear3d()
      }, silent = TRUE)
      
      # Switch back to first tab ----
      if (!is.null(main_session)) {
        nav_select(
          session = main_session, 
          "main_tabs", 
          selected = "Land with Crosscut and Grooves"
        )
      }
      
      # Close the modal ----
      removeModal()
      
      # Show success message ----
      showNotification(
        "All data has been reset successfully!", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    })
    
    output$reset_buttonUI <- renderUI({
      req(buttons_rv$reset)
      
      tagList(
        tooltip(
          actionButton(session$ns("reset_button"), "Reset All"),
          "Click Reset All to upload a new file."
        )
      )
      
    })
    
  })
}
