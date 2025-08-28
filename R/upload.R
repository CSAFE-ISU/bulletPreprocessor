uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("upload_buttonUI"))
  )
}

uploadServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Switch the button on or off ----
    observe({
      if (buttons_rv$upload) {
        enable("upload_button")
      } else {
        disable("upload_button")
      }
    })
    
    observeEvent(input$study, {
      # Show message ----
      showNotification(
        "Study selected", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$upload_button, {
      
      # Delete temp directory if it already exists ----
      unlink(app_config$file_params$temp_dir, recursive = TRUE)
      
      # Create temp directory and save land in it ----
      dir.create(app_config$file_params$temp_dir, showWarnings = FALSE)
      file.copy(
        input$upload_button$datapath, 
        file.path(app_config$file_params$temp_dir, input$upload_button$name)
      )
      
      # Store study ----
      land_rv$study <- input$study
      
      # Load bullet and get metadata ----
      land_rv$df <- read_bullet(app_config$file_params$temp_dir)
      land_rv$barrel <- get_barrel_name(
        filename = input$upload_button$name, 
        study = land_rv$study
      )
      land_rv$bullet <- get_bullet_name(input$upload_button$name)
      land_rv$land <- get_land_name(input$upload_button$name)
      land_rv$resolution <- x3ptools::x3p_get_scale(land_rv$df$x3p[[1]])
      land_rv$x3p_dims <- dim(land_rv$df$x3p[[1]])
      
      # Disable upload button until reset button is clicked ----
      buttons_rv$upload <- FALSE
      
      # Enable signal button ----
      buttons_rv$signal <- TRUE
      
      # Enable reset button ----
      buttons_rv$reset <- TRUE
      
      # Show message ----
      showNotification(
        "x3p file uploaded", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
      
    })
    
    output$upload_buttonUI <- renderUI({
      req(buttons_rv$upload)
      tagList(
        tooltip(
          selectInput(
            session$ns("study"), 
            label = "Select bullet study", 
            choices = c("Hamby 44", "Houston Group 1", "Houston Group 2", "Houston Group 3", "Phoenix"), 
            selected = app_config$ui_params$default_study),
          "Each bullet study uses a different naming convention. The app will use the naming convention to determine the barrel and bullet names from the filename."
        ),
        tooltip(
          fileInput(
            session$ns("upload_button"), 
            "Upload x3p file", 
            accept = app_config$file_params$allowed_extensions, 
            multiple = FALSE
          ),
          "Upload an image of a single land engraved area saved in x3p file format."
        )
      )
    })
  })
}
