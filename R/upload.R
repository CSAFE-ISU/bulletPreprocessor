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
      cleanup_temp_directory(
        temp_dir = app_config$file_params$temp_dir,
        force = TRUE
      )
      
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
      land_rv$bullet <- get_bullet_name(input$upload_button$name, land_rv$study)
      land_rv$land <- get_land_name(input$upload_button$name)
      land_rv$resolution <- x3ptools::x3p_get_scale(land_rv$df$x3p[[1]])
      land_rv$x3p_dims <- dim(land_rv$df$x3p[[1]])
      
      # Show confirmation modal with extracted metadata ----
      showModal(modalDialog(
        title = "Confirm File Information",
        div(
          h4("Please confirm the following information extracted from your file:"),
          br(),
          tags$table(
            style = "width: 100%; border-collapse: collapse;",
            tags$tr(
              tags$td(strong("Study:"), style = "padding: 8px; border-bottom: 1px solid #ddd; width: 30%;"),
              tags$td(land_rv$study, style = "padding: 8px; border-bottom: 1px solid #ddd;")
            ),
            tags$tr(
              tags$td(strong("Barrel:"), style = "padding: 8px; border-bottom: 1px solid #ddd;"),
              tags$td(land_rv$barrel, style = "padding: 8px; border-bottom: 1px solid #ddd;")
            ),
            tags$tr(
              tags$td(strong("Bullet:"), style = "padding: 8px; border-bottom: 1px solid #ddd;"),
              tags$td(land_rv$bullet, style = "padding: 8px; border-bottom: 1px solid #ddd;")
            ),
            tags$tr(
              tags$td(strong("Land:"), style = "padding: 8px; border-bottom: 1px solid #ddd;"),
              tags$td(land_rv$land, style = "padding: 8px; border-bottom: 1px solid #ddd;")
            ),
            tags$tr(
              tags$td(strong("Resolution:"), style = "padding: 8px; border-bottom: 1px solid #ddd;"),
              tags$td(paste(land_rv$resolution, "µm"), style = "padding: 8px; border-bottom: 1px solid #ddd;")
            ),
            tags$tr(
              tags$td(strong("X3P Dimensions:"), style = "padding: 8px; border-bottom: 1px solid #ddd;"),
              tags$td(paste(land_rv$x3p_dims[1], "x", land_rv$x3p_dims[2], "x", land_rv$x3p_dims[3]), 
                      style = "padding: 8px; border-bottom: 1px solid #ddd;")
            )
          ),
          br(),
          p("If this information looks correct, click 'Confirm' to proceed. Otherwise, click 'Cancel' and upload a different file.")
        ),
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton(session$ns("cancel_upload"), "Cancel", class = "btn-secondary"),
          actionButton(session$ns("confirm_upload"), "Confirm", class = "btn-primary")
        )
      ))
    })
    
    # Handle confirmation ----
    observeEvent(input$confirm_upload, {
      
      land_rv$upload_confirmed <- TRUE
      
      # Disable upload button until reset button is clicked ----
      buttons_rv$upload <- FALSE
      
      # Enable signal button ----
      buttons_rv$signal <- TRUE
      
      # Enable reset button ----
      buttons_rv$reset <- TRUE
      
      # Close the modal ----
      removeModal()
      
      # Show success message ----
      showNotification(
        "x3p file uploaded and confirmed", 
        type = "message", 
        duration = app_config$display_params$notification_duration
      )
      
    })
    
    # Handle cancellation ----
    observeEvent(input$cancel_upload, {
      
      # Clean up: delete temp directory ----
      cleanup_temp_directory(
        temp_dir = app_config$file_params$temp_dir,
        force = TRUE
      )
      
      # Reset land reactive values to NULL and run garbage collection ----
      reset_land_data(land_rv)
      
      # Close the modal ----
      removeModal()
      
      # Show cancellation message ----
      showNotification(
        "Upload cancelled", 
        type = "warning", 
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
