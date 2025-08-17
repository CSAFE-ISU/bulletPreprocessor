uploadUI <- function(id) {
  tagList(
    selectInput(
      NS(id, "study"), 
      label = "Select bullet study", 
      choices = c("Hamby 44", "Houston Group 1", "Houston Group 2", "Houston Group 3", "Phoenix"), 
      selected = app_config$ui_params$default_study),
    fileInput(
      NS(id, "land_upload"), 
      "Upload x3p file", 
      accept = app_config$file_params$allowed_extensions, 
      multiple = TRUE
    )
  )
}

uploadServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$land_upload, {
      
      # Delete temp directory if it already exists ----
      unlink(app_config$file_params$temp_dir, recursive = TRUE)
      
      # Create temp directory and save land in it ----
      dir.create(app_config$file_params$temp_dir, showWarnings = FALSE)
      file.copy(
        input$land_upload$datapath, 
        file.path(app_config$file_params$temp_dir, input$land_upload$name)
      )
      
      # Store study ----
      land_rv$study <- input$study
      
      # Load bullet and get metadata ----
      land_rv$df <- read_bullet(app_config$file_params$temp_dir)
      land_rv$barrel <- get_barrel_name(
        input$land_upload$name, 
        study = land_rv$study
      )
      land_rv$bullet <- get_bullet_name(input$land_upload$name)
      land_rv$land <- get_land_name(input$land_upload$name)
      land_rv$resolution <- x3ptools::x3p_get_scale(land_rv$df$x3p[[1]])
      land_rv$x3p_dims <- dim(land_rv$df$x3p[[1]])
      
      # Enable crosscut button ----
      buttons_rv$crosscut <- TRUE
    })
  })
}
