uploadUI <- function(id) {
  tagList(
    fileInput(NS(id, "land_upload"), "Upload a land", accept = ".x3p", multiple = TRUE)
  )
}

uploadServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$land_upload, {
      # Delete temp directory if it already exists
      land_dir <- file.path(tempdir(), "land")
      unlink(land_dir, recursive = TRUE)
      
      # Create temp directory and save land in it
      dir.create(land_dir, showWarnings = FALSE)
      file.copy(
        input$land_upload$datapath, 
        file.path(land_dir, input$land_upload$name)
      )
      land_rv$df <- read_bullet(land_dir)
      land_rv$barrel <- get_barrel_name(input$land_upload$name, study = "houston")
      land_rv$bullet <- get_bullet_name(input$land_upload$name)
      land_rv$land <- get_land_name(input$land_upload$name)
      land_rv$resolution <- x3ptools::x3p_get_scale(land_rv$df$x3p[[1]])
      land_rv$x3p_dims <- dim(land_rv$df$x3p[[1]])
        
      # Enable crosscut button
      buttons_rv$crosscut <- TRUE
    })
  })
}
