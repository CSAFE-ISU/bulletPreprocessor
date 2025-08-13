uploadUI <- function(id) {
  tagList(
    fileInput(NS(id, "land_upload"), "Upload a land", accept = ".x3p", multiple = TRUE)
  )
}

uploadServer <- function(id, land_rv) {
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
      land_rv$barrel_name <- get_barrel_name(input$land_upload$name, study = "houston")
      land_rv$bullet_name <- get_bullet_name(input$land_upload$name)
      land_rv$land_name <- get_land_name(input$land_upload$name)
    })
  })
}