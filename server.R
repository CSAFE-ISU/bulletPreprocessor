library(bslib)
library(bulletxtrctr)
library(rgl)
library(x3ptools)

# Increase maximum upload size
options(shiny.maxRequestSize = 150*1024^2)

get_land_name <- function(filename) {
  return(stringr::str_extract(filename, "Land \\d+"))
}

get_bullet_name <- function(filename) {
  return(stringr::str_extract(filename, "Bullet \\d+"))
}

get_barrel_name <- function(filename, study) {
  if (study == "houston") {
    return(get_houston_barrel_name(filename))
  }
}

get_houston_barrel_name <- function(filename) {
  return(stringr::str_extract(filename, "\\b[A-Z]+(?=\\s+Bullet)"))
}

# Render RGL Widget UI ----
make_land_card <- function(id = "land_scan", land_name = NULL) {
  card(
    card_header(class = "bg-dark", land_name),
    # max_height = 600,
    full_screen = TRUE,
    rglwidgetOutput(id, width = "auto"),
  )
}

# Server ----
server <- function(input, output) {

  # Reactive object to store bullet data ----
  land_data <- reactiveValues(
    barrel_name = NULL,
    bullet_name = NULL,
    land_name = NULL
  )
  
  # Read bullet ----
  land <- reactive({
    # Delete temp directory if it already exists
    land_dir <- file.path(tempdir(), "land")
    unlink(land_dir, recursive = TRUE)
    
    # Create temp directory and save land in it
    dir.create(land_dir, showWarnings = FALSE)
    file.copy(
      input$land_upload$datapath, 
      file.path(land_dir, input$land_upload$name)
    )
    
    return(read_bullet(land_dir))
  })
  
  # Get land, bullet, and barrel names on upload ----
  observeEvent(input$land_upload, {
    land_data$barrel_name <- get_barrel_name(input$land_upload$name, study = "houston")
    land_data$bullet_name <- get_bullet_name(input$land_upload$name)
    land_data$land_name <- get_land_name(input$land_upload$name)
  })
  
  # Display barrel name ----
  output$barrel_name <- renderText({
    req(land_data$barrel_name)
    land_data$barrel_name
  })
  
  # Display bullet name ----
  output$bullet_name <- renderText({
    req(land_data$bullet_name) 
    land_data$bullet_name
  })
  
  # Display land name ----
  output$land_name <- renderText({
    req(land_data$land_name) 
    land_data$land_name
  })
  
  # Display land ----
  output$land_display <- renderUI({
    req(!is.null(input$land_upload))

    # Render land
    local({
      output[["land_scan"]] <- renderRglwidget({
        x3p_image(x3p_sample(land()$x3p[[1]], m=5), size=500, zoom=.4)
        rglwidget()
      })
    })
    
    # Format land display
    make_land_card(id = "land_scan", land_name = land_data$land_name)
  })
}