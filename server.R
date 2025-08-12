library(bslib)
library(bulletxtrctr)
library(rgl)
library(x3ptools)

# Increase maximum upload size
options(shiny.maxRequestSize = 150*1024^2)

# Force RGL to use null device to prevent pop-ups
options(rgl.useNULL = TRUE)

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
make_land_card <- function(id = "land_scan", 
                           barrel_name = NULL,
                           bullet_name = NULL,
                           land_name = NULL) {
  card(
    card_header(class = "bg-dark", paste(barrel_name, bullet_name, land_name)),
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
  
  # Render land ----
  output$land_scan <- renderRglwidget({
    # Clear any existing RGL scenes
    rgl::clear3d()
    
    if (is.null(land_data$crosscut)) {
      land()$x3p[[1]] %>%
        x3p_sample(m=5) %>%
        x3p_image(size = 500, zoom=.4)
    } else {
      land()$x3p[[1]] %>%
        x3p_add_hline(yintercept = land_data$crosscut, size = 20, color = "#eeeeee") %>%
        x3p_sample(m=5) %>%
        x3p_image(size = 500, zoom=.4)
    }

    rglwidget()
  })
  
  # Display land in card ----
  output$land_display <- renderUI({
    req(!is.null(input$land_upload))
    make_land_card(id = "land_scan", 
                   barrel_name = land_data$barrel_name, 
                   bullet_name = land_data$bullet_name,
                   land_name = land_data$land_name)
  })
  
  # Get default crosscut ----
  observeEvent(input$crosscut, {
    land_data$crosscut <- sapply(land()$x3p, x3p_crosscut_optimize, ylimits = c(150, NA))
  })
  
  output$crosscut <- renderPrint({
    req(land_data$crosscut)
    land_data$crosscut
  })
  
}