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
    rglwidgetOutput(id),
  )
}

# Server ----
server <- function(input, output) {

  # Reactive object to store bullet data ----
  bullet_data <- reactiveValues(
    barrel_name = NULL,
    bullet_name = NULL,
    land_name = NULL
  )
  
  # Read bullet ----
  bullet <- reactive({
    # Delete temp directory if it already exists
    bullet_dir <- file.path(tempdir(), "bullet")
    unlink(bullet_dir, recursive = TRUE)
    
    # Create temp directory and save land in it
    dir.create(bullet_dir, showWarnings = FALSE)
    file.copy(
      input$bullet_upload$datapath, 
      file.path(bullet_dir, input$bullet_upload$name)
    )
    
    return(read_bullet(bullet_dir))
  })
  
  # Get land, bullet, and barrel names on upload ----
  observeEvent(input$bullet_upload, {
    bullet_data$barrel_name <- get_barrel_name(input$bullet_upload$name, study = "houston")
    bullet_data$bullet_name <- get_bullet_name(input$bullet_upload$name)
    bullet_data$land_name <- get_land_name(input$bullet_upload$name)
  })
  
  # Display barrel name ----
  output$barrel_name <- renderText({
    req(bullet_data$barrel_name)
    bullet_data$barrel_name
  })
  
  # Display bullet name ----
  output$bullet_name <- renderText({
    req(bullet_data$bullet_name) 
    bullet_data$bullet_name
  })
  
  # Display land name ----
  output$land_name <- renderText({
    req(bullet_data$land_name) 
    bullet_data$land_name
  })
  
  # Display land ----
  output$land_display <- renderUI({
    req(!is.null(input$bullet_upload))

    # Render land
    local({
      output[["land_scan"]] <- renderRglwidget({
        x3p_image(x3p_sample(bullet()$x3p[[1]], m=5), size=500, zoom=.4)
        rglwidget()
      })
    })
    
    # Format land display
    make_land_card(id = "land_scan", land_name = bullet_data$land_name)
  })
}