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
make_land_card <- function(land_id, 
                           barrel_name = NULL,
                           bullet_name = NULL,
                           land_name = NULL) {
  card(
    card_header(class = "bg-dark", paste(barrel_name, bullet_name, land_name)),
    full_screen = TRUE,
    rglwidgetOutput(land_id, width = "auto"),
  )
}

make_table_card <- function(table_id, header_title) {
  card(
    card_header(class = "bg-dark", header_title),
    full_screen = TRUE,
    tableOutput(table_id),
  )
}

# Server ----
server <- function(input, output) {
  
  # Reactive object to store bullet data ----
  land_data <- reactiveValues(
    df = NULL,
    barrel_name = NULL,
    bullet_name = NULL,
    land_name = NULL,
    crosscut = NULL
  )
  
  # Read bullet ----
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
    land_data$df <- read_bullet(land_dir)
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
      land_data$df$x3p[[1]] %>%
        x3p_sample(m=5) %>%
        x3p_image(size = 500, zoom=.4)
    } else {
      land_data$df$x3p[[1]] %>%
        x3p_add_hline(yintercept = land_data$crosscut, size = 20, color = "#eeeeee") %>%
        x3p_sample(m=5) %>%
        x3p_image(size = 500, zoom=.4)
    }
    
    rglwidget()
  })
  
  # Display land in card ----
  output$land_display <- renderUI({
    req(!is.null(input$land_upload))
    make_land_card(land_id = "land_scan", 
                   barrel_name = land_data$barrel_name, 
                   bullet_name = land_data$bullet_name,
                   land_name = land_data$land_name)
  })
  
  # Get default crosscut ----
  observeEvent(input$default_crosscut_button, {
    land_data$crosscut <- land_data$df$x3p[[1]] %>% x3p_crosscut_optimize(ylimits = c(150, NA))
  })
  
  # Update crosscut on land display with slider ----
  observeEvent(input$crosscut_slider, {
    land_data$crosscut <- input$crosscut_slider
  })
  
  # Store crosscut data
  observeEvent(input$finalize_crosscut_button, {
    # Use lapply to place crosscut data frame in a list so it can be assigned to a column
    land_data$df$ccdata <- lapply(land_data$df$x3p, function(x) x3p_crosscut(x, y = land_data$crosscut))
  })
  
  # Display crosscut data after finalized ----
  output$final_crosscut_df <- renderTable({
    req("ccdata" %in% colnames(land_data$df))
    land_data$df$ccdata[[1]]
  })

  # Display crosscut slider ----
  output$crosscutUI <- renderUI({
    req(land_data$crosscut)
    
    # Store land dimensions
    land_data$x3p_dims <- dim(land_data$df$x3p[[1]])
    
    # renderUI requires tagList to render multiple inputs
    tagList(
      br(),
      sliderInput(
        inputId = "crosscut_slider", 
        label = "Crosscut location",
        min = 0, 
        max = land_data$x3p_dims[2], 
        value = land_data$crosscut
      ),
      actionButton(inputId = "finalize_crosscut_button", label = "Finalize crosscut")
    )
  })
  
  # Display crosscut table in card ----
  output$crosscut_table_display <- renderUI({
    req(!is.null(land_data$df) & "ccdata" %in% colnames(land_data$df))
    make_table_card(table_id = "final_crosscut_df", header_title = "Crosscut Data")
  })
  
}