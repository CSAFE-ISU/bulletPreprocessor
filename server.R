
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

# Server ----
server <- function(input, output) {

  # Reactive object to land data ----
  land <- reactiveValues(
    name = NULL,
    bullet_name = NULL,
    barrel_name = NULL
  )
  
  observeEvent(input$land_upload, {
    land$barrel_name <- get_barrel_name(input$land_upload$name, study = "houston")
    land$bullet_name <- get_bullet_name(input$land_upload$name)
    land$name <- get_land_name(input$land_upload$name)
  })
  
  output$barrel_name <- renderText({
    req(land$barrel_name)
    land$barrel_name
  })
  
  
  output$bullet_name <- renderText({
    req(land$bullet_name) 
    land$bullet_name
  })
  
  output$land_name <- renderText({
    req(land$name) 
    land$name
  })
  

  
  
  ## Push current bullet data to all bullet data object
  observeEvent(input$up_bull,{
    if(nrow(bulldata$cbull)==0) return(NULL)
    allbull <- bulldata$allbull
    allbull <- allbull[!(allbull$bullet %in% input$bul_x3p_name),]
    bull <- bulldata$cbull
    bull$bullet <- input$bul_x3p_name
    #bull$land <- 1:nrow(bull)
    bull$land <- factor(bull$land_names, levels = bull$land_names)
    bulldata$allbull <- rbind(allbull,bull)
    disable("up_bull")
  })
  
  observeEvent(input$bul_x3p, {
    values$show_alert <- TRUE
  })
  
  uploaded_bull <- reactive({
    temp_refresh <- input$prevreport
    
    # Create Temporary Directory and save bullets in it
    temp_dir <- tempfile()
    dir.create(temp_dir)
    file.copy(input$bul_x3p$datapath, paste0(temp_dir, "/", input$bul_x3p$name))
    
    return(read_bullet(temp_dir))
  })
  
  output$lpupload <- renderUI({
    if(is.null(input$bul_x3p)) return(NULL)
    disable("up_bull")
    progress <- shiny::Progress$new();on.exit(progress$close())
    
    ## Read Bullet
    progress$set(message = "Reading Bullets", value = .25)
    bull <- uploaded_bull()
    
    # Check if we need to rotate the bullet
    hinfo <- bull$x3p[[1]]$header.info
    if (hinfo$sizeX < hinfo$sizeY) {
      if (values$show_alert) {
        showModal(modalDialog(
          title = "Rotated Bullet",
          "Detected rotated bullet, rotating 90 degrees...",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      values$show_alert <- FALSE
      bull$x3p <- lapply(bull$x3p, x3p_rotate, angle = 90)
    }
    
    # Check if we need to down-sample the bullet
    # Calculate the closest integer `n` that samples reference resolution to match incrementX
    if (nrow(bulldata$allbull) > 0) {
      reference_resolution <- x3p_get_scale(bulldata$allbull$x3p[[1]]) / 1e6
      current_resolution <- x3p_get_scale(bull$x3p[[1]])
      
      # Down-sample if necessary
      if (reference_resolution > current_resolution) {
        if (values$show_alert) {
          showModal(modalDialog(
            title = "Higher Resolution Bullet",
            "Detected higher resolution bullet, down-sampling...",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
        values$show_alert <- FALSE
        m <- round(reference_resolution / current_resolution)
        
        bull$x3p <- lapply(bull$x3p, x3p_sample, m = m)
      } else if (reference_resolution < current_resolution) {
        if (values$show_alert) {
          showModal(modalDialog(
            title = "Lower Resolution Bullet",
            "Detected lower resolution bullet, down-sampling previous bullets...",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
        values$show_alert <- FALSE
        
        m <- round(current_resolution / reference_resolution)
        bulldata$allbull$x3p <- lapply(bulldata$allbull$x3p, x3p_sample, m = m)
      }
    }
    
    cond_x3p_m_to_mum <- function(x3p)
    {
      scale <- x3p %>% x3p_get_scale()
      if (scale < .1) x3p <-  x3p %>% x3p_m_to_mum() # only scale conditionally
      x3p
    }
    bull$x3p <- lapply(bull$x3p,cond_x3p_m_to_mum)
    bull$md5sum <- tools::md5sum(bull$source)
    bull$filename <- basename(bull$source)
    bull$land_names <- identify_lands(bull$filename)
    bull$bullet_name <- identify_bullet(bull$filename)
    bulldata$cbull <- bull
    
    ## Render Bullet
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bull)) 
    {
      local({
        cidx <- idx
        output[[paste0("x3prgl",idx)]] <- renderRglwidget({
          x3p_image(x3p_sample(bull$x3p[[cidx]],m=5) %>% x3p_rotate(),size=500,zoom=.4)
          rglwidget()
        })
      })
    }
    
    ## Enable Upload Button
    enable("up_bull")
    
    ## UI
    layout_column_wrap(
      width = 1/6,
      !!!lapply(1:nrow(bull), FUN= function(x) parse_rglui(x, name = "x3prgl", land_name = bull$land_names[x]))
    )
  })

  
}