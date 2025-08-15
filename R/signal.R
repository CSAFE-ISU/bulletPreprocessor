signalUI <- function(id) {
  tagList(
    selectInput(
      NS(id, "study"), 
      label = "Choose bullet study", 
      choices = c("Hamby 44", "Houston Group 1", "Houston Group 2", "Houston Group 3", "Phoenix"), 
      selected = "Houston Group 1"),
    actionButton(NS(id, "signal_button"), "Get signal")
  )
}

signalTabUI <- function(id) {
  tagList(
    # DT::DTOutput(NS(id, "signal_df"))
    plotOutput(NS(id, "signal_plot"))
  )
}

signalServer <- function(id, land_rv, buttons_rv, main_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Disable button when app starts ----
    disable("signal_button")
    
    # Switch the button on or off ----
    observe({
      if (buttons_rv$signal) {
        enable("signal_button")
      } else {
        disable("signal_button")
      }
    })
    
    # Get signal in data frame ----
    observeEvent(input$signal_button, {
      
      # Construct data frame to match format used in bulletxtrctr_replicate_results package ----
      df <- data.frame(
        study = input$study,
        folder = NA,
        barrel = ifelse(str_detect(land_rv$barrel_name, "^Barrel"), land_rv$barrel_name, paste("Barrel", land_rv$barrel_name)),
        bullet = land_rv$bullet_name,
        land = land_rv$land_name,
        source = NA
      )
      df$resolution <- sapply(land_rv$df$x3p, function(x) x3ptools::x3p_get_scale(x))
      df$crosscut <- land_rv$crosscut
      df$ccdata <- land_rv$crosscut_df
      df$grooves <- list(groove = land_rv$grooves)  # this format is required by cc_get_signature()
      
      # Get signal ----
      df <- df %>% dplyr::mutate(
        sigs = purrr::map2(
          .x = ccdata,
          .y = grooves,
          .f = function(x, y) {
            bulletxtrctr::cc_get_signature(
              ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
          })
      )

      land_rv$df <- df
      
      # Switch to signal tab after extracting signal ----
      if (!is.null(main_session)) {
        nav_select(session = main_session, "main_tabs", selected = "Signal")
      }
    })
    
    output$signal_plot <- renderPlot({
      req(land_rv$df$sigs)
      plot_signal(land_rv$df$sigs[[1]])
    })
    
    # output$signal_df <- DT::renderDT({
    #   req(land_rv$df$sigs)
    #   land_rv$df$sigs[[1]]
    # })
    
  })
}
