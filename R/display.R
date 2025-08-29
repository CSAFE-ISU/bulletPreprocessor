displayPlotCardUI <- function(id) {
  ns <- NS(id)
  tagList(
   uiOutput(ns("plot_card"))
  )
}

displayPlotCardServer <- function(id, plot_reactive = NULL, header_title = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      req(plot_reactive())
      plot_reactive()
    })
    
    # Render the card with the plot
    output$plot_card <- renderUI({
      req(plot_reactive())
      
      # Check if reactive first, then make sure header title is truthy to avoid
      # problems with header_title is NULL and reactive
      if (is.reactive(header_title)) {
        header_title <- header_title()
      }
      req(header_title)
      
      card(
        card_header(
          class = app_config$display_params$card_header_class,
          header_title
        ),
        full_screen = app_config$display_params$card_full_screen,
        plotOutput(session$ns("plot"))
      )
    })
    
  })
}

displayPrintUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("value"))
  )
}

displayPrintServer <- function(id, value = NULL, label = NULL) {
  moduleServer(id, function(input, output, session) {
      output$value <- renderPrint({
        req(value)
        if (!is.null(label)) {
          paste(label, value)
        } else {
          value
        }
      })
  })
}
