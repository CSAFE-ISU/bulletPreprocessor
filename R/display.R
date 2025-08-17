displayPlotCardUI <- function(id) {
  tagList(
   uiOutput(NS(id, "plot_card"))
  )
}

displayPlotCardServer <- function(id, plot_reactive = NULL, header_title = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # req(plot_reactive)
      plot_reactive()
    })
    
    # Render the card with the plot
    output$plot_card <- renderUI({
      card(
        card_header(class = "bg-dark", header_title),
        full_screen = TRUE,
        plotOutput(session$ns("plot"))
      )
    })
    
  })
}

displayPrintUI <- function(id) {
  tagList(
    verbatimTextOutput(NS(id, "value"))
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
