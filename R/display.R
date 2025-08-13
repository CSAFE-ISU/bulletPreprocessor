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
