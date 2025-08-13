groovesUI <- function(id) {
  tagList(
    actionButton(NS(id, "default_grooves_button"), "Get default grooves")
  )
}

groovesTabUI <- function(id) {
  tagList(
    verbatimTextOutput(NS(id, "grooves_verbatim"))
  )
}

groovesServer <- function(id, land_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Get default grooves ----
    observeEvent(input$default_grooves_button, {
      # Use same argument values as bulletAnalyzr
      land_rv$grooves <- cc_locate_grooves(
        land_rv$df$ccdata[[1]], 
        method = "middle", 
        adjust = 30, 
        return_plot = FALSE
      )
      land_rv$left_groove <- land_rv$grooves[[1]][1]
      land_rv$right_groove <- land_rv$grooves[[1]][2]
    })
    
    # Display left groove value ----
    output$grooves_verbatim <- renderPrint({
      req(land_rv$grooves)
      land_rv$left_groove
    })
    
  })
}