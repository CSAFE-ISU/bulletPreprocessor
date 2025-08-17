downloadUI <- function(id) {
  tagList(
    downloadButton(NS(id, "download_button"), "Download")
  )
}

downloadServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Disable button when app starts ----
    disable("download_button")
    
    # Switch the button on or off ----
    observe({
      if (buttons_rv$download) {
        enable("download_button")
      } else {
        disable("download_button")
      }
    })
    
    output$download_button <- downloadHandler(
      filename = function() {
        paste(land_rv$study, land_rv$barrel, land_rv$bullet, land_rv$land, "signal.rds", sep="-")
      },
      content = function(file) {
        saveRDS(land_rv$df, file)
      }
    )
    
  })
}
