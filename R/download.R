downloadUI <- function(id) {
  tagList(
    downloadButton(NS(id, "download_button"), "Download")
  )
}

downloadServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
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
