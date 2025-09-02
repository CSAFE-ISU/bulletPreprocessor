landScanUI <- function(id) {
  ns <- NS(id)
  tagList(
    crosscutGroovesUI(ns("crosscut_grooves1"))
  )
}

landScanTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("land_display"))
  )
}

landScanServer <- function(id, land_rv, buttons_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Display land in card ----
    output$land_display <- renderUI({
      req(land_rv$upload_confirmed)
      validate(
        need(land_rv$df, "Bullet data frame cannot be NA"),
        need(land_rv$barrel, 'Barrel name cannot be NA'),
        need(land_rv$bullet, 'Bullet name cannot be NA'),
        need(land_rv$land, 'Land name cannot be NA')
      )
      
      tagList(
        layout_column_wrap(
          crosscutGroovesTabUI(session$ns("crosscut_grooves1")),
          profileTabUI(session$ns("profile1"))
        )
      )

    })
    
    crosscutGroovesServer("crosscut_grooves1", land_rv, buttons_rv)
    profileServer("profile1", land_rv, buttons_rv)
    
  })
}
