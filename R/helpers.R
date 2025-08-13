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

groove_plot <- function(ccdata, grooves) {
  ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
    #    theme_bw()+
    geom_vline(xintercept = 0, colour = "grey50") + 
    geom_vline(xintercept = grooves[2]-grooves[1], colour = "grey50") +
    geom_line(linewidth = .5) + # put signal in front
    annotate("rect", fill="grey50", alpha = 0.15, xmax = 0, xmin = -Inf, ymin = -Inf, ymax = Inf) +
    annotate("rect", fill="grey50", alpha = 0.15, xmax = Inf, xmin = grooves[2]-grooves[1], ymin = -Inf, ymax = Inf) +
    geom_line(linewidth = 1, data = filter(ccdata, between(x, 0, grooves[2]-grooves[1]))) + # put signal in front
    scale_x_continuous(
      breaks=c(0,round(as.numeric(grooves[2]-grooves[1]),0),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2)),
      labels=c("\n0",paste0("\n",round(as.numeric(grooves[2]-grooves[1]),0)),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2))
    ) +
    xlab("Position along width of Land [µm]") +
    ylab("Surface Height [µm]") 
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

make_plot_card <- function(plot_id, header_title) {
  card(
    card_header(class = "bg-dark", header_title),
    full_screen = TRUE,
    plotOutput(plot_id),
  )
}