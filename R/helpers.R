get_barrel_name <- function(filename, study) {
  if (study == "houston") {
    return(get_houston_barrel_name(filename))
  }
}

get_bullet_name <- function(filename) {
  return(stringr::str_extract(filename, "Bullet \\d+"))
}

get_houston_barrel_name <- function(filename) {
  return(stringr::str_extract(filename, "\\b[A-Z]+(?=\\s+Bullet)"))
}

get_land_name <- function(filename) {
  return(stringr::str_extract(filename, "Land \\d+"))
}

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

make_plot_card <- function(plot_id, header_title) {
  card(
    card_header(class = "bg-dark", header_title),
    full_screen = TRUE,
    plotOutput(plot_id),
  )
}

make_table_card <- function(table_id, header_title) {
  card(
    card_header(class = "bg-dark", header_title),
    full_screen = TRUE,
    tableOutput(table_id),
  )
}

plot_grooves <- function(ccdata, left_groove, right_groove) {
  ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
    geom_vline(xintercept = left_groove, color = "orange") +
    geom_vline(xintercept = right_groove, color = "orange") +
    geom_line(linewidth = .5) +
    xlim(min(ccdata$x, na.rm = TRUE), max(ccdata$x, na.rm = TRUE)) +
    xlab("Position along width of Land [µm]") +
    ylab("Surface Height [µm]") +
    theme_bw()
}

plot_signal <- function(signal_df) {
  signal_df %>% 
    filter(!is.na(sig),!is.na(raw_sig)) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = raw_sig), colour = "grey70") +
    geom_line(aes(y = sig), colour = "grey30") +
    ylim(c(-5,5)) +
    theme_bw() +
    labs(y = "value")
}
