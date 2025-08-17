get_barrel_name <- function(filename, study) {
  
  # Get barrel name based on bullet study
  if (stringr::str_detect(tolower(study), "houston")) {
    barrel <- get_houston_barrel_name(filename)
  } else {
    stop("Barrel name not defined for study yet")
  }
  
  # Add "Barrel" to beginning of name if needed
  barrel <- ifelse(str_detect(barrel, "^Barrel"), barrel, paste("Barrel", barrel))
  
  return(barrel)
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

plot_grooves <- function(ccdata, left_groove, right_groove) {
  ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
    geom_vline(xintercept = left_groove, color = app_config$display_params$groove_color) +
    geom_vline(xintercept = right_groove, color = app_config$display_params$groove_color) +
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
    geom_line(aes(y = raw_sig), colour = app_config$display_params$signal_color) +
    geom_line(aes(y = sig), colour = app_config$display_params$signal_raw_color) +
    ylim(c(-5,5)) +
    theme_bw() +
    labs(y = "value")
}
