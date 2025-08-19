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

change_slider_value <- function(current_value, step, max_value) {
  
  proposed_value <- current_value + step
  
  if (step < 0) {
    # Don't allow value below min
    new_val <- max(0, proposed_value)  
  } else {
    # Don't allow value above max
    new_val <- min(max_value, proposed_value) 
  }
  
  return(new_val)
}

# Make data frame with same columns as those created with GitHub repo
# bulletxtrctr_replicate_results
make_output_df <- function(land_rv, drop_x3p = TRUE) {
  land_rv$df$study <- land_rv$study
  land_rv$df$folder <- NA  # Folder is tempdir() so no reason to save it
  land_rv$df$barrel <- land_rv$barrel
  land_rv$df$bullet <- land_rv$bullet
  land_rv$df$land <- land_rv$land
  land_rv$df$source <- NA  # Source is a file in tempdir() so no reason to save it
  land_rv$df$resolution <- land_rv$resolution
  land_rv$df$crosscut <- land_rv$crosscut
  land_rv$df$ccdata <- list(land_rv$ccdata)
  land_rv$df$grooves <- NA
  land_rv$df$grooves <- list("groove" = c(land_rv$left_groove, land_rv$right_groove))
  land_rv$df$sigs <- list(land_rv$sigs)
  
  if (drop_x3p) {
    # Drop x3p data frame to make file smaller
    land_rv$df <- land_rv$df %>% 
      select(all_of(c("study", "folder", "barrel", "bullet", "land", "source", 
                      "resolution", "crosscut", "ccdata", "grooves", "sigs")))
  } else {
    land_rv$df <- land_rv$df %>% 
      select(all_of(c("x3p", "study", "folder", "barrel", "bullet", "land", "source", 
                      "resolution", "crosscut", "ccdata", "grooves", "sigs")))
  }

  return()
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
    theme_bw() +
    labs(y = "value")
}
