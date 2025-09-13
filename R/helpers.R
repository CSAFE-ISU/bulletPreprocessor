cleanup_temp_directory <- function(temp_dir, force = FALSE) {

  if (dir.exists(temp_dir)) {
    # Get all files in temp directory
    files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
    
    # Try to remove all files first
    try({
      file.remove(files)
    }, silent = TRUE)
    
    # Then remove the directory
    try({
      unlink(temp_dir, recursive = TRUE, force = force)
    }, silent = TRUE)
    
    # Wait a moment and try again if directory still exists
    if (dir.exists(temp_dir)) {
      Sys.sleep(0.1)
      try({
        unlink(temp_dir, recursive = TRUE, force = TRUE)
      }, silent = TRUE)
    }
  }
}

extract_houston_pattern <- function(filename) {
  # Houston naming patterns
  patterns <- list(
    full = "Kit C[A-Z] - K[A-Z] Bullet \\d+ Land [1-6]",
    no_kit = "Kit K[A-Z] - Bullet \\d+ Land [1-6]",
    no_kit_two_dashes = "Kit K[A-Z] - Bullet \\d+ - Land [1-6]",
    no_barrel = "Kit C[A-Z] - U\\d+ - Land [1-6]"
  )
  
  if (str_detect(filename, patterns$full)) {
    return(str_extract(filename, patterns$full))
  } else if (str_detect(filename, patterns$no_kit)) {
    return(str_extract(filename, patterns$no_kit))
  } else if (str_detect(filename, patterns$no_kit_two_dashes)) {
    return(str_extract(filename, patterns$no_kit_two_dashes))
  } else if (str_detect(filename, patterns$no_barrel)) {
    return(str_extract(filename, patterns$no_barrel))
  } else {
    stop("Unknown_Pattern")
  }
}

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

get_bullet_name <- function(filename, study) {
  # Get bullet name based on bullet study
  if (stringr::str_detect(tolower(study), "houston")) {
    bullet <- get_houston_bullet_name(filename)
  } else {
    stop("Bullet name not defined for study yet")
  }
  
  # Add "Bullet" to beginning of name if needed
  bullet <- ifelse(str_detect(bullet, "^Bullet"), bullet, paste("Bullet", bullet))
  
  return(bullet)
}

get_houston_barrel_name <- function(filename) {
  pattern <- extract_houston_pattern(filename = filename)
  
  if (str_detect(pattern, "K[A-Z]")) {
    return(str_extract(pattern, "K[A-Z]"))
  } else if (str_detect(pattern, "U\\d+")) {
    return(str_extract(pattern, "U\\d+"))
  } else {
    stop("Barrel not found in pattern")
  }
}

get_houston_bullet_name <- function(filename) {
  pattern <- extract_houston_pattern(filename = filename)
  
  if (str_detect(pattern, "Bullet \\d+")) {
    return(str_extract(pattern, "Bullet \\d+"))
  } else if (str_detect(pattern, "U\\d+")) {
    return(str_extract(pattern, "U\\d+"))
  } else {
    stop("Bullet not found in pattern")
  }
}

get_land_name <- function(filename) {
  return(stringr::str_extract(filename, "Land \\d+"))
}

# Make data frame with same columns as those created with GitHub repo
# bulletxtrctr_replicate_results
make_output_df <- function(land_rv, drop_x3p = TRUE) {
  
  # NOTE: Make new data frame for output so that if drop_x3p is TRUE, the scan
  # will still preview on the Land with Crosscut and Grooves tab
  df <- land_rv$df
  df$study <- land_rv$study
  df$folder <- NA  # Folder is tempdir() so no reason to save it
  df$barrel <- land_rv$barrel
  df$bullet <- land_rv$bullet
  df$land <- land_rv$land
  df$source <- NA  # Source is a file in tempdir() so no reason to save it
  df$resolution <- land_rv$resolution
  df$crosscut <- land_rv$crosscut
  df$ccdata <- list(land_rv$ccdata)
  df$grooves <- land_rv$grooves
  df$sigs <- list(land_rv$sigs)
  
  if (drop_x3p) {
    # Drop x3p data frame to make file smaller
    df <- df %>% 
      select(all_of(c("study", "folder", "barrel", "bullet", "land", "source", 
                      "resolution", "crosscut", "ccdata", "grooves", "sigs")))
  } else {
    df <- df %>% 
      select(all_of(c("x3p", "study", "folder", "barrel", "bullet", "land", "source", 
                      "resolution", "crosscut", "ccdata", "grooves", "sigs")))
  }
  
  land_rv$output_df <- df

}

plot_grooves <- function(ccdata, left_groove, right_groove) {
  ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
    geom_vline(xintercept = left_groove, color = app_config$display_params$groove_left_color) +
    geom_vline(xintercept = right_groove, color = app_config$display_params$groove_right_color) +
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

reset_land_data <- function(land_rv) {
  land_rv$barrel <- NULL
  land_rv$bullet <- NULL
  land_rv$ccdata <- NULL
  land_rv$crosscut <- NULL
  land_rv$df <- NULL
  land_rv$grooves <- NULL
  land_rv$land <- NULL
  land_rv$left_scan <- NULL
  land_rv$output_df <- NULL
  land_rv$resolution <- NULL
  land_rv$right_scan <- NULL
  land_rv$sigs <- NULL
  land_rv$study <- NULL
  land_rv$upload_confirmed <- NULL
  land_rv$x3p_dims <- NULL
  
  # Force garbage collection
  gc(verbose = FALSE)
}
