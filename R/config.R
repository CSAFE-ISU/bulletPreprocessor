app_config <- list(
  file_params = list(
    max_file_size = 150 * 1024^2,  # 150 MB
    allowed_extensions = c(".x3p"),
    temp_dir = file.path(tempdir(), "land")
  ),
  
  display_params = list(
    card_full_screen = TRUE,
    card_header_class = "bg-dark",
    crosscut_color = "#eeeeee",
    crosscut_size = 20,
    groove_left_color = "turquoise",
    groove_right_color = "greenyellow",
    groove_size = 20,
    notification_duration = 5,
    rgl_popups_null = TRUE,  # TRUE prevents popups. FALSE allows popups
    scan_size = 200,
    scan_zoom = 1,
    scan_sample_rate = 5,  # for x3p_sample(m=5)
    signal_color = "grey30",
    signal_raw_color = "grey70" 
  ),
  
  # Default processing parameters
  proc_params = list(
    crosscut_ylimits = c(150, NA),
    grooves_method = "middle",
    grooves_adjust = 30,
    signal_span1 = 0.75,
    signal_span2 = 0.03
  ),
  
  # UI configuration
  ui_params = list(
    studies = c("Hamby 44", "Houston Group 1", "Houston Group 2", "Houston Group 3", "Phoenix"),
    default_study = "Houston Group 1",
    slider_step_size = 1,
    slider_large_step = 25
  )
  
)
