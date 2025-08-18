library(bulletxtrctr)
library(x3ptools)

# Save a lower resolution version of an x3p file to save space
bullet <- read_bullet("/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/houston_group1/Barrel KA/Bullet 1/Preprocessed")
land_x3p <- bullet$x3p[[1]] %>% 
  x3p_sample(m=2) %>%
  x3p_write(file = "data-raw/KA Bullet 1 Land 1 downsampled.x3p")
