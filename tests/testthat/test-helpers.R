testthat::test_that("extract_houston_pattern for full naming convention", {
  filename <- "HTX - Group 1 - Kit CU - KE Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.x3p"
  actual <- extract_houston_pattern(filename = filename)
  
  testthat::expect_identical(actual, "Kit CU - KE Bullet 1 Land 1")
  
})

testthat::test_that("extract_houston_pattern works when kit is missing", {
  filename <- "HTX - Group 1 - Kit KD - Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.x3p"
  actual <- extract_houston_pattern(filename = filename)
  
  testthat::expect_identical(actual, "Kit KD - Bullet 1 Land 1")
  
})

testthat::test_that("extract_houston_pattern works when barrel is missing", {
  filename <- "HTX - Group 1 - Kit CE - U10 - Land 1 - Sneox1 - 20x - auto light left image +20 perc. x10 - threshold 2 - resolution 4 - Connor Hergenreter.x3p"
  actual <- extract_houston_pattern(filename = filename)
  
  testthat::expect_identical(actual, "Kit CE - U10 - Land 1")
  
})

testthat::test_that("get_barrel_name works on known barrel from Houston study", {
  filename <- "HTX - Group 1 - Kit CU - KE Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.x3p"
  actual <- get_barrel_name(filename = filename, study = "houston")
  
  expect_identical(actual, "Barrel KE")
})

testthat::test_that("get_barrel_name works on unknown bullet from Houston study", {
  # NOTE: For unknown bullets from the Houston study, the barrel is given the same
  # name as the bullet
  filename <- "HTX - Group 1 - Kit CE - U10 - Land 1 - Sneox1 - 20x - auto light left image +20 perc. x10 - threshold 2 - resolution 4 - Connor Hergenreter.x3p"
  actual <- get_barrel_name(filename = filename, study = "houston")
  
  expect_identical(actual, "Barrel U10")
})

testthat::test_that("get_bullet_name works on known bullet from Houston study", {
  filename <- "HTX - Group 1 - Kit CU - KE Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.x3p"
  actual <- get_bullet_name(filename = filename, study = "houston")
  
  expect_identical(actual, "Bullet 1")
})

testthat::test_that("get_bullet_name works on unknown bullet from Houston study", {
  # NOTE: For unknown bullets from the Houston study, the barrel is given the same
  # name as the bullet
  filename <- "HTX - Group 1 - Kit CE - U10 - Land 1 - Sneox1 - 20x - auto light left image +20 perc. x10 - threshold 2 - resolution 4 - Connor Hergenreter.x3p"
  actual <- get_bullet_name(filename = filename, study = "houston")
  
  expect_identical(actual, "Bullet U10")
})

testthat::test_that("get_land_name works on known bullet from Houston study", {
  filename <- "HTX - Group 1 - Kit CU - KE Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.x3p"
  actual <- get_land_name(filename = filename)
  
  expect_identical(actual, "Land 1")
})

testthat::test_that("get_land_name works on unknown bullet from Houston study", {
  filename <- "HTX - Group 1 - Kit CE - U10 - Land 1 - Sneox1 - 20x - auto light left image +20 perc. x10 - threshold 2 - resolution 4 - Connor Hergenreter.x3p"
  actual <- get_land_name(filename = filename)
  
  expect_identical(actual, "Land 1")
})