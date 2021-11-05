# tabulate

test_that(desc = "tabulate() works", code = {

  stdev_per_species <- c(1.76529823325947, 0.762237668960347, 0.828066127977863, 0.435866284936698)
  stdev_per_species_res <- tabulate::tabulate(iris, cols = where(is.numeric), return_mean = TRUE) %>% pull(stdev)
  expect_equal(stdev_per_species_res, stdev_per_species)

  stdev_per_species <- c(0.33333333, 0.33333333, 0.33333333)
  stdev_per_species_res <- tabulate::tabulate(iris, cols = Species, return_mean = FALSE) %>% pull(pct)
  expect_equal(stdev_per_species_res, stdev_per_species)

})

