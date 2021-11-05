# tabulate

test_that(desc = "tabulate() works", code = {

  test <- iris
  stdev_per_species <- c(1.76529823325947, 0.762237668960347, 0.828066127977863, 0.435866284936698)
  stdev_per_species_res <- tabulate::tabulate(iris, cols = where(is.numeric), return_mean = TRUE) %>% pull(stdev)
  expect_equal(stdev_per_species_res, stdev_per_species)

  stdev_per_species <- c(0.33333333, 0.33333333, 0.33333333)
  stdev_per_species_res <- tabulate::tabulate(iris, cols = Species, return_mean = FALSE) %>% pull(pct)
  expect_equal(stdev_per_species_res, stdev_per_species)

  levels(test$Species) <- c("firs_extra", levels(test$Species))
  stdev_per_species_res <- tabulate::tabulate(test, cols = Species, keep_empty_levels = TRUE) %>% pull(value)
  expect_equal(first(stdev_per_species_res), "firs_extra")

  stdev_per_species_res <- tabulate::tabulate(iris, cols = where(is.numeric), return_mean = TRUE, variable_sep = "\\.") %>% pull(variable) %>% unique()
  expect_equal(stdev_per_species_res, c("Petal","Sepal"))

  test$Species[c(1,51)] <- NA
  stdev_per_species_res <- tabulate::tabulate(test, cols = Species, values_drop_na = FALSE) %>%
    filter(is.na(value)) %>% pull(n)
  expect_equal(stdev_per_species_res, 2)

  })

