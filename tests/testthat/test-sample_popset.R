test_that("plotting works", {
  seed <- 123456788
  years <- with_seed(seed, floor(rnorm(20000, mean = 2020, sd = 20)))
  years <- paste0("DP  - ", years[years <=2024])
  counter <- length(years)
  plot_data(years, counter, seed)
})
