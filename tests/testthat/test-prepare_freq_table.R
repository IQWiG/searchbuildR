test_that("Frequency table is prepared correctly with prepare_freq_table()", {
  freq_table <- prepare_freq_table(readRDS(test_path("fixtures","corpus.rds")))
  expect_length(freq_table, 7)
  expect_true(nrow(freq_table) > 0)
})
