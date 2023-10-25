test_that("adjacency creates a data frame", {
  test_corpus <- readRDS(test_path("fixtures", "testset.rds"))$text_corpus
  result <- summarise_adjacency(test_corpus)
  expect_named(result, c("feature", "frequency", "Skip-0", "Skip-1", "Skip-2"))
  expect_true(nrow(result) > 0 )
})
