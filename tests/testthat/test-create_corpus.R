test_that("Corpus can be created from imported references with create_corpus()", {
  corpus <- create_corpus(readRDS(test_path("fixtures","testset_ref.RDS")))
  expect_identical(class(corpus), c("corpus", "character"))
  expect_length(corpus, 34)
})
