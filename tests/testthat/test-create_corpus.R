test_that("Corpus can be created from imported references with create_corpus()", {
  corpus <- create_corpus(readRDS(test_path("fixtures","testset_ref.RDS")))
  expect_identical(class(corpus), c("corpus", "character"))
  expect_length(corpus, 34)
})

#test_that("corpus function produces the same result as in older versions", {
#  original <- as.data.frame(readRDS(test_path('fixtures', 'corpus.rds')))
#  test <- as.data.frame(searchbuildR:::create_corpus(readRDS(test_path('fixtures', 'testset_ref.rds'))))
#  expect_equal()
#})
