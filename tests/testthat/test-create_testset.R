# test formal correctness of testset object
test_that("create_testset(), creates as named list of objects", {
  testset <- create_testset(test_path("fixtures", "testset_ris.txt"))
  expect_named(testset, c("freetext","MeSH.Terms", "PMIDS", "reference.list", "text_corpus"))
})
test_that("create_testset() returns the correct development set and validation set in raw format",{
  testset <- create_testset(test_path("fixtures", "testset_ris.txt"), dev_set = TRUE)
  dev <- testset$development_set
  pmids_pattern <- paste(testset$PMIDS$development_set, collapse = "|")
  test <- grep(pmids_pattern, dev)
  expect_identical(seq_along(dev), test)
})
