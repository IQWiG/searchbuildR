test_that("population set was imported successfully", {
  path <- test_path(pattern = "fixtures", "testset_ris.txt")
  expect_silent(read_bibliography(path, return_df = F))
})

test_that("references can be imported in pubmed format", {
  path <- test_path(pattern = "fixtures", "testset_pubmed.txt")
  expect_silent(read_bibliography(path, return_df = F))
})

test_that("ris import is the same result as in older versions", {
  original <- readRDS(test_path("fixtures", "testset_ref.rds"))
  test <- read_bibliography(test_path("fixtures", "testset_ref.txt"), return_df = FALSE)
  expect_equal(original, test)
})
