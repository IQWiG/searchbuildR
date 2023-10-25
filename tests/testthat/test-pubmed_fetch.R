test_that("efetch returns info if no results are retrieved", {
  expect_equal(efetch(80000000), "The request retrieved no results.")
})

test_that("efetch returns a non-empty character vector", {
  expect_vector(efetch(4), ptype= character(), size = 61)
})

test_that("sample_uids returns a list with the result of length 'size' and the highest UID below or equal to 'maxUID'",{
  size <- 3
  maxUID <- 5000
  result <- sample_uids(size = size, maxUID = maxUID)
 expect_length(result$random_result, size)
 expect_true (max(result$random_result) <= maxUID)
})

test_that("clean_efetch_result correctly identifies falsely MeSH terms from efetch results", {
  request <- readRDS(test_path("fixtures", "efetch217.RDS"))
  result <- clean_efetch_result(request)
  expect_true(length(request) - length(result) == 2)
})
