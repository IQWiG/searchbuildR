test_that("mesh_esummary returns results for existing MeSH UIDs", {
result <- mesh_esummary(c("68005891","68001943"))
expect_equal(length(result$result),3)
})

test_that("mesh_esummary returns error message for non-existing MeSH UIDs", {
  expect_error(mesh_esummary("40000000000"), "At least one MeSH identifier does not exist")
})

test_that("mesh_esearch retrieves only exact matches", {
  result <- mesh_esearch(c("Breast Neoplasms", "Gingivitis"))
  expect_equal(result, c("68005891","68001943"))
})


