# test formal correctness of population set object
test_that("population set was created successfully", {
  path <- testthat::test_path(pattern = "fixtures", "testset_ris.txt")
  expect_silent(create_popset(path))
})
