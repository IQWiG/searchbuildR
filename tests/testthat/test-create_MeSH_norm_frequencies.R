test_that("MeSH norm frequencies were created successfully", {
  reference_set <- (readRDS(test_path("fixtures","testset_ref.RDS")))
  expect_message(create_MeSH_norm_frequencies(reference_set), "New norms have been calculated.")
})
